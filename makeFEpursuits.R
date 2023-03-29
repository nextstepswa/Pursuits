#
# Create clean pursuit dataframes from merged FE-WaPo WA2015 data
#

library(tidyverse)

# Key dates

reform.date = as.Date("2021-07-25")
start.date = as.Date("2015-01-01")

# Read in merged FE-WaPo data
# This file is created by fewapo/DataCleaningScripts/ScrapeMerge.R
# You can reconstruct it by forking https://github.com/nextstepswa/fewapo

## Initial pursuit cleaning and coding is done in ScrapeMerge.R and pursuit_coding.R
## creates: vpursuit, victim, injury, incident.num, pursuit.type and wtsc flag variables

load(file = "~/GitHub/fewapo/data-outputs/WA2015.rda")

min.yr <- min(merged_data$year)
max.yr <- max(merged_data$year)

# All cases: ----

## Fatalities
## Remove people not killed by police unless the incident is pursuit-related.
## incident.type identifies pursuit and after pursuit cases from vpursuit
## incident.num is stable for multi-fatality incidents (coded manually), but
##  may not be stable for all other cases

fatalities.all <- merged_data %>% 
  filter(!(not.kbp==1 & pursuit.type == "Reviewed not related")) %>%
  mutate(
    incident.type = factor(
      case_when(
        grepl("Active|Terminated", vpursuit) ~ "Pursuit vehicular fatality",
        grepl("Involved", vpursuit) & suicide==1 ~ "After pursuit suicide",
        grepl("Involved", vpursuit) ~ "After pursuit homicide",
        vpursuit == "Attempted stop" ~ "Attempted stop fatality",
        grepl("accident", vpursuit) ~ "Vehicle accident fatality",
        TRUE ~ "All other fatalities"),
      levels = c("After pursuit homicide", "Pursuit vehicular fatality", 
                 "After pursuit suicide", "Attempted stop fatality", 
                 "Vehicle accident fatality", "All other fatalities")),
    
    traffic = if_else(grepl('traffic', description), "yes", "no"),
    cod3 = case_when(cod == "Vehicle" ~ "Vehicle",
                     cod == "Gunshot" ~ "Gunshot",
                     TRUE ~ "Other"),
    cod3 = forcats::fct_relevel(cod3, "Other", after=Inf),
    postreform = ifelse(date > reform.date, 1, 0),
    agency.type = ifelse(grepl("State", agency.type), "State Patrol", agency.type)
  ) %>%
  arrange(desc(incident.type), date) %>%
  mutate(
    incident.num = if_else(
      !is.na(incident.num), incident.num, # multiple fatalities, same incident
      100 + as.numeric(rownames(.))),
    victim = factor(victim,
                    levels = c("Officer", "Bystander", "Passenger", "Subject")))

## Incidents

incidents.all <- fatalities.all %>%
  group_by(incident.num) %>%
  summarize(
    date = first(date),
    year = first(year),
    month = first(month),
    day = first(day),
    leg.year = first(leg.year),
    vpursuit = first(vpursuit),
    incident.type = first(incident.type),
    lat = first(latitude),
    long = first(longitude),
    agency = first(agency),
    agency.type = first(agency.type),
    city = first(city),
    county = first(county),
    url_info = first(url_info),
    url_click = first(url_click),
    fatalities = n(),
    inci.mapmult = ifelse(fatalities==1, "single", "mult"),
    
    # For now, the rest are for vehicle-related incidents only
    # So are NA for "not related" cases
    
    subjects.killed = ifelse(grepl("not related", vpursuit), NA_real_, 
                             sum(grepl("Subject", victim))), # one IP case also had officer shot
    passengers.killed = sum(victim=="Passenger"),
    bystanders.killed = sum(victim=="Bystander"),
    officers.killed = sum(victim=="Officer"),
    subjects.injured = ifelse(grepl("not related", vpursuit), NA_real_, sum(grepl("Subject", injury))),
    passengers.injured = ifelse(grepl("not related", vpursuit), NA_real_, sum(grepl("Passenger", injury))),
    bystanders.injured = ifelse(grepl("not related", vpursuit), NA_real_, sum(grepl("Bystander", injury))),
    officers.injured = ifelse(grepl("not related", vpursuit), NA_real_, sum(grepl("Officer", injury)))
  ) %>%
  
  rowwise() %>%
  mutate(pbo.killed = passengers.killed + bystanders.killed + officers.killed,
         pb.killed = passengers.killed + bystanders.killed,
         total.injuries = sum(c_across(contains("injured"))), # note this probably underestimates
         pbo.injured = passengers.injured + bystanders.injured + officers.injured, # counts any, not number
         pb.injured = passengers.injured + bystanders.injured,
         pbo.total = pbo.killed + pbo.injured,
         pb.total = pb.killed + pb.injured,
         pbo.any = ifelse(pbo.total > 0, 1, 0),
         pb.any = ifelse(pb.total > 0, 1, 0)
  ) %>%
  #mutate(across(total.injuries:pb.any, ~ tidyr::replace_na(.,0))) %>%
  ungroup() %>%
  
  # map colors for all vehicle & pursuit cases
  mutate(inci.mapcolor = case_when(
    grepl("vehicular fatality", incident.type) & pbo.killed==1 ~ "red", 
    grepl("vehicular fatality", incident.type) & pbo.killed==0 ~ "blue", 
    grepl("homicide", incident.type) ~ "green",
    grepl("suicide", incident.type) ~ "beige"))



# Pursuits ----

## All pursuits = active + terminated + involved (with suicides)

## Fatalities: unit of analysis is the person killed
## exclude incident.type = "Attempted stop" & "Vehicle Accident"
## fatalities.p = pursuit veh fatalties (number 1-99) includes active and terminated
##                + after pursuit fatalities (number 100+) includes homicides and suicides

fatalities.p <- fatalities.all %>% 
  filter(grepl("Pursuit|After", incident.type)) %>%
  mutate(incident.type = factor(incident.type,
                                levels = c("After pursuit suicide", "After pursuit homicide",
                                           "Pursuit vehicular fatality")))


## Incidents: unit of analysis is the incident
## this is used for mapping and agency analyses

incidents.p <- incidents.all %>%
  filter(grepl("Pursuit|After", incident.type)) %>%
  mutate(incident.type = factor(incident.type,
                                levels = c("After pursuit suicide", "After pursuit homicide",
                                           "Pursuit vehicular fatality")))

## Pursuit vehicular fatalities (pvf)

fatalities.pvf <- fatalities.p %>% 
  filter(!grepl("After", incident.type)) 

incidents.pvf <- incidents.p %>% 
  filter(!grepl("After", incident.type)) 

## After pursuit fatalities (apf)

fatalities.apf <- fatalities.p %>% 
  filter(grepl("After", incident.type)) 

incidents.apf <- incidents.p %>% 
  filter(grepl("After", incident.type)) 

# Attempted stops (as) ----

fatalities.as <- fatalities.all %>%
  filter(grepl("stop", incident.type))

incidents.as <- incidents.all %>% 
  filter(grepl("stop", incident.type))


# Summary stats ----

## Pursuit fatalities % (incl active & after, with suicides)
pct.p <- nrow(fatalities.p) / nrow(fatalities.all)

## Pursuit vehicular fatality %

pct.pvf <- nrow(fatalities.pvf) / nrow(fatalities.all)

## Gunshot COD after pursuits
shot.apf <- fatalities.apf %>% 
  filter(cod=="Gunshot") %>%
  count() %>% as.numeric()

pct.shot.apf <- shot.apf/nrow(fatalities.apf)

## % Pursuit vehicular fatalities that are subjects
pct.subject.pvf <- sum(fatalities.pvf$victim == "Subject")/nrow(fatalities.pvf)


# Plot helpers, to get consistent color mapping and legends ----
# https://stackoverflow.com/questions/63358121/plotly-and-ggplot-legend-order-interaction


## fatalities by victim type and year

victim.types <- levels(fatalities.pvf$victim)
allrows.calyr.pvf <- data.frame(Year = sort(rep(min.yr:max.yr, length(victim.types))),
                                Victim = factor(rep(victim.types, length(min.yr:max.yr)),
                                                levels = victim.types))

allrows.legyr.pvf <- data.frame(leg.year = sort(rep(unique(fatalities.pvf$leg.year), length(victim.types))),
                                Victim = factor(rep(victim.types, length(unique(fatalities.pvf$leg.year))),
                                                levels = victim.types))


colors <- scales::brewer_pal(palette = "Spectral")(4) # Colors for active pursuit fatalities
cols.ap <- setNames(colors, victim.types) # Set names according to legend order

## incidents by incident type

inci.types <- levels(incidents.p$incident.type)
allrows.calyr.inci <- data.frame(Year = sort(rep(min.yr:max.yr, length(inci.types))),
                                 incident.type = factor(rep(inci.types, length(min.yr:max.yr)),
                                                        levels = inci.types))

allrows.legyr.inci <- data.frame(leg.year = sort(rep(unique(fatalities.pvf$leg.year), length(inci.types))),
                                 incident.type = factor(rep(inci.types, length(unique(fatalities.pvf$leg.year))),
                                                        levels = inci.types))


ramp <- scales::colour_ramp(c("seashell", "firebrick")) # Colors for incident types
colors.inci <- ramp(seq(0, 1, length = 3))
cols.inci <- setNames(colors.inci, inci.types) # Set names according to legend order


# Save all objects in environment

save.image(file = here::here("data-outputs", "FEpursuits.rda"))
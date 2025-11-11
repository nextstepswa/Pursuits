#
# Create clean pursuit dataframes from WA_2015 data
#

library(tidyverse)

# Key dates ----

date.start = as.Date("2015-01-01")

date.HB1054 = as.Date("2021-07-25") # Gov signed, effective 7/25
date.SB5352 = as.Date("2023-05-03") # Immediate activation
date.I2113 = as.Date("2024-06-05") # 90 days after end of session, 3/7

today = Sys.Date()

# Pre-post comparison metrics and anchors
# Note there are three periods:

## Pre reform (1/1/2015 - 7/24/2021)
## Reform (7/25/2024 - 6/5/2024)
## Rollback (6/6/2024 - current)

days.pre = as.numeric(date.HB1054 - date.start)
days.HB1054 = as.numeric(date.SB5352 - date.HB1054)
days.SB5352 = as.numeric(date.I2113 - date.SB5352)
days.I2113 = as.numeric(today - date.I2113)

yrs.pre = days.pre/365
yrs.HB1054 = days.HB1054/365
yrs.SB5352 = days.SB5352/365
yrs.I2113 = days.I2113/365


# Data ----
# Read in WA since 2015 data
# Data and code used to construct this is available in another
# GitHub repository:  https://github.com/nextstepswa/WA-FEWP

## Legacy pursuit coding is recovered (pre 11/2025)
## Current pursuit coding is done as cases are added to wa_local.xlsx file

load(file = "~/GitHub/WA-FEWP/Data/Clean/WA_2015.rda")
wa_2015 <- wa_clean_2015

min.yr <- min(wa_2015$year)
max.yr <- max(wa_2015$year)

## Agency data

load("~/GitHub/WA-LEAs/Data/Clean/WA.agencies.rda")

# All cases ----

## incident.type identifies pursuit and after pursuit cases from vpursuit
## pursuit.incinum is < 100 for multi-fatality incidents (coded manually), and
##  > 100 for single-fatality incidents though may not be stable over time

## Fatalities ----

fatalities.all <- wa_2015 %>% 

  mutate(
    agency.type = case_when(
      grepl("Other", agency.type) ~ "Other",
      TRUE ~ agency.type),
    
    incident.type = factor(
      case_when(
        grepl("Active|Terminated", pursuit.type) ~ "Pursuit vehicular fatality",
        grepl("Involved", pursuit.type) & mod == "Suicide" ~ "After pursuit suicide",
        grepl("Involved", pursuit.type) ~ "After pursuit homicide",
        pursuit.type == "Attempted stop" ~ "Attempted stop fatality",
        grepl("accident", pursuit.type) ~ "Vehicle accident fatality",
        TRUE ~ "All other fatalities"),
      levels = c("After pursuit homicide", "Pursuit vehicular fatality", 
                 "After pursuit suicide", "Attempted stop fatality", 
                 "Vehicle accident fatality", "All other fatalities")),
    
    traffic = if_else(grepl('traffic', description), "yes", "no"),
    cod3 = case_when(cod == "Vehicle" ~ "Vehicle",
                     cod == "Gunshot" ~ "Gunshot",
                     TRUE ~ "Other"),
    cod3 = forcats::fct_relevel(cod3, "Other", after=Inf),
    policy.period = case_when(
      date < date.HB1054 ~ "pre",
      date >= date.HB1054 & date < date.SB5352 ~ "HB1054",
      date >= date.SB5352 & date < date.I2113 ~ "SB5352",
      date >= date.I2113 ~ "I2113"),
    policy.period = factor(policy.period,
                           levels = c("pre", "HB1054", "SB5352", "I2113")),
    reform.period = ifelse(policy.period %in% c("HB1054", "SB5352"), 1, 0),
    rollback.period = ifelse(policy.period == "I2113", 1, 0)
  ) %>%
  arrange(desc(incident.type), date) %>%
  mutate(
    incident.num = if_else(
      !is.na(pursuit.incinum), pursuit.incinum, # multiple fatalities, same incident
      100 + as.numeric(rownames(.))),
    victim = factor(pursuit.victim,
                    levels = c("Officer", "Bystander", "Passenger", "Subject")))

## Incidents ----

incidents.all <- fatalities.all %>%
  group_by(incident.num) %>%
  summarize(
    date = first(date),
    year = first(year),
    month = first(month),
    day = first(day),
    leg.year = first(leg.year),
    policy.period = first(policy.period),
    pursuit.type = first(pursuit.type),
    incident.type = first(incident.type),
    lat = first(latitude),
    long = first(longitude),
    agency = first(agency),
    agency.type = first(agency.type),
    city = first(city),
    county = first(county),
    district = first(WA_District),
    url_info = first(url_info),
    url_click = first(url_click),
    fatalities = n(),
    inci.mapmult = ifelse(fatalities==1, "single", "mult"),
    
    # For now, the rest are for vehicle-related incidents only
    # So are NA for "Not pursuit related" cases
    
    subjects.killed = ifelse(grepl("Not pursuit related", pursuit.type), NA_real_, 
                             sum(grepl("Subject", victim))), # one IP case also had officer shot
    passengers.killed = sum(victim=="Passenger"),
    bystanders.killed = sum(victim=="Bystander"),
    officers.killed = sum(victim=="Officer"),
    subjects.injured = ifelse(grepl("Not pursuit related", pursuit.type), NA_real_, sum(grepl("Subject", pursuit.injury))),
    passengers.injured = ifelse(grepl("Not pursuit related", pursuit.type), NA_real_, sum(grepl("Passenger", pursuit.injury))),
    bystanders.injured = ifelse(grepl("Not pursuit related", pursuit.type), NA_real_, sum(grepl("Bystander", pursuit.injury))),
    officers.injured = ifelse(grepl("Not pursuit related", pursuit.type), NA_real_, sum(grepl("Officer", pursuit.injury)))
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
    grepl("fatality", incident.type) & pbo.killed==1 ~ "red", 
    grepl("fatality", incident.type) & pbo.killed==0 ~ "blue", 
    grepl("homicide", incident.type) ~ "green",
    grepl("suicide", incident.type) ~ "beige"))



# Pursuits ----

## All pursuits -----

### Included:  active + terminated + after pursuit
###   pursuit veh fatalities (number 1-99) includes active and terminated
###   after pursuit fatalities (number 100+) includes homicides and suicides
### Excluded: "Attempted stop" & "Vehicle Accident"

### Fatalities ----

fatalities.p <- fatalities.all %>% 
  filter(grepl("Pursuit|After", incident.type)) %>%
  mutate(incident.type = factor(incident.type,
                                levels = c("After pursuit suicide", "After pursuit homicide",
                                           "Pursuit vehicular fatality")))

### Incidents ----
### this is used for mapping and agency analyses

incidents.p <- incidents.all %>%
  filter(grepl("Pursuit|After", incident.type)) %>%
  mutate(incident.type = factor(incident.type,
                                levels = c("After pursuit suicide", "After pursuit homicide",
                                           "Pursuit vehicular fatality")))


## Pursuit vehicular fatalities (pvf) ----
## Includes: active + terminated pursuits
## Excludes: everything else

### Fatalities ----

fatalities.pvf <- fatalities.p %>% 
  filter(!grepl("After", incident.type)) 

### Incidents ----

incidents.pvf <- incidents.p %>% 
  filter(!grepl("After", incident.type)) 

## After pursuit fatalities (apf) ----
## Includes: after pursuit homicides and suicides
## Excludes: everything else

#### Fatalities ----
fatalities.apf <- fatalities.p %>% 
  filter(grepl("After", incident.type)) 

#### Incidents ----
incidents.apf <- incidents.p %>% 
  filter(grepl("After", incident.type)) 


## Attempted stops (as) ----

#### Fatalities ----
fatalities.as <- fatalities.all %>%
  filter(grepl("stop", incident.type))

#### Incidents ----
incidents.as <- incidents.all %>% 
  filter(grepl("stop", incident.type))


# Agencies ----

# There can be multiple agencies per incident or fatality.
# This df is used to make agencies the unit of analysis for counting;
# includes all pursuits, some will be counted more than once.

# Note that agency counts using fatalities as the unit of analysis
# may be obtained from the fatalities dataframes -- where multi-agency
# incidents are grouped together.

## Fatalities  ----

### One record per fatality ----
### up to 6 possible agencies recorded in cols 

agency.fatality.p.wide <- data.frame(
  cbind(str_split(fatalities.p$agency, ", ", simplify = TRUE),
        recID = fatalities.p$recID)
  ) %>%
  rename_with(., ~ sub("V", "agency",  .x)) %>%

  left_join(fatalities.p %>% select(recID, date, policy.period, incident.type, victim)) %>%
  
  select(recID, date, policy.period, incident.type, victim, contains("agency")) # reorder vars

### One record per involved agency per fatality ----
### multiple records per fatality if more than one agency was involved

agency.fatality.p.long <- agency.fatality.p.wide %>%
  pivot_longer(contains("agency"),
               names_to = "seq",
               values_to = "agency") %>%
  filter(agency != "") %>%
  left_join(fatalities.p %>% select(recID, date, policy.period, agency.type)) %>%

  # Fix "multiple agencies" type, since each record is a single agency
  mutate(agency.type = if_else(grepl("Multiple", agency.type),
                               case_when(
                                 grepl("Police", agency) ~ "Local PD",
                                 grepl("Tribal", agency) ~ "Tribal PD",
                                 grepl("Sherif", agency) ~ "County SO",
                                 grepl("County", agency) ~ "Other County",
                                 grepl("Correc", agency) ~ "Corrections",
                                 grepl("Patrol", agency) ~ "WSP",
                                 grepl("Fed|US", agency) ~ "Federal",
                                 TRUE ~ "Other"),
                               agency.type)
  )

### Agency FATALITY counts by policy.period since 2015 ----
### One record per agency
### fatalities counted multiple times if multiple agencies involved

agency.fatalities.period <- agency.fatality.p.long %>%
  
  group_by(agency, policy.period) %>%

  summarize(fatalities=n(),
            pursuit.veh.fatalities = sum(grepl("vehicular", incident.type)),
            after.pursuit.homicides = sum(grepl("homicide", incident.type)),
            after.pursuit.suicides = sum(grepl("suicide", incident.type)),
            subjects = sum(victim == "Subject"),
            bystanders = sum(victim == "Bystander"),
            passengers = sum(victim == "Passenger"),
            officers = sum(victim == "Officer"),
            agency.type = first(agency.type)
  ) %>%
  pivot_wider(id_cols = c(agency, agency.type),
              names_from = policy.period,
              values_from = fatalities:officers
              ) %>%
  select(agency, agency.type, contains("pre"), contains("HB"), contains("SB"), contains("I2")) %>%
  mutate(fatalities_tot = sum(across(contains("fatalities_")), na.rm=T),
         pursuit.veh.fatalities_tot = sum(across(contains("pursuit.veh.fatalities")), na.rm=T) ,
         after.pursuit.homicides_tot = sum(across(contains("after.pursuit.homicides")), na.rm=T),
         after.pursuit.suicides_tot = sum(across(contains("after.pursuit.suicides")), na.rm=T),
         subjects_tot = sum(across(contains("subjects")), na.rm=T),
         bystanders_tot = sum(across(contains("bystanders")), na.rm=T),
         passengers_tot = sum(across(contains("passengers")), na.rm=T),
         officers_tot = sum(across(contains("officers")), na.rm=T)
  ) %>%
  
  # merge on selected agency info
  left_join(agencies %>% select(agency, ORI, population, accredited)) %>%
  ungroup()
  

## Incidents:  ----

### One record per incident ----
### up to 6 possible agencies recorded

agency.incident.p.wide <- as.data.frame(
  cbind(str_split(incidents.p$agency, ", ", simplify = TRUE),
        incident.num = incidents.p$incident.num)
  ) %>%
  rename_with(., ~ sub("V", "agency",  .x)) %>%
  mutate(incident.num = as.numeric(incident.num)) %>%

  left_join(incidents.p %>% select(date, policy.period, incident.num, incident.type)) %>%
  
  select(date, policy.period, incident.num, incident.type, contains("agency"))

### One record per involved agency per incident ---- 
### multiple records per incident if multiple agencies involved

agency.incident.p.long <-
  agency.incident.p.wide %>%
  pivot_longer(contains("agency"),
               names_to = "seq",
               values_to = "agency") %>%
  filter(agency != "") %>%
  left_join(incidents.p %>% select(date, policy.period, incident.num, agency.type)) %>%
  
  # Fix "multiple agencies" type, since each record is a single agency
  mutate(agency.type = if_else(grepl("Multiple", agency.type),
                               case_when(
                                 grepl("Police", agency) ~ "Local PD",
                                 grepl("Tribal", agency) ~ "Tribal PD",
                                 grepl("Sherif", agency) ~ "County SO",
                                 grepl("County", agency) ~ "Other County",
                                 grepl("Correc", agency) ~ "Corrections",
                                 grepl("Patrol", agency) ~ "WSP",
                                 grepl("Fed|US", agency) ~ "Federal",
                                 TRUE ~ "Other"),
                               agency.type)
  )

### Agency INCIDENT totals by period since 2015 ----
### One record per agency
### incidents will be counted multiple times if multiple agencies involved

agency.incidents.period <- agency.incident.p.long %>%
  
  group_by(agency, policy.period) %>%
  
  summarize(incidents=n(),
            pursuit.veh.fatalities = sum(grepl("vehicular", incident.type)),
            after.pursuit.homicides = sum(grepl("homicide", incident.type)),
            after.pursuit.suicides = sum(grepl("suicide", incident.type)),
            agency.type = first(agency.type)
  ) %>%
  pivot_wider(id_cols = c(agency, agency.type),
              names_from = policy.period,
              values_from = incidents:after.pursuit.suicides
  ) %>%
  select(agency, agency.type, contains("pre"), contains("HB"), contains("SB"), contains("I2"))%>%
  mutate(incidents_tot= sum(across(contains("incidents_")), na.rm=T),
         pursuit.veh.incidents_tot = sum(across(contains("pursuit.veh.incidents")), na.rm=T) ,
         after.pursuit.homicides_tot = sum(across(contains("after.pursuit.homicides")), na.rm=T),
         after.pursuit.suicides_tot = sum(across(contains("after.pursuit.suicides")), na.rm=T)
  ) %>%
  
  # merge on selected agency info
  left_join(agencies %>% select(agency, ORI, population, accredited)) %>%
  ungroup()

# Remove intermediate datasets
# rm(list = ls(pattern= "*.long|*.wide"))


# Summary stats ----

## Pursuit fatalities % (incl active & after pursuits, with suicides; excl attempted stops & accidents)
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

## % of all fatal incidents that involve Pursuits
## (incl active & after, with suicides)
pct.incidents.p <- nrow(incidents.p) / nrow(incidents.all)


# Index plot helpers ----
## to get consistent color mapping and legends 
## https://stackoverflow.com/questions/63358121/plotly-and-ggplot-legend-order-interaction

## Calyr & Legyr Index ----

victim.types <-  levels(fatalities.p$victim) # all possible pursuit victim types, for pvf
incident.types.apf <- sort(unique(fatalities.apf$incident.type)) # only apf types
cal.yrs <- min.yr:max.yr
leg.yrs <- sort(unique(fatalities.p$leg.year))[-1] # from first fully covered leg year


### PVF fatalities by victim type and year ----

allrows.calyr.pvf <- data.frame(Year = sort(rep(cal.yrs, length(victim.types))),
                                Victim = factor(rep(victim.types, 
                                                    length(cal.yrs)),
                                                levels = victim.types))

allrows.legyr.pvf <- data.frame(leg.year = sort(rep(leg.yrs, length(victim.types))),
                                Victim = factor(rep(victim.types, 
                                                    length(leg.yrs)),
                                                levels = victim.types))

### APF fatalities by incident type and year ----

allrows.calyr.apf <- data.frame(Year = sort(rep(cal.yrs, length(incident.types.apf))),
                                incident = factor(rep(incident.types.apf, 
                                                      length(cal.yrs)),
                                                levels = incident.types.apf))

allrows.legyr.apf <- data.frame(leg.year = sort(rep(leg.yrs, length(incident.types.apf))),
                                incident = factor(rep(incident.types.apf, 
                                                      length(leg.yrs)),
                                                levels = incident.types.apf))



### Incidents by incident type ----

inci.types <- levels(incidents.p$incident.type)
allrows.calyr.inci <- data.frame(Year = sort(rep(cal.yrs, length(inci.types))),
                                 incident.type = factor(rep(inci.types, length(cal.yrs)),
                                                        levels = inci.types))

allrows.legyr.inci <- data.frame(leg.year = sort(rep(leg.yrs, length(inci.types))),
                                 incident.type = factor(rep(inci.types, length(leg.yrs)),
                                                        levels = inci.types))

## Color scales for calyr and legyr ----

### Fatalities ----
colors.pvf <- scales::brewer_pal(palette = "Spectral")(4) # Colors for active pursuit fatalities
cols.pvf.4 <- setNames(colors.pvf, victim.types) # Set names according to legend order for 4 grps
cols.pvf.2 <- setNames(c("firebrick", "lightblue3"), c("Other People", "Subject")) # For 2-grp breakdown

### Incidents ----
ramp <- scales::colour_ramp(c("seashell", "firebrick")) # Colors for incident types
colors.inci <- ramp(seq(0, 1, length = 3))
cols.inci <- setNames(colors.inci, inci.types) # Set names according to legend order


## Monthly Index  ----

## starts with 1/1/2015
## ends with last data month (whether complete or not -- select later if complete month end wanted)
## use "mo" for numeric month and "mon" for alpha month abb

start_yr <- 2015
start_mo <- 1
start_date <- as.Date(paste0(start_yr, "-", start_mo, "-01"))

curr_mo <- month(Sys.Date())
curr_yr <- year(Sys.Date())

last_complete_mo <- curr_mo -1
last_complete_yr <- curr_yr -1

numyrs <- last_complete_yr - start_yr + 1

## set up first year (in case we want to start other than Jan)
mon <- month.abb[start_mo:12]
year <- rep(start_yr, length(mon))

## generate middle months/years
for(i in 1:(numyrs-1)) {
  #print(i)
  thisyr = start_yr+i
  #print(thisyr)
  year <- c(year, rep(thisyr, 12))
  mon <- c(mon, month.abb)
}

## tack on last months/year for current year if
## month is not December (of prev yr)
if (curr_mo != 12) {
  year <- c(year, rep(curr_yr, curr_mo))
  mon <- c(mon, month.abb[1:curr_mo])
}

## Sequential complete mo/yr index for date plotting

index.monthly <- data.frame(year = year,
                            mon = mon,
                            mon.yr = paste0(mon, ".", year),
                            index.date = lubridate::ymd(
                              paste0(year, "-", mon, "-01")
                            ))


# Save all objects in environment

save.image(file = here::here("Data", "Clean", "WA_pursuits.rda"))




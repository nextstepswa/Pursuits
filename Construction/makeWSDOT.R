
# WSDOT crash data: 2015- 2.2023, fatal crashes only.  PDR 2.2023
# pre-cursor to WTSC and FARS data
# `Collision Report Number` = WTSC "par"
# Some errors fixed initially if they are needed for subsequent construction, others near end

# NB: We don't add on the additional WSP records for the Moreno case E832464
#   so those are unique to WTSC

# And there are 3 extra records in WSDOT for another pursuit case E812564 that
# Staci Hoff said were a collision that preceded the fatal collision, so WTSC
# counted this as a separate event and excluded them

rm(list=ls())
library(readxl)
library(tidyverse)

# County and City codes from GSA (see WA-pop/makeGSAgeocodes.R)
# You can contruct this by forking https://github.com/nextstepswa/WA-pop

load("~/GitHub/WA-pop/data-outputs/WA_GSA_geocodes.rda")
county.codes <- county.codes %>%
  mutate(county.name = str_to_title(county.name))
city.codes <- city.codes %>%
  mutate(city.name = str_to_title(city.name))


# WSDOT data

WSDOT.raw <- read_excel(here::here("data-raw", "WSDOT", "WSDOT_crash_data_2015-2023.xlsx"), 
                        skip = 1) %>%
  
  # NB: WSDOT does not have pursuit.crash tag
  mutate(pursuit.driver = if_else(grepl("pursuit", `MV Driver Miscellaneous Action 1`) |
                                    grepl("pursuit", `MV Driver Miscellaneous Action 2`) |
                                    grepl("pursuit", `MV Driver Miscellaneous Action 3`), 
                                  1, 0) 
  ) %>%
  
  
  # Exclusions:
  ## erroneous fatality/extra record for EA65822, per Staci Hoff email 4/11/2023
  ## extra record/other errors for 3796019 per Staci Hoff email 4/21/2023

  mutate(exclude = case_when(
    
    `Collision Report Number` == "EA65822" & 
      `Unit Number` == 3 &
      `Involved Person Type` == "MV Passenger" ~ 1, 
    
    `Collision Report Number` == "3796019" & is.na(`Injury Type`) ~ 1, 
    
    TRUE ~ 0),
    
    `Number of Fatalities` = ifelse(`Collision Report Number` == "EA65822", 
                                    `Number of Fatalities` - 1, 
                                    `Number of Fatalities`),
    
    `Involved Person Type` = ifelse(`Collision Report Number` == "3796019" &
                                      `Injury Type` == "Fatal", 
                                    "MV Driver",  
                                    `Involved Person Type`)) %>%
  filter(exclude==0) %>%
  
  arrange(`Collision Report Number`, `Unit Number`, `Involved Person Type`)

# Pursuit driver ids used to identify pursued vehicle and other records for this incident
# NB: unlike WTSC there is no pursuit.crash indicator
# Pursuit unit number will be used to identify passengers in the pursued vehicle

wsdot.pursued.vnums <- WSDOT.raw %>% filter(pursuit.driver == 1) %>%
  mutate(pursuit.tag=1) %>%
  select(`Collision Report Number`, pursuit.tag, vnum.p = `Unit Number`)

# Records for this incident

wsdot.inci.records <- WSDOT.raw %>%
  group_by(`Collision Report Number`) %>%
  count() %>%
  rename("records" = "n")

# Unique record id: inci.unit#.person#.  WSDOT doesn't have a person# so we need to
# loop thru to construct one

WSDOT.raw$pnumber <- rep(0, nrow(WSDOT.raw))
WSDOT.raw$recordID.wsdot <- rep("x", nrow(WSDOT.raw))

for(i in 1:nrow(WSDOT.raw)){
  if(i==1){
    WSDOT.raw$pnumber[i] <- 1
  } else {
    WSDOT.raw$pnumber[i] <- 
      ifelse(WSDOT.raw$`Collision Report Number`[i] == lag(WSDOT.raw$`Collision Report Number`)[i] &
               WSDOT.raw$`Unit Number`[i] == lag(WSDOT.raw$`Unit Number`)[i],
             lag(WSDOT.raw$pnumber)[i]+1,
             1)
  }
  WSDOT.raw$recordID.wsdot[i] <- paste0(WSDOT.raw$`Collision Report Number`[i], 
                                  "u", WSDOT.raw$`Unit Number`[i],
                                  "p", WSDOT.raw$pnumber[i])
                                  
}

# Convert the state plane coordinates to lat/lon -- 
# Note this relies on pkg rgdal, which will be retired at the end of 2023

source(here::here("geoconvertWSDOT.R"))

#aaa <- WSDOT.raw %>% select(`Collision Report Number`, `Unit Number`, `Involved Person Type`, pnumber, recordID.wsdot)

WSDOT <- WSDOT.raw %>%
  
  # First, join on all the external info
  
  left_join(., wsdot.pursued.vnums) %>%
  left_join(., wsdot.inci.records) %>%
  left_join(., ll.coords, by=c(`Collision Report Number` = "Collision.Report.Number")) %>%
  
  ## Standardize city names to GSA versions
  mutate(city.name = `City Name`,
         city.name = case_when(
           city.name == "DuPont" ~ "Dupont",
           city.name == "McCleary" ~ "Mc Cleary",
           city.name == "Moxee" ~ "Moxee City",
           city.name == "SeaTac" ~ "Seatac",
           TRUE ~ city.name)
  ) %>%
  rename("county.name" = `County Name`) %>%
  
  left_join(., county.codes, by = "county.name") %>%
  left_join(., city.codes, by = "city.name") %>%
  
  # Next, general transforms
  
  mutate(across(contains("pursuit"), ~replace_na(., 0))) %>%
  
  mutate(sex = case_when(
    Gender == "Male" ~ "M", 
    Gender == "Female" ~ "F",
    TRUE ~ "O/U")) %>%
  
  # Create proxy variable for number of persons - unlike WTSC there is no direct count
  # Here we use their indicator for unoccupied parked cars
  
  mutate(parked.empty.car = ifelse(grepl("Unoccupied", `Vehicle Action 1`), 1, 0),
         parked.empty.car = replace_na(parked.empty.car, 0)) %>% # most NA have age or sex values
  group_by(`Collision Report Number`) %>%
  mutate(persons = sum(parked.empty.car == 0)) %>%
  ungroup() %>%
  
  mutate(persons = as.numeric(persons),
         vehicles = as.numeric(`Number of Motor Vehicles Involved`),
         fatals = as.numeric(`Number of Fatalities`),
         records = as.numeric(records)
  ) %>%
  
  # Rename vars and organize by Incident/Vehicle/Person
  select(inciID.w = `Collision Report Number`,
         recordID.wsdot,
         pursuit.tag,
         county.name, county.code,
         city.name, city.code,
         latitude, longitude,
         date = Date,
         year = Year,
         mo.num = `Month Number`,
         quarter = `Quarter Number`,
         persons,
         vehicles,
         fatals,
         records,
         injuries = `Total Number of Injuries`,
         injuries.serious = `Total Serious Injuries`,
         pedestrians = `Number of Pedestrians Involved`,
         pedcyclists = `Number of Pedal Cyclists Involved`, 
         agency.investigation = `Investigative Agency`,
         agency.ori = `ORI#`,
         agency.name = `Agency Long Name`,
         agency.type = `Investigative Agency`,
         case.number = `Case Number`,
         vnum.p,
         
         vnum = `Unit Number`,
         veh.description = `Unit Type Description`,
         veh.govt = `Government Owned Indicator`,
         
         person.type = `Involved Person Type`,
         pursuit.driver, 
         officer = `On Duty Indicator`,
         parked.empty.car,
         age = Age,
         sex,
         injury.type = `Injury Type`,
         where.died = `Most Severe Injury Type`,
         ) %>%
  
  # Injury
  mutate(injury = case_when(
    grepl("Dead|Died", injury.type) ~ "fatal",
    grepl("Serious", injury.type) ~ "serious injury",
    grepl("Possible|Minor|Non", injury.type) ~ "minor injury",
    grepl("No", injury.type) ~ "no injury",
    TRUE ~ "Unknown")
  ) %>%
  
  # Person categories
  mutate(
    pcat.v = case_when(
      officer == "Yes" ~ "Officer",
      parked.empty.car == 1 ~ "Parked empty car",
      person.type == "MV Driver" ~ "Driver",
      person.type == "MV Passenger" ~ "Passenger",
      TRUE ~ "Non-motorist"),
    pcat.p = case_when(
      pursuit.tag == 0 ~ "Not tagged pursuit",
      officer == "Yes" & pursuit.tag == 1 ~ "Officer",
      pursuit.driver == 1 ~ "Subject",
      person.type == "MV Passenger" & vnum.p == vnum ~ "Passenger",
      parked.empty.car == 1 ~ "Parked empty car",
      pursuit.tag == 1 ~ "Bystander")
  )
  

# Data corrections -- 
# When found during matching to WTSC CFC data, 
#  for some attributes we assume WTSC is correct
#  for others we verify with Staci Hoff
# When found during matching to FE data, 
#   we verify using online sources before correcting

WSDOT$sex[WSDOT$inciID.w == "E812083"] <- "M" # from WTSC, only one record for this ID
WSDOT$sex[WSDOT$inciID.w == "EA38486" & WSDOT$age == 25] <- "M" # from WTSC, multiple records for this ID
WSDOT$sex[WSDOT$inciID.w == "EC90669" & WSDOT$age == 22] <- "M" # from FE Hardeep Chokhhar, multiple records for this ID
WSDOT$sex[WSDOT$inciID.w == "E883511" & WSDOT$recordID.wsdot == "E883511u1p1"] <- "M" # from WTSC, untagged nonfatal in both

WSDOT$pcat.v[WSDOT$inciID.w == "3876403" & WSDOT$recordID.wsdot == "3876403u1p1"] <- "Officer" # from WTSC, untagged nonfatal in both
WSDOT$pcat.v[WSDOT$inciID.w == "E936436" & WSDOT$recordID.wsdot == "E936436u2p1"] <- "Officer" # from WTSC, untagged nonfatal in both
WSDOT$pcat.v[WSDOT$inciID.w == "EA28579" & WSDOT$recordID.wsdot == "EA28579u2p1"] <- "Officer" # from WTSC, untagged nonfatal in both

# 3 extra records in WSDOT for E812564, 
# It is an untagged pursuit (WTSC tagged)
# these are bystanders from a crash earlier in the pursuit per Staci Hoff
WSDOT$pcat.p[WSDOT$inciID.w == "E812564" & WSDOT$vnum %in% c(6,7)] <- "Bystander"


WSDOT.min <- WSDOT %>%
  select(inciID.w, recordID.wsdot, pursuit.tag, pursuit.driver, 
         county.code, city.code, date, pcat.p, pcat.v,
         vnum, vnum.p, parked.empty.car,
         age, sex, injury, persons, vehicles, records, fatals)


WSDOT.pursuit.min <- WSDOT.min %>% 
  filter(pursuit.tag == 1)

wsdot.pursuit.inciIDs <- data.frame(inciID.w = unique(WSDOT.pursuit.min$inciID.w))
wsdot.pursued.vnums <- wsdot.pursued.vnums %>% rename("inciID.w" = `Collision Report Number`) %>% select(-pursuit.tag)

rm(list = ls(pattern="codes|dest|url|person|coords|pursuit.ids"))
rm(i)

save.image(here::here("data-outputs", "WSDOT_fatal_crash.rda"))

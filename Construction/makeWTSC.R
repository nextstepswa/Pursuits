
# WTSC crash data: 2015- 2021, fatal crashes only.  From Staci Hoff at WTSC 2.2023 (see email)
# Coding is similar to FARS, but varnames are different
# WTSC "par" = WSDOT `Collision Report Number`

# Hoff sent the CF data for 2015-2019 separately 3.6.2023

# Some extra records pulled in from WSP CAT for Moreno case b/c the pursued vehicle crashed
#  without a fatality 3 min after initial fatality event so has a different par (E823712)
#  The CAT data was transcribed manually into WTSC person file format
# Note:  this is an issue that may affect other cases as well, it would be difficult to
#  identify without the news coverage that we have for Moreno

rm(list=ls())
library(readxl)
library(tidyverse)

# County and City codes from GSA (see WA-pop/makeGSAgeocodes.R)
# You can contruct this by forking https://github.com/nextstepswa/WA-pop

load("~/GitHub/WA-pop/data-outputs/WA_GSA_geocodes.rda")
city.codes <- city.codes %>%
  mutate(city.name = str_to_title(city.name))

# WTSC data

person15 <- read_excel(here::here("data-raw", "WTSC", "person15.xlsx"))
person16 <- read_excel(here::here("data-raw", "WTSC", "person16.xlsx"))
person17 <- read_excel(here::here("data-raw", "WTSC", "person17.xlsx"))
person18 <- read_excel(here::here("data-raw", "WTSC", "person18.xlsx"))
person19 <- read_excel(here::here("data-raw", "WTSC", "person19.xlsx"))
person20 <- read_excel(here::here("data-raw", "WTSC", "person20.xlsx")) %>%
  mutate(accday = as.numeric(accday),
         accmon = as.numeric(accmon))
person21 <- read_excel(here::here("data-raw", "WTSC", "person21.xlsx"))

## extra records from WSP for Moreno case (includes CF data)
wsp.records <- read_excel(here::here("data-raw", "WSP", "person.wsp.xlsx"))

## CF data not included in person files for 2015-2019
WTSC.cfs <- readr::read_csv(here::here("data-raw", "WTSC", "2015-2019_CFs_CFC.csv"))


WTSC.raw <- bind_rows(list(lapply(ls(pattern = "person.*"), get))) %>%
  left_join(WTSC.cfs, by = c("par", "year")) %>%
  bind_rows(., wsp.records) %>% # b/c already has CF data
  arrange(crash_dt, par) %>%

  # Unique record number
  mutate(recordID.wtsc = ifelse(is.na(pnumber),
                                paste0(par, "v", vnumber, "p999"),
                                paste0(par, "v", vnumber, "p", pnumber)
                                )
  ) %>%
  
  mutate(across(cf1:cf3, ~ tidyr::replace_na(.,0))) %>%
  mutate(across(crf1:crf3, ~ tidyr::replace_na(.,0))) %>%
  mutate(across(drf1:drf4, ~ tidyr::replace_na(.,0))) %>%
  
  mutate(
    pursuit.driver = ifelse(drf1==37 | drf2==37 | drf3==37 | drf4==37, 1, 0),
    pursuit.crash = ifelse(cf1==20 | cf2==20 | cf3==20 | crf1==20 | crf2==20 | crf3==20, 1, 0),
    pursuit.tag = ifelse(pursuit.driver==1 | pursuit.crash==1, 1, 0),
    parked.empty.car = ifelse(regowner==6 & numoccs==0, 1, 0),
    is.officer = ifelse(drf1==16 | drf2==16 | drf3==16 | drf4==16, 1, 0)
  ) %>%
  
  # spot fixes, found during merges
  mutate(pursuit.driver = case_when(
    par == "3780577" & recordID.wtsc == "3780577v1p1" ~ 1, # drf not tagged, ID'd from feID 17519
    par == "E435713" & recordID.wtsc == "E435713v1p1" ~ 1, # drf not tagged, only one driver
    par == "E525196" & recordID.wtsc == "E525196v1p1" ~ 1, # drf not tagged, only one driver
    par == "E692481" & recordID.wtsc == "E692481v1p1" ~ 1, # drf not tagged, ID'd from feID 21769
    par == "E904233" & recordID.wtsc == "E904233v1p1" ~ 1, # drf not tagged, only one driver
    TRUE ~ pursuit.driver),
    
    pforms = ifelse(par=="E832464", as.numeric(pforms)+3, as.numeric(pforms)), # Moreno case
    vforms = ifelse(par=="E832464", as.numeric(vforms)+1, as.numeric(vforms)), # Moreno case
    is.officer = ifelse(par=="E832464" & recordID.wtsc == "E832464v0p1", 1, is.officer), # Moreno
    
    
  ) %>%

  # Remove odd mix of extra records and empty vehicles not marked as parked cars
  filter(!is.na(ptype) | (is.na(ptype) & parked.empty.car==1))

  
# Pursuit ids used to identify all records involved in this incident

wtsc.pursuit.ids <- data.frame(par = unique(WTSC.raw$par[WTSC.raw$pursuit.tag==1])) %>%
  filter(!is.na(par))

# Pursuit vehicle number used to identify pursued vehicle and its passengers

wtsc.pursued.vnums <- WTSC.raw %>% filter(pursuit.driver == 1) %>%
  select(par, vnum.p = vnumber)
  
# Records for this incident

wtsc.inci.records <- WTSC.raw %>%
  group_by(par) %>%
  count() %>%
  rename("records" = "n")


WTSC <- WTSC.raw %>%
  
  # First join on all the external info
  
  left_join(., wtsc.pursuit.ids) %>%
  left_join(., wtsc.pursued.vnums) %>%
  left_join(., wtsc.inci.records) %>%
  
  # Some WTSC city codes don't exist in the GSA; inspection suggests
  # the changes made here
  # Lots of city codes == 0, are missing
  # For pursuit cases, looked up missing city names/codes using lat/long in google
  # fixed here using inciID.w b/c (need for matching)
  
  mutate(city.code = ifelse(city==0, NA_real_, city),
         city.code = case_when(
           city.code == 111 ~ 2550, # Bainbridge Island
           city.code == 171 ~ 170,  # Bothell
           city.code == 1124 ~ 1122, # Lakewood
           par == "E853014" ~ 2370, # Salmon Creek (Vancouver)
           par == "E979909" ~ 630, # Elma
           par == "E423885" ~ 2230, # Waller (Tacoma)
           par == "2545845" ~ 1960, # White Center (Seattle)
           par == "EA62201" ~ 1322, # Valleyford, near Spokane (Mica)
           par == "EA40438" ~ 340, # Chewelah
           TRUE ~ city.code)
  ) %>%
  left_join(., city.codes, by = "city.code") %>%
  
  mutate(across(contains("pursuit"), ~replace_na(., 0)),
         is.officer = replace_na(is.officer, 0)) %>%
  
  mutate(sex = case_when(
    sex == 1 ~ "M", 
    sex == 2 ~ "F",
    TRUE ~ "O/U")) %>%
  
  mutate(
    pcat.v = case_when(
      ptype %in% c(1,2,5,9) & spuse == 5 ~ "Officer",
      is.officer == 1 ~ "Officer",
      ptype == 1 ~ "Driver",
      ptype %in% c(2,9) ~ "Passenger",
      ptype == 3 ~ "Parked car occupant",
      ptype %in% c(5,6,7,12,13,19) ~ "Non-motorist",
      parked.empty.car == 1 ~ "Parked empty car",
      TRUE ~ "Other"),
    pcat.p = case_when(
      pursuit.tag == 0 ~ "Not tagged pursuit",
      ptype %in% c(1,2,5,9) & spuse == 5 ~ "Officer",
      is.officer == 1 ~ "Officer",
      ptype == 1 & pursuit.driver==1 ~ "Subject",
      ptype %in% c(2,9) & vnum.p==vnumber ~ "Passenger",
      parked.empty.car == 1 ~ "Parked empty car",
      pursuit.tag == 1 ~ "Bystander")
    ) %>%
  
  mutate(persons = as.numeric(pforms+nmforms), # To be consistent with other datasets
         vehicles = as.numeric(vforms),
         nonmotorists = as.numeric(nmforms),
         fatals = as.numeric(numfatal),
         records = as.numeric(records),
         injury.type = injury,
         injury = case_when(
           injury.type==4 ~ "fatal",
           injury.type==3 ~ "serious injury",
           injury.type %in% c(1, 2, 5) ~ "minor injury",
           injury.type==0 ~ "no injury",
           TRUE ~ "Unknown")
             
  ) %>%
  
  # Rename vars and organize by Incident/Vehicle/Person
  select(inciID.w = par,
         recordID.wtsc,
         pursuit.crash, pursuit.tag,
         county.name = co_char, county.code = county,
         city.name, city.code,
         latitude = y, longitude = x,
         date = crash_dt,
         year,
         mo.num = accmon,
         persons, # all p, including nonmotorists
         vehicles,
         nonmotorists,
         fatals,
         records,
         agency.invest.name = investjur,
         agency.report.num = repjur,
         agency.report.name = repag_long,
         vnum.p,
         
         vnum = vnumber,
         numoccs,
         spuse, # 5=police
         
         pursuit.driver,  
         pcat.p,
         pcat.v,
         injury, 
         age,
         sex,
         hispanic,
         race = race_me,
         is.officer,
         parked.empty.car,
         ptype,
         pnumber,
         injury.type, diedscene, death_dt
         
         # roadclass,
         # urbrur,
         # drf1:drf4,
         # crf1:crf3,
         # race1:race5
  )

# Check city names and codes
#with(WTSC, table(is.na(city.code), is.na(city.name), useNA = "al"))
#table(WTSC$city.code[is.na(WTSC$city.name)])
  


#WTSC$sex[WTSC$inciID.w == "EC90669" & WTSC$age == 22] <- "M" # 2022 case, may be needed b/c WSDOT is wrong

WTSC.min <- WTSC %>%
  select(inciID.w, recordID.wtsc, pursuit.tag, pursuit.driver, pursuit.crash, 
         county.code, city.code, date, pcat.p, pcat.v, 
         vnum, vnum.p, numoccs, parked.empty.car,
         age, sex, race, hispanic, injury, persons, vehicles, records, fatals)

WTSC.pursuit.min <- WTSC.min %>% 
  filter(pursuit.tag == 1)

wtsc.pursuit.inciIDs <- data.frame(inciID.w = unique(WTSC.pursuit.min$inciID.w))
wtsc.pursued.vnums <- wtsc.pursued.vnums %>% rename("inciID.w" = "par")

rm(list = ls(pattern="codes|dest|url|person|wsp|pursuit.ids"))

save.image(here::here("data-outputs", "WTSC_fatal_crash.rda"))


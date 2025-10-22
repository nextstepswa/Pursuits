###########################################################################################

# This file matches the records of pursuit related deaths and injuries across 3 sources:
#  * WTSC - The CFC data, available via request (or PDR)
#  * WSDOT - The "crash data", available via PDR
#  * FE

# It is possible to match the FARS data, but WTSC CFC has more info so is preferable for WA.
###########################################################################################

rm(list=ls())
library(tidyverse)

# Check if any files need to be rebuilt ----

## Orig WSDOT and WTSC files ----

### WSDOT ----
if(file.exists(here::here("data-outputs", "WSDOT_fatal_crash.rda"))) {
  if(
    file.info(here::here("data-outputs", "WSDOT_fatal_crash.rda"))$mtime <
    file.info(here::here("makeWSDOT.R"))$mtime )  {
    source(here::here("makeWSDOT.R"))
  }
} else {
  source(here::here("makeWSDOT.R"))  
}
rm(list=ls())

### WTSC ----
if(file.exists(here::here("data-outputs", "WTSC_fatal_crash.rda"))) {
  if(
    file.info(here::here("data-outputs", "WTSC_fatal_crash.rda"))$mtime <
    file.info(here::here("makeWTSC.R"))$mtime )  {
    source(here::here("makeWTSC.R"))
  }
} else {
  source(here::here("makeWTSC.R"))  
}
rm(list=ls())

## Matched W ----
if(file.exists(here::here("data-outputs", "matched.WTSC.WSDOT.rda"))) {
  if(
    file.info(here::here("data-outputs", "matched.WTSC.WSDOT.rda"))$mtime <
    file.info(here::here("match.WTSC.WSDOT.R"))$mtime )  {
    
    source(here::here("match.WTSC.WSDOT.R"))
  }
} else {
  source(here::here("match.WTSC.WSDOT.R")) 
}
rm(list=ls())

## FE ----
if(
  file.info(here::here("data-outputs", "FEpursuits.rda"))$mtime <
  file.info("~/Github/fewapo/data-outputs/WA2015.rda")$mtime) {
  source(here::here("makeFEpursuits.R"))
  rm(list=ls())
}

##########################################################################
# Load current files and continue ----

## WTSC/WSDOT matched pursuits file ----
## Records include all persons and vehicles involved in a fatal accident,
##   including parked empty cars
## Use matched.fatalities df for matching to FE

load(here::here("data-outputs", "matched.WTSC.WSDOT.rda"))

# Select the cleaned consensus variables with .w suffix
matched.w.fatals <- matched.fatalities.w %>%
  select(inciID.w:records.w)

## WTSC and WSDOT complete files ----
## For matching residual FE pursuits to untagged WTSC/WDOT
## Add WTSC/WSDOT suffixes to vars not used for matching

load(here::here("data-outputs", "WTSC_fatal_crash.rda"))
WTSC.min.fatals <- WTSC.min %>%
  filter(injury == "fatal") %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  rename_with(~sub("(.*)", "\\1.wtsc", .), 
              .cols=c(pursuit.tag:pursuit.crash,
                      city.code, pcat.p:parked.empty.car,
                      race:fatals))

WTSC.pursuit.min.fatals <- WTSC.pursuit.min %>%
  filter(injury == "fatal") %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  rename_with(~sub("(.*)", "\\1.wtsc", .), 
              .cols=c(pursuit.tag:pursuit.crash,
                      city.code, pcat.p:parked.empty.car,
                      race:fatals))

load(here::here("data-outputs", "WSDOT_fatal_crash.rda"))
WSDOT.min.fatals <- WSDOT.min %>%  
  filter(injury == "fatal") %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  rename_with(~sub("(.*)", "\\1.wsdot", .), 
              .cols=c(pursuit.tag, pursuit.driver,
                      city.code, pcat.p:parked.empty.car,
                      injury:fatals))

# don't need the WSDOT.pursuit.min as it's a subset of WTSC


## County and City codes from GSA (see WA-pop/makeGSAgeocodes.R) ----
## You can construct this by forking https://github.com/nextstepswa/WA-pop

load("~/GitHub/WA-pop/data-outputs/WA_GSA_geocodes.rda")
city.codes <- city.codes %>%
  mutate(city.name = str_to_title(city.name))
county.codes <- county.codes %>%
  mutate(county.name = str_to_title(county.name))

## FE ----
## Select vcod fatalities that have any relation to pursuit, 
## in case we can match them to untagged incidents in WTSC/WSDOT
## Mimic the pursuit.min df format from WSTC/WSDOT as much as possible
## Add .fe suffix to vars not used for matching

load(file = here::here("data-outputs", "FEpursuits.rda"))
rm(list = ls(pattern="allrows|last|pct|scrape|shot|fe_data|wapo_data"))

fe.pursuit.min <- fatalities.all %>%
  filter(pursuit.type != "Reviewed not related") %>%

  rename("pcat.p" = "victim",
         "other.injuries" = "injury") %>%
  
  mutate(injury = "fatal",
         sex = case_when(
           gender == "Male" ~ "M",
           gender == "Female" ~ "F",
           TRUE ~ "O/U"),
         year = lubridate::year(date),
         month = lubridate::month(date),
         status.fe=2
  ) %>%
  
  # Change census designated places to closest city
  mutate(city.name = case_when(
    city == "Cathcart" ~ "Mill Creek",
    city == "Birchfield" ~ "Moxee City",
    city == "Salmon Creek" ~ "Vancouver",
    city == "Martha Lake" ~ "Mill Creek",
    city == "Sunnydale" ~ "Burien",
    TRUE ~ city)
  ) %>%
  left_join(city.codes) %>%
  left_join(county.codes, by = c("county" = "county.name")
  ) %>%
  select(feID, inciID.fe = incident.num, status.fe,
         date, year, month, county.code, age=age.fe, sex, 
         city.code, vpursuit, cod, pcat.p, injury, other.injuries,
         county.name = county, city.name = city) %>%
  
  rename_with(~sub("(.*)", "\\1.fe", .), 
              .cols=c(city.code:city.name))


fe.inci <- fe.pursuit.min %>%
  group_by(inciID.fe) %>%
  summarize(
    persons.fe = n(),
    pcat.p1.fe = first(pcat.p.fe),
    pcat.p2.fe = nth(pcat.p.fe, 2)
  )

fe.pursuit.min <- left_join(fe.pursuit.min, fe.inci, 
                            by = "inciID.fe")

## FE vcod, keeps all active pursuits, filters out most "Involved pursuit"
fe.vcod <- fe.pursuit.min %>%
  filter(cod.fe == "Vehicle")

fe.involved <- fe.pursuit.min %>%
  filter(cod.fe != "Vehicle")


## FARS (not used) ----
#load(file = here::here("data-outputs", "FARS.rda"))

# One way to examine the data:
# all.pursuits <- bind_rows(fe.pursuit.min, WSDOT.pursuit.min, WTSC.pursuit.min) %>%
#   select(contains("ID"), contains("county"), contains("city"), date, age, sex, pcat.p, fatals) %>%
#   arrange(date)

################################################################
# Fatality Matching ----
# FE to matched.w, and untagged WSDOT/WTSC


## 1. Join FE vcod pursuits to W pursuits ----

## Needs careful inspection when first starting
## Check for duplicate record IDs -- duplicates need to be removed 
## if all keys are the same with "distinct"
## Use .w extension to indicate this is a consensus variable

# (n=59)
match.w.fe.draft <- full_join(
  matched.w.fatals, 
  fe.vcod)
  
# Matched pursuits (n=13)
pursuit.matched.fatalities <- match.w.fe.draft %>%
  filter(!is.na(inciID.w) & !is.na(feID)) 

# Matched incidents (n=11)
pursuit.matched.inciIDs <- pursuit.matched.fatalities %>%
  group_by(inciID.w) %>%
  summarize(feID = first(feID),
            num.fatal = n())


## 2. Pull unmatched W and FE pursuit records ----

## Unmatched WTSC pursuits ----
## (n=15 fatality records)
## reverts to structure of matched.w.fatals + match.status)

## This is a dead end for now, as FE has no add'l vehicle
## fatalities to use for residual matching

# (n=15)
unmatched.w.fatalities <- match.w.fe.draft %>%
  filter(is.na(feID)) %>%
  select(inciID.w) %>%
  left_join(matched.w.fatals) %>%
  mutate(status.fe = 0)

# (n=15)
unmatched.w.fatals.ids <- unmatched.w.fatalities %>%
  group_by(inciID.w)  %>%
  count() %>%
  rename("wtsc.fatalities" = "n")
  

## Unmatched FE pursuits ----
## (n=31, 21 for 2015-21)
## Will look for these in the untagged WTSC records

unmatched.fe.fatalities <- match.w.fe.draft %>%
  filter(is.na(inciID.w)) %>%
  select(feID) %>%
  left_join(fe.vcod)


## Join unmatched FE pursuit IDs to untagged records in WTSC & WSDOT ----
## (Note: pursuits were matched above, so residuals will only match untagged)
## Varname extensions for WTSC/WSDOT need to be added manually so dfs can be
## merged later without ambiguity

## We do this iteratively:

## a. Left join to WTSC.min.fatals using exact date

## b. Compare residual FE to WTSC.min.fatals for possible matches / error corrections
##    Some errors were found/corrected/matched, but some residual remain
##    No fuzzy year/month matches seemed plausible

##    Corrected FE vars:
##    26981 (missing age and sex, corrected)
##    28776, 28777 (age 56, corrected)
##    29763 (age 32 corrected)

##    3 FE pursuits remain unmatched for 2015-21 (+10 2022+)
##    25804, Richard Lee from Tad Norman case in Lake City, SPD
##    28774, Long chase from Lynwood to 145th, SnoCo Sheriff
##    90059, Trooper Justin Schaffer, Thurston Co Sheriff
##    Visual inspection shows none are in WTSC.pursuit.min or WTSC.min

## c. Left join residual FE to WSDOT.min.fatals -- to pick up 2022+ and possibly others

#######################################################################################

### a. Left join to WTSC using exact date ----
untagged.wtsc.fatalities.draft <- left_join(
  unmatched.fe.fatalities, 
  WTSC.min.fatals)

## Matches 18 FE pursuits, incl. 1 Involved pursuit
## We assume the matched fatalities are also present in WSDOT
untagged.wtsc.fatalities <- untagged.wtsc.fatalities.draft %>%
  filter(!is.na(inciID.w)) %>%
  mutate(status.wtsc = 1,
         status.wsdot = 1)


### b. Inspect Residual FE unmatched for possible WTSC matches, fix any errors ----
##    Residual FE:  13 pursuits after corrections, 
##    Set WSDOT status to 0, modify if they match below

residual.fe.from.wtsc <- untagged.wtsc.fatalities.draft %>%
  filter(is.na(inciID.w)) %>%
  select(feID) %>%
  left_join(fe.vcod) %>%
  mutate(status.wtsc = 0,
         status.wsdot = 0)

### c. Match residual FE to WSDOT.min  ----
##    picks up 7 cases, all from 2022

untagged.wsdot.fatalities.draft <- left_join(
  residual.fe.from.wtsc,
  WSDOT.min.fatals)
 # some city.code discrepancies, but neighboring

untagged.wsdot.fatalities <- untagged.wsdot.fatalities.draft %>%
  filter(!is.na(inciID.w)) %>%
  mutate(status.wsdot = 1)

unmatched.fe.fatalities <- untagged.wsdot.fatalities.draft %>%
  filter(is.na(inciID.w)) %>%
  select(feID) %>%
  left_join(fe.vcod) %>%
  mutate(status.wtsc=0,
         status.wsdot=0)


with(untagged.wtsc.fatalities, table(pcat.p.fe, pcat.v.wtsc, useNA = "al"))## 3. Add pursuit fatalities from each match outcome ----

all.pursuit.fatalities <- bind_rows(
  pursuit.matched.fatalities %>% mutate(inclusion = "FE+W"),
  untagged.wtsc.fatalities %>% mutate(inclusion = "FE+untagged-WTSC"), 
  untagged.wsdot.fatalities %>% mutate(inclusion = "FE+untagged-WSDOT"),
  unmatched.w.fatalities %>% mutate(inclusion = "WTSC-only"),
  unmatched.fe.fatalities %>% mutate(inclusion = "FE-only")
  ) 
%>%
  
  # rename(injury.pcat.p.fe = injury.fe,
  #        fatals.fe = persons.fe) %>%
  
  mutate(
    inciID = ifelse(is.na(inciID.w), inciID.fe, inciID.w),
    pursuit.type = if_else(is.na(vpursuit.fe), "Active pursuit", vpursuit.fe),
    tag.status = case_when(
      status.fe == 2 & status.wtsc < 2 & status.wsdot < 2 ~ "FE-only",
      status.fe == 0 & (status.wtsc == 2 | status.wsdot == 2) ~ "W-only",
      status.fe == 2 & (status.wtsc == 2 | status.wsdot == 2) ~ "FE+W",
      TRUE ~ "Other"),
  ) %>%
  
  # Consensus W variables
  mutate(
    status.w = ifelse(status.wtsc != 0 | status.wsdot != 0, 1, 0),
    injury.w = "fatal",
    pcat.p.w = ifelse(is.na(pcat.p.w), pcat.p.wtsc, pcat.p.w), # no pcat.p.wsdot
    parked.empty.car.w = ifelse(is.na(parked.empty.car.w), parked.empty.car.wtsc, parked.empty.car.w),
    pcat.v.w = case_when(
      !is.na(pcat.v.w) ~ pcat.v.w,
      !is.na(pcat.v.wtsc) ~ pcat.v.wtsc,
      !is.na(pcat.v.wsdot) ~ pcat.v.wsdot),
    pcat.p.w = case_when(
      !is.na(pcat.p.w) ~ pcat.p.w,
      !is.na(pcat.p.wtsc) ~ pcat.p.wtsc),
    persons.w = case_when(
      !is.na(persons.w) ~ persons.w,
      !is.na(persons.wtsc) ~ persons.wtsc,
      !is.na(persons.wsdot) ~ persons.wsdot),
    vehicles.w = case_when(
      !is.na(vehicles.w) ~ vehicles.w,
      !is.na(vehicles.wtsc) ~ vehicles.wtsc,
      !is.na(vehicles.wsdot) ~ vehicles.wsdot),
    fatals.w = case_when(
      !is.na(fatals.w) ~ fatals.w,
      !is.na(fatals.wtsc) ~ fatals.wtsc,
      !is.na(fatals.wsdot) ~ fatals.wsdot),
    records.w = case_when(
      !is.na(records.w) ~ records.w,
      !is.na(records.wtsc) ~ records.wtsc,
      !is.na(records.wsdot) ~ records.wsdot),
    city.code.w = case_when(
      !is.na(city.code.w) ~ city.code.w,
      !is.na(city.code.wtsc) ~ city.code.wtsc,
      !is.na(city.code.wsdot) ~ city.code.wsdot)
  ) %>%
  
  # Consensus city code -- WTSC takes precedence
  # Note county code is used for matching, so is already consistent
  # Will rejoin names for both county and city
  mutate(city.code = ifelse(is.na(city.code.w), city.code.fe, city.code.w)
  ) %>%
  
  left_join(city.codes) %>%
  left_join(county.codes) %>%

  select(inciID, inciID.w, inciID.fe, recordID.wsdot, recordID.wtsc, feID, 
         pursuit.type, inclusion, tag.status, status.wsdot, status.wtsc, status.w, status.fe,
         date, year, month, county.code, age, sex, 
         city.code, city.name, county.name,
         pcat.v.w:records.w,
         city.code.fe:fatals.fe,
         pursuit.tag.wtsc:fatals.wtsc,
         pursuit.tag.wsdot:fatals.wsdot)


table(all.pursuit.fatalities$pursuit.type, all.pursuit.fatalities$inclusion, useNA = "al")
table(all.pursuit.fatalities$pursuit.type, all.pursuit.fatalities$tag.status, useNA = "al")

all.pursuit.fatalities %>%
  filter(tag.status == "FE-only") %>%
  group_by(inclusion) %>%
  count()

#############################################################################
# Nonfatal records from WTSC/WSDOT for these incidents ----

# Note:  20 untagged incidents were found using FE above, 
#            14 WTSC and 6 WSDOT (all 2022+)
#        26 incidents were WTSC tagged, so those nonfatals are in matched.records
#        


## 1. New untagged nonfatals matched to FE incidents ----
##    status.fe = 2 for all of these
#     Need to match the 20 newly identified incidents for WTSC/WSDOT
#     non-fatals -- using inciID.w, age, sex, pcat.v
#     This uses .min files so have to add suffixes manually where needed

#     Also need to pull the info from FE to try to identify pcat.p status

untagged.w.pursuit.ids <- all.pursuit.fatalities %>%
  group_by(inciID.w) %>%
  summarize(inclusion = first(inclusion),
            tag.status = first(tag.status),
            inciID.fe = first(inciID.fe),
            feID = first(feID),
            pcat.p.fe = first(pcat.p.fe)) %>%
  filter(grepl("FE-untagged", inclusion))

### WTSC non-fatals ----
untagged.nonfatal.wtsc <- left_join(untagged.w.pursuit.ids, WTSC.min) %>% 
  filter(injury != "fatal") %>%
  mutate(status.wtsc = 1)

### WSDOT non-fatals ----
untagged.nonfatal.wsdot <- left_join(untagged.w.pursuit.ids, WSDOT.min)%>% 
  filter(injury != "fatal") %>%
  mutate(status.wsdot = 1)

### Match them ----
untagged.nonfatal.match.draft <- full_join(untagged.nonfatal.wtsc, untagged.nonfatal.wsdot,
                           by = c("inciID.w", "inciID.fe", "feID", "age", "sex", "pcat.v"),
                           suffix = c(".wtsc", ".wsdot")) %>%
  rename_with(~sub("(.*)", "\\1.wtsc", .), 
              .cols=c(pursuit.crash, ptype, pnumber, numoccs))

# 2 dupes from parked cars, so 46 records total
# aaa <- table(untagged.nonfatal.match.draft$recordID.wtsc, useNA = "al")
# wtsc.dupes <- names(aaa[aaa>1])
# bbb <- table(untagged.nonfatal.match.draft$recordID.wsdot, useNA = "al")
# wsdot.dupes <- names(bbb[bbb>1])

### Pull unique records by match status ----
untagged.nonfatal.wsdot.wtsc <- untagged.nonfatal.match.draft %>%
  filter(!is.na(recordID.wtsc) & !is.na(recordID.wsdot)) %>%
  distinct(recordID.wtsc, .keep_all = TRUE)

untagged.nonfatal.wtsc.only <- untagged.nonfatal.match.draft %>%
  filter(!is.na(recordID.wtsc) & is.na(recordID.wsdot)) %>%
  distinct(recordID.wtsc, .keep_all = TRUE) %>%
  mutate(status.wsdot=0)

untagged.nonfatal.wsdot.only <- untagged.nonfatal.match.draft %>%
  filter(is.na(recordID.wtsc) & !is.na(recordID.wsdot)) %>%
  distinct(recordID.wsdot, .keep_all = TRUE) %>%
  mutate(status.wtsc=0)

### Add them together ----
### Note: pcat.v used for matching, so there is no separate wtsc/wsdot var

### Still waiting on resolution of discrepancies for 3796019

untagged.nonfatal.w <- bind_rows(untagged.nonfatal.wsdot.wtsc,
                                 untagged.nonfatal.wtsc.only,
                                 untagged.nonfatal.wsdot.only) %>%
  
  rename(pcat.v.w = pcat.v) %>%

  mutate(status.fe = 2,
         pcat.p.fe = pcat.p.fe.wsdot, # WTSC has some NAs
         pcat.v.wtsc = pcat.v.w,
         pcat.v.wsdot = pcat.v.w,
         inclusion = ifelse(!is.na(inclusion.wsdot), inclusion.wsdot, inclusion.wtsc),
         tag.status = ifelse(!is.na(tag.status.wtsc), tag.status.wtsc, tag.status.wsdot),
         date = if_else(!is.na(date.wtsc), date.wtsc, date.wsdot),
         county.code = ifelse(!is.na(county.code.wtsc), county.code.wtsc, county.code.wsdot),
         city.code.w = ifelse(!is.na(city.code.wtsc), city.code.wtsc, city.code.wsdot),
         pcat.p.w = ifelse(!is.na(pcat.p.wtsc), pcat.p.wtsc, pcat.p.wsdot),
         parked.empty.car.w = ifelse(!is.na(parked.empty.car.wtsc), parked.empty.car.wtsc, parked.empty.car.wsdot),
         injury.w = ifelse(!is.na(injury.wtsc), injury.wtsc, injury.wsdot), # 2 discrepancies, choose WTSC (none vs. Unknown)
         persons.w = ifelse(!is.na(persons.wtsc), persons.wtsc, persons.wsdot), # 2 discrepancies, need to check
         vehicles.w = ifelse(!is.na(vehicles.wtsc), vehicles.wtsc, vehicles.wsdot),
         records.w = ifelse(!is.na(records.wtsc), records.wtsc, records.wsdot), # 2 discrepancies, need to check
         fatals.w = ifelse(!is.na(fatals.wtsc), fatals.wtsc, fatals.wsdot)
  ) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)
  ) %>%
  select(inciID.w, inciID.fe, recordID.wtsc, recordID.wsdot, feID, pcat.p.fe,
         inclusion, tag.status, status.wsdot, status.wtsc, status.fe,
         date, county.code, age, sex, year, month, 
         pcat.v.w, city.code.w:fatals.w,
         inclusion.wtsc, tag.status.wtsc, pcat.v.wtsc, 
         pursuit.tag.wtsc:fatals.wtsc,
         inclusion.wsdot, tag.status.wsdot, pcat.v.wsdot, 
         pursuit.tag.wsdot:fatals.wsdot)

# check discrepancies -- most vars match across both datasets
# there are 2 discrepancies, from one incident, 3796019
# have emailed Staci Hoff 4/12/2023

injury <- untagged.nonfatal.w %>% filter(injury.wtsc != injury.wsdot)
persons <- untagged.nonfatal.w %>% filter(persons.wtsc != persons.wsdot)
records <- untagged.nonfatal.w %>% filter(records.wtsc != records.wsdot)


## 2. Previously matched tagged nonfatals in w ----

## Will need tag.status, inclusion and status.fe matched on
pursuit.fatality.info <- all.pursuit.fatalities %>%
  group_by(inciID.w) %>%
  summarize(status.fe = first(status.fe),
            inciID.fe = first(inciID.fe),
            pcat.p.fe = first(pcat.p.fe),
            pcat.p.w = first(pcat.p.w),
            tag.status = first(tag.status),
            inclusion = first(inclusion),
            vnumber.wtsc = first(vnumber.wtsc)) %>%
  filter(!is.na(inciID.w))

tagged.nonfatal.w <- matched.records %>%
  filter(injury.w != "fatal") %>%
  left_join(pursuit.fatality.info, by = "inciID.w")


## 3. Add the matched nonfatals together ----

nonfatal.matched.w <- bind_rows(untagged.nonfatal.w,
                                tagged.nonfatal.w) 




# Add fatal and nonfatal records together ----

all.pursuit.records <- bind_rows(all.pursuit.fatalities,
                                 nonfatal.matched.w) %>%
  rowwise() %>%
  mutate(status.w2 = ifelse(is.na(status.w), max(status.wsdot, status.wtsc, na.rm=T), status.w))

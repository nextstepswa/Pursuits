#####################################################################################
# Pursuit matching is a 3 step process for each pair of datasets

## 1. Join WTSC pursuit records to WSDOT pursuit records, identify unmatched cases.
##    In this case, WSDOT pursuit records are a subset of WTSC pursuit records
## 2. Take unmatched WTSC cases and join to untagged WSDOT records.  
##    Matched cases here indicate the WSDOT record exists, but was not tagged as a pursuit.
## 3. Add the pursuit-matched, and matched but untagged records together

## The variables "status.xxx" indicates how this record was matched:
## 2 = pursuit tagged
## 1 = untagged
## 0 = not found

# There is also some iterative cleaning as we debug the matches.  All cleaning is
# implemented back in the original data source make scripts.

## In general we assume wtsc coding is correct when there is a conflict
#####################################################################################

rm(list=ls())
library(tidyverse)

# Make data if needed ----
# Since files are cleaned iteratively, we check to see if rebuilds are needed before loading

## WSDOT ----
if(file.exists(here::here("data-outputs", "WSDOT_fatal_crash.rda"))) {
  if(
    file.info(here::here("data-outputs", "WSDOT_fatal_crash.rda"))$mtime <
    file.info(here::here("makeWSDOT.R"))$mtime )  {
      source(here::here("makeWSDOT.R"))
    }
} else {
  source(here::here("makeWSDOT.R"))  
}

## WTSC ----
if(file.exists(here::here("data-outputs", "WTSC_fatal_crash.rda"))) {
  
  # check if rebuild needed
  if(
    file.info(here::here("data-outputs", "WTSC_fatal_crash.rda"))$mtime <
    file.info(here::here("makeWTSC.R"))$mtime )  {
    source(here::here("makeWTSC.R"))
  }
} else {
  source(here::here("makeWTSC.R"))  
}

# Load data files ----

rm(list=ls())
load(file = here::here("data-outputs", "WSDOT_fatal_crash.rda"))
load(file = here::here("data-outputs", "WTSC_fatal_crash.rda"))

# 1. Join pursuits to pursuits ----

## Duplicates need to be removed when all keys are the same 
## Do this by creating unique record ID, then keep with "distinct" (1 incident)
## (n=69)
pursuit.fulljoin.wtsc.wsdot <- full_join(WTSC.pursuit.min %>% rowid_to_column("ID"), 
                              WSDOT.pursuit.min %>% rowid_to_column("ID"), 
                              by = c("inciID.w", "county.code", "age", "sex"),
                              suffix = c(".wtsc", ".wsdot")) %>%
  distinct(ID.wtsc, .keep_all = TRUE) 

# Matched pursuits (n=36 records)
pursuit.matched.records <- pursuit.fulljoin.wtsc.wsdot %>%
  filter(!is.na(recordID.wtsc) & !is.na(recordID.wsdot)) %>%
  select(-c("ID.wtsc", "ID.wsdot"))%>%
  mutate(status.wsdot = 2,
         status.wtsc = 2)

# (n = 14 incidents incl Moreno)
pursuit.matched.inciIDs <- pursuit.matched.records %>%
  group_by(inciID.w) %>%
  summarize(vnum.p.wtsc = first(vnum.p.wtsc),
            vnum.p.wsdot = first(vnum.p.wsdot))

# (n = 12 incidents, all in WTSC, none in WSDOT)
pursuit.unmatched.inciIDs <- data.frame(inciID.w = unique(pursuit.fulljoin.wtsc.wsdot$inciID.w)) %>%
  anti_join(., pursuit.matched.inciIDs) %>%
  left_join(., wsdot.pursuit.inciIDs %>% mutate(in.wsdot=1)) %>%
  left_join(., wtsc.pursuit.inciIDs %>% mutate(in.wtsc=1))

## Note:  WSDOT pursuit incidents turn out to be a subset of WTSC pursuit incidents
## But both datasets have extra records for matched pursuits

# 2. Try to find the unmatched WTSC pursuit records in WSDOT non-pursuits ----

## Pull the unmatched records

## Unmatched wtsc pursuit-tagged records (n=33, 3 from Moreno)
unmatched.wtsc.records <- pursuit.fulljoin.wtsc.wsdot %>%
  filter(is.na(recordID.wsdot)) %>%
  select(inciID.w, recordID.wtsc) %>%
  left_join(WTSC.pursuit.min)

## Unmatched pursuit-tagged wsdot records (n=0)
unmatched.wsdot.records <- pursuit.fulljoin.wtsc.wsdot %>%
  filter(is.na(recordID.wtsc)) %>%
  select(inciID.w, recordID.wsdot) %>%
  left_join(WSDOT.pursuit.min)


## Identify pursuits that are untagged by WSDOT
## Join unmatched WTSC pursuit incident IDs to non-pursuit records in WSDOT
## NOTE:  this may identify additional WSDOT records for wtsc tagged pursuits, 
##        if the incident, but not the records are in the WTSC data.  
##        Currently there are 3 such person records for par==E812564
##        Per email from Staci Hoff, these vehicles were hit during
##        the incident, but preceding the crash that caused the fatality
##        so WTSC defined them as not part of the "event."

## (n=33)
untagged.wsdot.records <- left_join(pursuit.unmatched.inciIDs, 
                                    WSDOT.min, 
                                    by = "inciID.w") %>%
  select(-c(in.wsdot, in.wtsc))

## Match untagged WSDOT records back to unmatched WTSC records, allowing for
## extra cases from both datasets.  Resulting data should have the same
## structure as the matched pursuits

## Records without WTSC data indicate additional WSDOT records for that incident
## Records without WSDOT data indicate additional WTSC records for that incident
## Assign inciID.w to these

## Empty parked cars will generate duplicate matches, use distinct inciID.comb to de-dup

## n=36: 30 untagged WSDOT matched to WTSC, 3 extra WSDOT, 3 extra WTSC from Moreno
untagged.matched.records <- full_join(unmatched.wtsc.records %>% rowid_to_column("ID"), 
                                      untagged.wsdot.records %>% rowid_to_column("ID"), 
                                      by = c("inciID.w", "county.code", "age", "sex"),
                                      suffix = c(".wtsc", ".wsdot")) %>%
  mutate(ID.comb = ifelse(!is.na(ID.wtsc), ID.wtsc, 10*ID.wsdot)) %>%
  distinct(ID.comb, .keep_all = TRUE) %>%
  
  mutate(status.wsdot = ifelse(is.na(recordID.wsdot), 0, 1), # tags the additional WTSC records,
         status.wtsc = ifelse(is.na(recordID.wtsc), 0, 2) # tags the additional WSDOT records

  ) %>%
  
  # pull missing vnum and vnum.p for E812564 extra records from WSDOT
  # vnum comes from WSDOT vnum; vnum.p comes from incident vnum.p.wtsc
  left_join(., wtsc.pursued.vnums) %>%
  mutate(vnum.wtsc = ifelse(is.na(vnum.wtsc), vnum.wsdot, vnum.p.wtsc),
         vnum.p.wtsc = ifelse(is.na(vnum.p.wtsc), vnum.p, vnum.p.wtsc)) %>%
  
  select(inciID.w, recordID.wtsc, recordID.wsdot, 
         status.wtsc, status.wsdot,
         pursuit.tag.wtsc:fatals.wtsc,
         pursuit.tag.wsdot:fatals.wsdot)


# 3. Add matched and untagged matched records together ----
#    Create consensus w vars

#    Vars without .w are used for matching later
#    Note:  Records includes parked empty cars

# n=72 (36 pursuit matched + 36 untagged matched)
matched.records.w <- bind_rows(pursuit.matched.records,
                             untagged.matched.records) %>%
  
  mutate(date = if_else(!is.na(date.wtsc), date.wtsc, date.wsdot),
         year = lubridate::year(date),
         month = lubridate::month(date),
         pcat.v.w = ifelse(is.na(pcat.v.wtsc), pcat.v.wsdot, pcat.v.wtsc),
         pcat.p.w = ifelse(is.na(pcat.p.wtsc), pcat.p.wsdot, pcat.p.wtsc), # wsdot info less accurate
         injury.w = if_else(!is.na(injury.wtsc), injury.wtsc, injury.wsdot),
         parked.empty.car.w = ifelse(is.na(parked.empty.car.wtsc), parked.empty.car.wsdot, parked.empty.car.wtsc),
         vnum.w = ifelse(is.na(vnum.wtsc), vnum.wsdot, vnum.wtsc),
         vnum.p.w = ifelse(is.na(vnum.p.wtsc), vnum.wsdot, vnum.p.wtsc),
         city.code.w = ifelse(!is.na(city.code.wtsc), city.code.wtsc, city.code.wsdot),
         pursuit.crash.w = 1,
         
         # incident totals are trickier given extra records in both datasets, but fatals are consistent
         persons.w = case_when(
           is.na(persons.wtsc) ~ persons.wsdot,
           is.na(persons.wsdot) ~ persons.wtsc,
           persons.wsdot > persons.wtsc ~ persons.wsdot,
           TRUE ~ persons.wtsc),
         vehicles.w = case_when(
           is.na(vehicles.wtsc) ~ vehicles.wsdot,
           is.na(vehicles.wsdot) ~ vehicles.wtsc,
           vehicles.wsdot > vehicles.wtsc ~ vehicles.wsdot,
           TRUE ~ vehicles.wtsc),
         fatals.w = if_else(!is.na(fatals.wtsc), fatals.wtsc, fatals.wsdot),
         records.w = case_when(
           is.na(records.wtsc) ~ records.wsdot,
           is.na(records.wsdot) ~ records.wtsc,
           records.wsdot > records.wtsc ~ records.wsdot,
           TRUE ~ records.wtsc)
  ) %>%
  
  select(inciID.w, recordID.wtsc, recordID.wsdot, 
         status.wtsc, status.wsdot, 
         date, year, month, county.code, age, sex,
         pcat.v.w:records.w,
         pursuit.tag.wtsc:vnum.p.wtsc,
         numoccs.wtsc=numoccs, parked.empty.car.wtsc,
         injury.wtsc:fatals.wtsc,
         pursuit.tag.wsdot:fatals.wsdot)

# Persons only
matched.persons.w <- matched.records.w %>%
  filter(parked.empty.car.w != 1)

# Fatalities only  
matched.fatalities.w <- matched.persons.w %>%
  filter(injury.w == "fatal")

fatal.info.inci <- matched.fatalities.w %>%
  group_by(inciID.w) %>%
  summarize(records.w = n(),
            fatal.pcat.p1.w = first(pcat.p.w),
            fatal.pcat.p2.w = nth(pcat.p.w, 2),
            fatal.vnum1.w = first(vnum.w),
            fatal.vnum2.w = nth(vnum.w, 2)
            ) %>%
  select(-c(records.w))

# Incidents
matched.incidents.w <- matched.records.w %>%
  group_by(inciID.w) %>%
  summarize(
    status.wsdot = max(status.wsdot), # max b/c extra records
    status.wtsc = max(status.wtsc),
    year = first(year),
    month = first(month),
    date = first(date),
    county.code = first(county.code),
    city.code.w = first(city.code.w),
    persons.w = first(persons.w),
    vehicles.w = first(vehicles.w),
    fatals.w = first(fatals.w),
    records.w = n(),
    vnum.p.w = first(vnum.p.w)
  ) %>%
  left_join(fatal.info.inci)

# Save outfiles ----

save(list = c("matched.records.w", "matched.persons.w",
              "matched.fatalities.w", "matched.incidents.w"),
     file = here::here("data-outputs", "matched.WTSC.WSDOT.rda"))

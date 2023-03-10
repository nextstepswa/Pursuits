
# WTSC crash data: 2015- 2021, fatal crashes only.  From Staci Hoff at WTSC 2.2023 (see email)
# Hoff sent the CF data separately 3.6.2023
# Coding is similar to FARS, but varnames are different
# WTSC "par" = WSDOT `Collision Report Number`

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
person21 <- read_excel(here::here("data-raw", "WTSC", "person21pre.xlsx"))

WTSC.cfs <- readr::read_csv(here::here("data-raw", "WTSC", "2015-2019_CFs_CFC.csv"))

# orig <- read_excel(here::here("data-raw", "WTSC", "Old", "WTSC_pursuit fatalities_MR_2-9-2023.xlsx"))
# orig.inci <- orig %>% group_by(par) %>% count()
# new.inci <- WTSC.raw %>% filter(pursuit.tag == 1) %>% group_by(par) %>% count()


# cftemp is a hack until Staci Hoff adds the cfX variable series to the data
# we're using the par IDs from the line level data that Max sent originally
# for 2015-2019 data

WTSC.raw <- bind_rows(list(lapply(ls(pattern = "person.*"), get))) %>%
  left_join(WTSC.cfs, by = c("par", "year")) %>%
  mutate(across(cf1:cf3, ~ tidyr::replace_na(.,0))) %>%
  mutate(across(crf1:crf3, ~ tidyr::replace_na(.,0))) %>%
  mutate(across(drf1:drf3, ~ tidyr::replace_na(.,0))) %>%
  
  mutate(
    pursuit.driver = ifelse(drf1==37 | drf2==37 | drf3==37 | drf4==37, 1, 0),
    pursuit.crash = ifelse(cf1==20 | cf2==20 | cf3==20 | crf1==20 | crf2==20 | crf3==20, 1, 0),
    pursuit.tag = ifelse(pursuit.driver==1 | pursuit.crash==1, 1, 0)
  )



# Pursuit ids will be used to identify all records involved in this incident

wtsc.pursuit.ids <- data.frame(wtscID = unique(WTSC.raw$par[WTSC.raw$pursuit.tag==1])) %>%
  filter(!is.na(wtscID)) %>%
  mutate(pursuit.tag.id=1)
  

WTSC <- WTSC.raw %>%
  left_join(., wtsc.pursuit.ids, by = c("par" = "wtscID")) %>%
  
  mutate(sex = case_when(
    sex == 1 ~ "M", 
    sex == 2 ~ "F",
    TRUE ~ "O/U")) %>%
  
  mutate(
    victim.all = case_when(
      ptype %in% c(1,2,9) & spuse == 5 ~ "Officer",
      ptype == 1 ~ "Driver",
      ptype %in% c(2,9) ~ "Passenger",
      pursuit == 1 ~ "Pursuit Bystander",
      ptype %in% c(5,6,7,12,13,19) ~ "Non-motorist",
      is.na(ptype) ~ "Unknown",
      TRUE ~ "Other"),
    victim = case_when(
      is.na(ptype) & pursuit.driver==0 & pursuit==1 ~ "Bystander", # by deduction
      ptype %in% c(1,2,9) & spuse == 5 ~ "Officer",
      ptype == 1 & pursuit.driver==1 ~ "Subject",
      ptype %in% c(2,9) & pursuit.driver==1 ~ "Passenger",
      pursuit == 1 ~ "Bystander",
      is.na(ptype) ~ "Unknown")
    ) %>%
  select(wtscID = par,
         pursuit, pursuit.driver, pursuit.crash, pursuit.tag, pursuit.tag.id, victim,
         victim.all,
         date = crash_dt,
         year,
         mo.num = accmon,
         county.name = co_char, county.code = county,
         city.code = city,
         person.num = pnumber,
         ptype,
         injury.type = injury, diedscene, death_dt,
         vehicle.num = vnumber,
         spuse, # 5=police
         persons = pforms,
         vehicles = vforms,
         nonmotorists = nmforms,
         fatals = numfatal,
         agency.invest.name = investjur,
         agency.report.num = repjur,
         agency.report.name = repag_long,
         age,
         sex,
         hispanic,
         race = race_me,
         latitude = y,
         longitude = x,
         roadclass,
         urbrur,
         drf1:drf4,
         crf1:crf3,
         race1:race5
  ) %>%
  
  # Injury
  mutate(injury = case_when(
    injury.type==4 ~ "fatal",
    injury.type==3 ~ "serious injury",
    injury.type %in% c(1, 2, 5) ~ "minor injury",
    injury.type==0 ~ "no injury",
    TRUE ~ "Unknown")
  ) %>%

  # Some WTSC city codes don't exist in the GSA; inspection suggests
  # the changes made here
  mutate(city.code = ifelse(city.code==0, NA_real_, city.code),
         city.code = case_when(
           city.code == 111 ~ 2550, # Bainbridge Island
           city.code == 171 ~ 170,  # Bothell
           city.code == 1124 ~ 1122) # Lakewood
         ) %>%
  left_join(., city.codes, by = "city.code")

with(WTSC, table(is.na(city.code), is.na(city.name), useNA = "al"))
#table(WTSC$city.code[is.na(WTSC$city.name)])


WTSC.min <- WTSC %>%
  select(wtscID, pursuit, county.code, city.code, date, victim, victim.all, age, sex, ptype, injury, persons, vehicles, fatals)

WTSC.pursuit.min <- WTSC.min %>% 
  filter(pursuit == 1) %>% 
  select(-c(pursuit, victim.all))

rm(list = ls(pattern="codes|dest|url|person"))

save.image(here::here("data-outputs", "WTSC_fatal_crash.rda"))

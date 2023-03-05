
# WSDOT crash data: 2015- 2.2023, fatal crashes only.  PDR 2.2023
# pre-cursor to WTSC and FARS data
# `Collision Report Number` = WTSC "par"

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
  mutate(pursuit.driver = if_else(grepl("pursuit", `MV Driver Miscellaneous Action 1`) |
                                    grepl("pursuit", `MV Driver Miscellaneous Action 2`) |
                                    grepl("pursuit", `MV Driver Miscellaneous Action 3`), 
         1, 0)
  )

# Pursuit ids will be used to identify other records involved in this incident

wsdot.pursuit.ids <- data.frame(wsdotID = unique(WSDOT.raw$`Collision Report Number`[WSDOT.raw$pursuit.driver==1])) %>%
  filter(!is.na(wsdotID)) %>%
  mutate(pursuit=1)


WSDOT <- WSDOT.raw %>%
  left_join(., wsdot.pursuit.ids, by = c(`Collision Report Number` = "wsdotID")) %>%
  select(wsdotID = `Collision Report Number`,
         pursuit.driver, pursuit,
         county.name = `County Name`,
         city.name = `City Name`,
         date = Date,
         year = Year,
         month = `Month Name` ,
         mo.num = `Month Number`,
         quarter = `Quarter Number`,
         person.type = `Involved Person Type`,
         fatals = `Number of Fatalities`,
         injuries = `Total Number of Injuries`,
         injuries.serious = `Total Serious Injuries`,
         where.died = `Most Severe Injury Type`,
         vehicles = `Number of Motor Vehicles Involved`,
         pedestrians = `Number of Pedestrians Involved`,
         agency.investigation = `Investigative Agency`,
         agency.ori = `ORI#`,
         agency.name = `Agency Long Name`,
         agency.type = `Investigative Agency`,
         age = Age,
         sex = Gender,
         case.number = `Case Number`,
         unit.number = `Unit Number`,
         unit.description = `Unit Type Description`,
         govt.unit = `Government Owned Indicator`
         ) %>%
  # standardize to GSA versions - but GSA does not have Friday Harbor
  mutate(city.name = case_when(
    city.name == "DuPont" ~ "Dupont",
    city.name == "McCleary" ~ "Mc Cleary",
    city.name == "Moxee" ~ "Moxee City",
    city.name == "SeaTac" ~ "Seatac",
    TRUE ~ city.name)
  ) %>%
  left_join(., county.codes, by = "county.name") %>%
  left_join(., city.codes, by = "city.name")
  

WSDOT.min <- WSDOT %>%
  select(wsdotID, pursuit, county.name, county.code, city.name, city.code, date, age, sex, person.type, fatals)

WSDOT.pursuit.min <- WSDOT.min %>% filter(pursuit == 1) 

rm(list = ls(pattern="codes|dest|url"))

save.image(here::here("data-outputs", "WSDOT_fatal_crash.rda"))

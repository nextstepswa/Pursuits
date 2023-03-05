#
# USA Today data on fatal pursuits 2013-2014
# forked from https://github.com/USATODAY/data/tree/master/police-chases
# analysis published: https://www.usatoday.com/pages/interactives/blacks-killed-police-chases-higher-rate/
#

library(tidyverse)
library(readr)

url <- "https://raw.githubusercontent.com/USATODAY/data/master/police-chases/fatal_pursuits_2013-2014.csv"
destfile <- (here::here("data-raw", "USAT_fatal_pursuits_2013-2014.csv"))
curl::curl_download(url, destfile)

fatal_pursuits_2013_2014 <- read_csv(destfile)

load("~/GitHub/WA-pop/data-outputs/WA_GSA_geocodes.rda")
county.codes <- county.codes %>%
  mutate(county.name = str_to_title(county.name))
city.codes <- city.codes %>%
  mutate(city.name = str_to_title(city.name))

USAT.WA <- fatal_pursuits_2013_2014 %>%
  filter(ST == "WA") %>%
  rename_with(., tolower) %>%
  rename("county.name" = "county", "city.name" = "city", 
         "day" = "date", "usatID" = "seq") %>%
  mutate(city.name = ifelse(city.name == "Lynwood", "Lynnwood", city.name),
         date = as.Date(paste0(year, "-", mo, "-", day))) %>%
  left_join(., county.codes, by = "county.name") %>%
  left_join(., city.codes, by = "city.name") %>%
  select(usatID, date, county.name, county.code, city.name, city.code, case:age7)

with(USAT.WA, table(is.na(city.name), is.na(city.code)))
#USAT.WA$city.name[is.na(USAT.WA$city.code)]

rm(list = ls(pattern="codes|dest|url"))
Note <- "Original file in https://github.com/moxboxwa/USATodayPursuitdata"

save.image(here::here("data-outputs", "USAT_WA_fatal_pursuits.rda"))

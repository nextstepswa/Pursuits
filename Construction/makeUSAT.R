###############################################################################################

# USA Today pursuit data: 2013-14
# my GitHub repo: https://github.com/moxboxwa/USATodayPursuitdata
# forked from USAT repo: https://github.com/USATODAY/data

# USAT does not have any cases beyond FE,
# it includes terminated pursuits and attempted stops
# But only FE has "involved pursuit" cases

###############################################################################################

#library(readxl)
library(tidyverse)

# FE clean data (no extra pursuit coding yet, so will do that here)

load("~/Github/fewapo/data-outputs/CleanData.rda")

# USAT data

USAT.raw <- read_csv(here::here("data-raw", "USAT_fatal_pursuits_2013-2014.csv"))

USAT.wa <- USAT.raw %>% filter(ST == "WA")
fe1314 <- fe_clean %>% filter(year==2013 | year==2014) %>% filter(st=="WA") %>%
  mutate(pursuit.tag = case_when(
    !is.na(vpursuit.draft) ~ 1, # All DBB pursuit related codes and 2022+ codes
    grepl('vehicle|car|crash|speed|chase|pursuit|flee|fled', description) ~ 2, # any other indication of vehicle or pursuit
    cod == "Vehicle" ~ 3 # this doesn't seem to pick up any additional, but may in the future
  )) %>%
  filter(!is.na(pursuit.tag)) %>%
  select(feID, name, date, homicide, suicide, not.kbp, pursuit.tag,  vpursuit.draft, cod, description, url_info)

# google sheet location for pursuit coding
coded.pursuit.file.location <- "https://docs.google.com/spreadsheets/d/18ZC93f_VbTXOMj_VyJQITnd83HcRDD1cefrFAGDKqQU/edit?usp=sharing"

googlesheets4::gs4_auth(
  email = TRUE, #gargle::gargle_oauth_email(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

# To create googlesheet from scratch
# googlesheets4::sheet_add(ss=coded.pursuit.file.location, 
#                             sheet = "2013-2014_data")
# googlesheets4::sheet_append(ss=coded.pursuit.file.location, 
#                          data = fe1314, 
#                          sheet = "2013-2014_data")

# Read in coded data
googlesheets4::gs4_deauth()
raw.coded.pursuits <- googlesheets4::read_sheet(coded.pursuit.file.location, 
                                                sheet = "2013-2014_data")

coded.pursuits <-  raw.coded.pursuits %>%
  mutate(vpursuit = if_else(!is.na(vpursuit.final), vpursuit.final, vpursuit.draft),
         vpursuit = str_to_title(vpursuit),
         #cod = if_else(!is.na(cod.final), cod.final, cod),
         victim = victim.final,
         #description = if_else(!is.na(description.final), description.final, description),
         url_info = if_else(!is.na(url_additional), url_additional, url_info)
  ) %>%
  select(feID, name, date, vpursuit, cod, victim, injury, description, url_info, incident.num,
         pursuit.notes = notes)


fe1314.final <- left_join(fe1314 %>% select(-"url_info"), 
                        coded.pursuits %>% select(-c("name", "date")),
                        by = "feID") %>%
  mutate(pursuit.type = ifelse(grepl("Active|Terminated", vpursuit), "Pursuit", vpursuit)) %>%
  select(-vpursuit.draft)

fe1314.pursuits <- fe1314.final %>% 
  filter(pursuit.type != "Reviewed Not Related") %>%
  select(feID, vpursuit, pursuit.type, incident.num) %>%
  left_join(fe_clean, fe1314.final, by="feID") %>%
  mutate(mo.num = match(month, month.abb))

# Match FE to USAT
matched.fe.usat <- full_join(fe1314.pursuits,
                             USAT.wa %>% mutate(usat = 1),
                             by = c("county" = "County", "year" = "Year", "mo.num" = "Mo", "day" = "Date")) %>%
  select(feID, usat, Seq, vpursuit:county, agency, Seq:Deaths, People, Vehs, `Fleeing driver`, `Pursuing agency`) %>%
  mutate(usat = tidyr::replace_na(usat, 0))

table(matched.fe.usat$vpursuit, matched.fe.usat$usat, useNA = "al")

save(list = c("USAT.raw", "USAT.wa", "fe1314.pursuits", "matched.fe.usat"),
     file = here::here("data-outputs", "USAT.rda"))

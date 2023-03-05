################################################################
# Scrapes and creates a harmonized FARS datafile
#
## Reads the FARS data from the NTHSA ftp site
## Reads the County/City codes from the GSA sige
## 
## Note: there are lots of persnickety differences in 
## FARS filenames and variables over the years
##
## Outputs fars.RDA with:
##
### Lists of original files from each year
### Geocode-name conversion file for counties/cities in WA
### fars.year - clean files for each year
### pursuits -- subset of combined fars, all observed pursuit fatalities
##
##############################################################


# From the 2020 FAR manual p.558
#
# Note that data elements DR_CF1-DR_CF4 were renamed to DR_SF1-DR_SF4 in 2010.
# Specific fatality types can be identified as follows:
# 1. occupant of police vehicle - all occupants (PER_TYP IN (1,2,9)) of special use vehicle police (SPEC_USE=5)
# 2. occupant of chased vehicle - all occupants (PER_TYP IN (1,2,9)) of vehicle with a driver having a driver related factor of high speed chase with police in pursuit (DR_CF1=37 or DR_CF2=37 or DR_CF3=37 [or (DR_CF4=37) since 1997)].
# 3. occupant of other vehicle - all other occupants (PER_TYP IN (1,2,9)) - excludes occupant of police vehicle and chased vehicle
# 4. non-occupant - pedestrians, pedalcyclists, and other non-occupants (PER_TYP IN (3,4,5,6,7,8,10,19))
# 5. unknown - (PER_TYP=99), this code existed for 1 year – 1996


library(tidyverse)

# County and City codes from GSA (see WA-pop/makeGSAgeocodes.R)
# You can contruct this by forking https://github.com/nextstepswa/WA-pop

load("~/GitHub/WA-pop/data-outputs/WA_GSA_geocodes.rda")

# FARS data

temp <- tempfile()

for(i in 2015:2020){
  
  # Download the dataset from NHTSA
  
  farsfile <- paste0("https://static.nhtsa.gov/nhtsa/downloads/FARS/", i,
                     "/National/FARS", i,
                     "NationalCSV.zip")
  download.file(farsfile,temp)
  
  # Read in the WA data: different filenames/files by year
  
  if(i %in% c(2015, 2018)){
    person <- read.csv(unz(temp, "person.csv")) %>%
      filter(STATENAME == "Washington")
    vehicle <- read.csv(unz(temp, "vehicle.csv")) %>%
      filter(STATENAME == "Washington")
    accident <- read.csv(unz(temp, "accident.csv")) %>%
      filter(STATENAME == "Washington")
    
  }
  
  if(i %in% c(2016, 2017)) {
    person <- read.csv(unz(temp, "Person.CSV")) %>%
      filter(STATENAME == "Washington")
    vehicle <- read.csv(unz(temp, "Vehicle.CSV")) %>%
      filter(STATENAME == "Washington")
    accident <- read.csv(unz(temp, "accident.CSV")) %>%
      filter(STATENAME == "Washington")
    
  }
  
  if(i == 2019) {
    person <- read.csv(unz(temp, "Person.CSV")) %>%
      filter(STATENAME == "Washington") 
    vehicle <- read.csv(unz(temp, "vehicle.csv")) %>%
      filter(STATENAME == "Washington")
    accident <- read.csv(unz(temp, "accident.CSV")) %>%
      filter(STATENAME == "Washington")
    race <- read.csv(unz(temp, "Race.CSV")) %>%
      filter(STATENAME == "Washington")
   
  }
  
  if(i == 2020){
    person <- read.csv(unz(temp, "person.csv")) %>%
      filter(STATENAME == "Washington")
    vehicle <- read.csv(unz(temp, "vehicle.csv")) %>%
      filter(STATENAME == "Washington")
    accident <- read.csv(unz(temp, "accident.CSV")) %>%
      filter(STATENAME == "Washington")
    race <- read.csv(unz(temp, "race.csv")) %>%
      filter(STATENAME == "Washington")
    driverrf <- read.csv(unz(temp, "driverrf.CSV")) %>%
      filter(STATENAME == "Washington")
    crashrf <- read.csv(unz(temp, "crashrf.CSV")) %>%
      filter(STATENAME == "Washington")
    
  }
  
  # Harmonize and create final fars datasets, keeping original files
  
  if(i < 2019) {
    
    pers <- person %>%
      mutate(date = as.Date(paste0(i, "-", MONTH, "-", DAY)),
             death.date = if_else(INJ_SEV==4, as.Date(paste0(2015, "-", DEATH_MO, "-", DEATH_DA)), as.Date(NA))) %>%
      select(date, death.date, ST_CASE, VEH_NO, PER_NO, STR_VEH, AGE, SEXNAME, PER_TYP:INJ_SEVNAME,
             HISPANIC, HISPANICNAME, RACE, RACENAME)
    
    veh <- vehicle %>%
      mutate(pursuit.driver = if_else(DR_SF1==37 | DR_SF2==37 | DR_SF3==37 | DR_SF4==37, 1, 0)) %>%
      select(ST_CASE, VEH_NO, SPEC_USE, pursuit.driver, DR_SF1:DR_SF4NAME)
    
    crash <- accident %>%
      mutate(pursuit.crash = if_else(CF1==20 | CF2==20 | CF3==20, 1, 0)) %>%
      select(ST_CASE, VE_TOTAL, VE_FORMS, PEDS, PERSONS, COUNTY, CITY, RUR_URBNAME, 
             LATITUDE, LONGITUDE=LONGITUD, pursuit.crash, CF1:FATALS)
    
    
    fars <- left_join(pers, veh, by = c("ST_CASE", "VEH_NO")) %>%
      left_join(., crash, by = "ST_CASE") %>%
      left_join(., county.codes, by = c("COUNTY" = "county.code")) %>%
      left_join(., city.codes, by = c("CITY" = "city.code")) %>%
      mutate(victim = case_when(
        PER_TYP==99 ~ "Unknown",
        PER_TYP %in% c(1,2,9) & SPEC_USE==5 ~ "Officer",
        PER_TYP == 1 & pursuit.driver==1 ~ "Subject",
        PER_TYP %in% c(2,9) & pursuit.driver==1 ~ "Passenger",
        pursuit.driver==1 | pursuit.crash==1 ~ "Bystander"
      ))
    
    
    assign(paste0("person.", i), person)
    assign(paste0("vehicle.", i), vehicle)
    assign(paste0("accident.", i), accident)
    assign(paste0("fars.", i), fars)
  
  } else if(i == 2019) {
    
    pers <- person %>%
      mutate(date = as.Date(paste0(i, "-", MONTH, "-", DAY)),
             death.date = if_else(INJ_SEV==4, as.Date(paste0(2015, "-", DEATH_MO, "-", DEATH_DA)), as.Date(NA))) %>%
      select(date, death.date, ST_CASE, VEH_NO, PER_NO, STR_VEH, AGE, SEXNAME, PER_TYP:INJ_SEVNAME,
             HISPANIC, HISPANICNAME)
    
    race <- race %>% select(ST_CASE:MULTRACE)
    
    veh <- vehicle %>%
      mutate(pursuit.driver = if_else(DR_SF1==37 | DR_SF2==37 | DR_SF3==37 | DR_SF4==37, 1, 0)) %>%
      select(ST_CASE, VEH_NO, SPEC_USE, pursuit.driver, DR_SF1:DR_SF4NAME)
    
    crash <- accident %>%
      mutate(pursuit.crash = if_else(CF1==20 | CF2==20 | CF3==20, 1, 0)) %>%
      select(ST_CASE, VE_TOTAL, VE_FORMS, PEDS, PERSONS, COUNTY, CITY, RUR_URBNAME, 
             LATITUDE, LONGITUDE=LONGITUD, pursuit.crash, CF1:FATALS)
    
    fars <- left_join(pers, race, by = c("ST_CASE", "VEH_NO", "PER_NO")) %>%
      left_join(., veh, by = c("ST_CASE", "VEH_NO")) %>%
      left_join(., crash, by = "ST_CASE") %>%
      left_join(., county.codes, by = c("COUNTY" = "county.code")) %>%
      left_join(., city.codes, by = c("CITY" = "city.code")) %>%
      mutate(victim = case_when(
        PER_TYP==99 ~ "Unknown",
        PER_TYP %in% c(1,2,9) & SPEC_USE==5 ~ "Officer",
        PER_TYP == 1 & pursuit.driver==1 ~ "Subject",
        PER_TYP %in% c(2,9) & pursuit.driver==1 ~ "Passenger",
        pursuit.driver==1 | pursuit.crash==1 ~ "Bystander"
      ))
    
    assign(paste0("person.", i), person)
    assign(paste0("race.", i), race)
    assign(paste0("vehicle.", i), vehicle) 
    assign(paste0("accident.", i), accident)
    assign(paste0("fars.", i), fars)
    
  } else if(i == 2020) {
    
    pers <- person %>%
      mutate(date = as.Date(paste0(i, "-", MONTH, "-", DAY)),
             death.date = if_else(INJ_SEV==4, as.Date(paste0(2015, "-", DEATH_MO, "-", DEATH_DA)), as.Date(NA))) %>%
      select(date, death.date, ST_CASE, VEH_NO, PER_NO, STR_VEH, AGE, SEXNAME, PER_TYP:INJ_SEVNAME,
             HISPANIC, HISPANICNAME)
    
    race <- race %>% select(ST_CASE:MULTRACE)
    
    veh <- vehicle %>%
      select(ST_CASE, VEH_NO, SPEC_USE)
    
    driver.p <- driverrf %>% 
      select(ST_CASE:DRIVERRFNAME) %>%
      mutate(pursuit.driver = if_else(grepl("Pursuing", DRIVERRFNAME), 1, 0))    
    
    crash <- accident %>%
      select(ST_CASE, VE_TOTAL, VE_FORMS, PEDS, PERSONS, COUNTY, CITY, RUR_URBNAME, 
             LATITUDE, LONGITUDE=LONGITUD, FATALS)
    crash.p <- crashrf %>%
      select(ST_CASE:CRASHRFNAME) %>%
      mutate(pursuit.crash = if_else(grepl("Pursuit", CRASHRFNAME), 1, 0))
    
    fars <- left_join(pers, race, by = c("ST_CASE", "VEH_NO", "PER_NO")) %>%
      left_join(., veh, by = c("ST_CASE", "VEH_NO")) %>%
      left_join(., driver.p, by = c("ST_CASE", "VEH_NO")) %>%
      left_join(., crash, by = "ST_CASE") %>%
      left_join(., crash.p, by = "ST_CASE") %>%
      left_join(., county.codes, by = c("COUNTY" = "county.code")) %>%
      left_join(., city.codes, by = c("CITY" = "city.code")) %>%
      mutate(victim = case_when(
        PER_TYP==99 ~ "Unknown",
        PER_TYP %in% c(1,2,9) & SPEC_USE==5 ~ "Officer",
        PER_TYP == 1 & pursuit.driver==1 ~ "Subject",
        PER_TYP %in% c(2,9) & pursuit.driver==1 ~ "Passenger",
        pursuit.driver==1 | pursuit.crash==1 ~ "Bystander"
      ))
    
    assign(paste0("person.", i), person)
    assign(paste0("race.", i), race)
    assign(paste0("vehicle.", i), vehicle) 
    assign(paste0("driverrf.", i), driverrf)
    assign(paste0("accident.", i), accident)
    assign(paste0("crashrf.", i), crashrf)
    assign(paste0("fars.", i), fars)
  }
  
  unlink(temp)
}

# To check variables across years

# names2015 <- data.frame(names = names(fars.2015))
# names2019 <- data.frame(names = names(fars.2019))
# names2020 <- data.frame(names = names(fars.2020))
# anti_join(names2015, names2019)
# anti_join(names2015, names2020)
# 
# anti_join(names2019, names2015)
# anti_join(names2020, names2015)

# Tidy up original FARS files

fars_filelist_2015 <- lapply(ls(pattern = "[^s]\\.2015"), get) # This creates a list of the objects.
fars_filelist_2015 <- setNames(fars_filelist_2015, ls(pattern = "[^s]\\.2015")) # This names the elements of the list. 

fars_filelist_2016 <- lapply(ls(pattern = "[^s]\\.2016"), get) 
fars_filelist_2016 <- setNames(fars_filelist_2016, ls(pattern = "[^s]\\.2016"))  

fars_filelist_2017 <- lapply(ls(pattern = "[^s]\\.2017"), get) 
fars_filelist_2017 <- setNames(fars_filelist_2017, ls(pattern = "[^s]\\.2017"))  

fars_filelist_2018 <- lapply(ls(pattern = "[^s]\\.2018"), get) 
fars_filelist_2018 <- setNames(fars_filelist_2018, ls(pattern = "[^s]\\.2018"))  

fars_filelist_2019 <- lapply(ls(pattern = "[^s]\\.2019"), get) 
fars_filelist_2019 <- setNames(fars_filelist_2019, ls(pattern = "[^s]\\.2019"))  

fars_filelist_2020 <- lapply(ls(pattern = "[^s]\\.2020"), get) 
fars_filelist_2020 <- setNames(fars_filelist_2020, ls(pattern = "[^s]\\.2020"))  

# Create pursuits df
pursuits.fars <- bind_rows(lapply(ls(pattern = "fars\\."), get)) %>%
  filter(pursuit.crash == 1 | pursuit.driver == 1)

# Save output
save(list = c(ls(pattern = "_20"), ls(pattern = "fars\\."), 
              "geo.codes", "pursuits.fars"),
     file = here::here("data-outputs", "FARS.rda"))


#load(here::here("data-outputs", "FARS.rda"))


       

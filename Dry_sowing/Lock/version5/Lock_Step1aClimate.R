# This file is for importing met file assigning frost days
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

### create a list of files
### create a list of headers
### put file name into df

#################################################################################

# Download Daily climate files -------------------------------------------------------

climate <- read.table("X:/Riskwi$e/met_files/LOCK_18046.sim", 
                      skip = 22, header = TRUE, sep ="")
climate <- climate [-1,]
### need to make a clm that has all the dates not just day of year and year
str(climate)
climate$rain <- as.character(climate$rain)
climate$rain <- as.double(climate$rain)

#create a date clm # note you need to be aware of the last day of downloaded met data

# Assign dates -------------------------------------------------
download_date <- read_csv("X:/Riskwi$e/met_files/LOCK_18046.sim",
                          col_names = FALSE, skip = 7)
download_date <-download_date[1,1] #just the row with the download date
download_date <-stringr::str_extract(download_date, "[[:digit:]]+") #just the numbers
download_date <- as.Date(as.character(download_date),format="%Y%m%d")

#minus one day from download
download_date <- lubridate::ymd(download_date) - days(1)
str(download_date)

download_date <-"20250403" # "2025/04/03"
download_date <- as.Date(as.character(download_date),format="%Y%m%d")
download_date



climate <- climate %>% 
  mutate(date = seq(as.Date("1900/1/1"), download_date, "days"))
# set date as a date
climate <- climate %>% 
  mutate(year = year(date),
         month =month(date),
         day = day(date),
         day_of_month = day(date),
         month_name = lubridate::month(date, label = TRUE),
         site = paste0("Lock","_", 018046))
str(climate)


### Frost days  ################################################################
climate$maxt <- as.numeric(climate$maxt)
climate$mint <- as.numeric(climate$mint)

str(climate)



Fost_details <- climate %>% 
  #select(site, date, year,maxt, mint) %>% 
  mutate(frost_event = 
    case_when(
      mint<1 ~ "frost",
      .default = "non_frost"
    ))
          
### apply a Frost risk period with the  

## Define the frost risk (FR) period and assign season type ---------------------------------

Day_start_FR <- 1
Month_start_FR <- 8 # Aug, it was 4 April


Day_end_FR <- 1
Month_end_FR <- 10 # Nov, it was 11 Oct

#File start date and end date
paste("Start date in file is: ",
      min(Fost_details$date),
      ". End date in file is: ",
      max(Fost_details$date))

FR_defined_as <- paste0(
  Day_start_FR, "/", Month_start_FR,
  " to ", Day_end_FR, "/", Month_end_FR)

str(Fost_details)

# Assign season type to climate data

Fost_details <- Fost_details %>% mutate(
  start_end_FR_date = case_when(
    month == Month_start_FR & day_of_month == Day_start_FR ~ "start_FR",
    month == Month_end_FR & day_of_month == Day_end_FR ~ "end_FR",
    TRUE ~ NA))

# Fill the blanks

Fost_details <- Fost_details %>% fill(start_end_FR_date) %>%
  mutate(
    season = case_when(
      start_end_FR_date == "start_FR" ~ "FR",
      TRUE ~ "outside_FR"))

Fost_details <- Fost_details %>% select(-start_end_FR_date)

Fost_details <- Fost_details %>%
  mutate(FR_definition = FR_defined_as)


Fost_details_only_FR_data <- Fost_details %>% filter(season == "FR")




write.csv(Fost_details ,
          "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Fost_details_18046.csv", row.names = FALSE )


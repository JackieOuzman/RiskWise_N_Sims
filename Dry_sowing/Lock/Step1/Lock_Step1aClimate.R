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

climate <- read.table("X:/Riskwi$e/met_files/KYANCUTTA_18170.sim", 
                      skip = 21, header = TRUE, sep ="")
climate <- climate [-1,]
### need to make a clm that has all the dates not just day of year and year
str(climate)
climate$rain <- as.character(climate$rain)
climate$rain <- as.double(climate$rain)

#create a date clm # note you need to be aware of the last day of downloaded met data

# Assign dates -------------------------------------------------
download_date <- read_csv("X:/Riskwi$e/met_files/KYANCUTTA_18170.sim",
                          col_names = FALSE, skip = 7)
download_date <-download_date[1,1] #just the row with the download date
download_date <-stringr::str_extract(download_date, "[[:digit:]]+") #just the numbers
download_date <- as.Date(as.character(download_date),format="%Y%m%d")

#minus one day from download
download_date <- lubridate::ymd(download_date) - days(1)
str(download_date)

download_date <-"20250416" # "2025/04/03"
download_date <- as.Date(as.character(download_date),format="%Y%m%d")
download_date



climate <- climate %>% 
  mutate(date = seq(as.Date("1957/1/1"), download_date, "days"))
# set date as a date
climate <- climate %>% 
  mutate(year = year(date),
         month =month(date),
         day = day(date),
         month_name = lubridate::month(date, label = TRUE),
         site = paste0("KYANCUTTA","_", 18170))
str(climate)


### Frost days  ################################################################
climate$maxt <- as.numeric(climate$maxt)
climate$mint <- as.numeric(climate$mint)

str(climate)
Fost_details <- climate %>% select(site, date, year,maxt, mint) %>% 
  mutate(frost_event = 
    case_when(
      mint<1 ~ "frost",
      .default = "non_frost"
    ))
          
write.csv(Fost_details ,
          "X:/Riskwi$e/Dry_sowing/Kyancutta/Results/Fost_details_18170.csv", row.names = FALSE )
                          
       
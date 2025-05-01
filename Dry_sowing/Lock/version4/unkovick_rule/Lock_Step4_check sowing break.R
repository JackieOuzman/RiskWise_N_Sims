
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# Download Daily climate files -------------------------------------------------------

climate <- read.table("X:/Riskwi$e/met_files/LOCK_18046.sim", 
                      skip = 22, header = TRUE, sep ="")
climate <- climate [-1,]
### need to make a clm that has all the dates not just day of year and year
str(climate)
climate$rain <- as.character(climate$rain)
climate$rain <- as.double(climate$rain)

#create a date clm # note you need to be aware of the last day of downloaded met data

## Assign dates, site detial and format -------------------------------------------------
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
         day_of_month = day(date),
         month_name = lubridate::month(date, label = TRUE),
         site = paste0("Lock","_", 018046))
str(climate)

climate$maxt <- as.numeric(climate$maxt)
climate$mint <- as.numeric(climate$mint)



## Define the GS period and assign season type ---------------------------------

Day_start_GS_rainfall <- 1
Month_start_GS_rainfall <- 4


Day_end_GS_rainfall <- 1
Month_end_GS_rainfall <- 11

#File start date and end date
paste("Start date in file is: ",
      min(climate$date),
      ". End date in file is: ",
      max(climate$date))

GS_defined_as <- paste0(
  Day_start_GS_rainfall, "/", Month_start_GS_rainfall,
  " to ", Day_end_GS_rainfall, "/", Month_end_GS_rainfall)

str(climate)

# Assign season type to climate data

climate <- climate %>% mutate(
  start_end_GS_date = case_when(
    month == Month_start_GS_rainfall & day_of_month == Day_start_GS_rainfall ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))

# Fill the blanks

climate <- climate %>% fill(start_end_GS_date) %>%
  mutate(
    season = case_when(
      start_end_GS_date == "start_gs" ~ "gs",
      TRUE ~ "summer"))

climate <- climate %>% select(-start_end_GS_date)


## Write out neat climate file ------------------------------------------------
write.csv(climate ,
          "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/Results/Check_season_break/NeatClimate_18046.csv", row.names = FALSE )


climate2022_23_24 <- climate %>% 
  filter(year %in% c(2022, 2023, 2024))
climate2015_to_2024 <- climate %>% 
  filter(year %in% c(2015,2016,2017,2018,2019, 2020, 2021, 2022, 2023, 2024))

write.csv(climate2022_23_24 ,
          "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/Results/Check_season_break/NeatClimate_18046_yrs2022_23_24.csv", row.names = FALSE )

write.csv(climate2015_to_2024 ,
          "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/Results/Check_season_break/NeatClimate_18046_yrs2015_to_2024.csv", row.names = FALSE )

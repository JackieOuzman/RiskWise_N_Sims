# This file is for importing met file assigning frost days
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

Est_sowing_conditions_v1 <- read_excel("X:/Riskwi$e/Dry_sowing/Lock/Est_soil_starting_conditions/Est_sowing_conditions_v1.xlsx", 
                                       col_types = c("text", "numeric", "text", 
                                                     "numeric", "text", "date", "text", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric"))



### sum of rainfall from '10Nov' to sowing date

names(Est_sowing_conditions_v1)
str(Est_sowing_conditions_v1)

Est_sowing_conditions_v1 <- Est_sowing_conditions_v1 %>%
  mutate(
    date = Clock.Today ,
    year = year(date),
    month = month(date),
    day = day(date),
    day_of_month = day(date),
    month_name = lubridate::month(date, label = TRUE),
    site = paste0("Lock", "_", 018046)
  )



## Define the GS period and assign season type ---------------------------------

Day_start_GS_rainfall <- 1
Month_start_GS_rainfall <- 4


Day_end_GS_rainfall <- 1
Month_end_GS_rainfall <- 11

#File start date and end date
paste("Start date in file is: ",
      min(Est_sowing_conditions_v1$date),
      ". End date in file is: ",
      max(Est_sowing_conditions_v1$date))

GS_defined_as <- paste0(
  Day_start_GS_rainfall, "/", Month_start_GS_rainfall,
  " to ", Day_end_GS_rainfall, "/", Month_end_GS_rainfall)

str(Est_sowing_conditions_v1)

# Assign season type to climate data

Est_sowing_conditions_v1 <- Est_sowing_conditions_v1 %>% mutate(
  start_end_GS_date = case_when(
    month == Month_start_GS_rainfall & day_of_month == Day_start_GS_rainfall ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))

# Fill the blanks

Est_sowing_conditions_v1 <- Est_sowing_conditions_v1 %>% fill(start_end_GS_date) %>%
  mutate(
    season = case_when(
      start_end_GS_date == "start_gs" ~ "gs",
      TRUE ~ "summer"))

Est_sowing_conditions_v1 <- Est_sowing_conditions_v1 %>% select(-start_end_GS_date)
str(Est_sowing_conditions_v1)

### what are the sowing dates for the 15 years
sow_dates_by_year <- Est_sowing_conditions_v1 %>% 
filter (Wheat.Phenology.CurrentStageName == "Sowing") %>% 
  select(year, date,  Wheat.Phenology.CurrentStageName,  SumESW,Sum_MM )

list_sow_dates <- sow_dates_by_year[,2]


summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1960-11-01'), 
                 as.Date('1961-04-01'))) #small to big dates
summer_rain1961 <- sum(summer_rain$Weather.Rain, na.rm = TRUE)  

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1961-11-01'), 
                 as.Date('1962-04-01'))) #small to big dates
summer_rain1962 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1962-11-01'), 
                 as.Date('1963-04-01'))) #small to big dates
summer_rain1963 <- sum(summer_rain$Weather.Rain, na.rm = TRUE)

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1963-11-01'), 
                 as.Date('1964-04-01'))) #small to big dates
summer_rain1964 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1964-11-01'), 
                 as.Date('1965-04-01'))) #small to big dates
summer_rain1965 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1965-11-01'), 
                 as.Date('1966-04-01'))) #small to big dates
summer_rain1966 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1966-11-01'), 
                 as.Date('1967-04-01'))) #small to big dates
summer_rain1967 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1967-11-01'), 
                 as.Date('1968-04-01'))) #small to big dates
summer_rain1968 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1968-11-01'), 
                 as.Date('1969-04-01'))) #small to big dates
summer_rain1969 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1969-11-01'), 
                 as.Date('1970-04-01'))) #small to big dates
summer_rain1970 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

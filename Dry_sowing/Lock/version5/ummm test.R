library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)


Lock_climate2015_2024 <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/NeatClimate_18046_yrs2014_to_2024.csv")
str(Lock_climate2015_2024)

# Number of days for averaging  ------------------------------------------------
###ie the last 7 days or 5 days
days_for_ave <- 7

# Run as a loop for years -----------------------------------------------------

year_analysis <- c("2014", "2015","2016","2017","2018","2019", "2020", "2021", "2022", "2023", "2024")
Lock_climate_yr <- Lock_climate2015_2024 %>% filter(year == as.double(year_analysis))



Lock_climate_yr <- Lock_climate_yr %>% 
  mutate(sum_rain_7_days = rollsumr(rain, k =days_for_ave, fill= NA),
         sum_evap_7_days = rollsumr(evap, k =days_for_ave, fill= NA),
         
         sum_rain_4_days = rollsumr(rain, k =4, fill= NA),
         sum_evap_4_days = rollsumr(evap, k =4, fill= NA),
         
         sum_rain_2_days = rollsumr(rain, k =2, fill= NA),
         sum_evap_2_days = rollsumr(evap, k =2, fill= NA),
         
         rainfall_exceeds_evaporation_7days = sum_rain_7_days -sum_evap_7_days,
         rainfall_exceeds_evaporation_4days = sum_rain_4_days -sum_evap_4_days,
         rainfall_exceeds_evaporation_2days = sum_rain_2_days -sum_evap_2_days)

# Did the conditions meet unkovich rule ----------------------------------------
Lock_climate_yr <- Lock_climate_yr %>% 
  mutate(Threshold_7day_0 = case_when(rainfall_exceeds_evaporation_7days >0 ~ "Sowing_break")) %>% 
  
  mutate(Threshold_4day_0 = case_when(rainfall_exceeds_evaporation_4days >0 ~ "Sowing_break")) %>% 
  mutate(Threshold_2day_0 = case_when(rainfall_exceeds_evaporation_2days >0 ~ "Sowing_break")) %>% 
  
  mutate(Threshold_4day_5 = case_when(rainfall_exceeds_evaporation_4days >5 ~ "Sowing_break")) %>% 
  mutate(Threshold_2day_5 = case_when(rainfall_exceeds_evaporation_2days >5 ~ "Sowing_break")) %>% 
  
  mutate(Threshold_4day_10 = case_when(rainfall_exceeds_evaporation_4days >10 ~ "Sowing_break")) %>% 
  mutate(Threshold_2day_10 = case_when(rainfall_exceeds_evaporation_2days >10 ~ "Sowing_break"))  
  
  
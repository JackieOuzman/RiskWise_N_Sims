
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)


Lock_climate2015_2024 <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/Results/Check_season_break/NeatClimate_18046_yrs2015_to_2024.csv")
str(Lock_climate2015_2024)

# Number of days for averaging  --------------------
###ie the last 7 days or 5 days
days_for_ave <- 7

################################################################################
## per year  --------------------
year_analysis <- "2022"


Lock_climate_yr <- Lock_climate2015_2024 %>% filter(year == as.double(year_analysis))

# Remove data outside end of window ------------------------------------------- 

Lock_climate_yr <- Lock_climate_yr %>% 
  filter(date <= as.Date(paste0(year , "-05-30")))

# average rain and evap for specified days -------------------------------------
Lock_climate_yr <- Lock_climate_yr %>% 
  mutate(sum_rain = rollsumr(rain, k =days_for_ave, fill= NA),
         sum_evap = rollsumr(evap, k =days_for_ave, fill= NA),
         rainfall_exceeds_evaporation = sum_rain -sum_evap)

# Did the conditions meet unkovich rule ----------------------------------------
Lock_climate_yr <- Lock_climate_yr %>% 
  mutate(Unkovich_conditions = case_when(
    rainfall_exceeds_evaporation >0 ~ "Sowing_break"
  ) )
         

################################################################################
## Quick check to see if it matches what I did in excel
## so far so good
slice(Lock_climate_yr, -(1:20)) #igrone the first few clm
Lock_climate_yr[21:30, c(10,6,7,16:19)] #just checking the clms I want to see. seems to match my excels example
################################################################################

## dry sowing date 4 dates in April(4) and 4 in May(5)
dry_sowing_month <- "04"
dry_sowing_day_of_month <- "01" #5,10,15,20,25

Lock_climate_yr_window <- Lock_climate_yr %>% 
  filter(date >= as.Date(paste0(year , "-",dry_sowing_month, "-", dry_sowing_day_of_month)))

## keep the all "Sowing_break"

Lock_climate_yr_window_Unk <- Lock_climate_yr_window %>% 
  filter(Unkovich_conditions == "Sowing_break")

Lock_climate_yr_window_Unk %>% 
  group_by( ) %>% 
  slice_min(date)

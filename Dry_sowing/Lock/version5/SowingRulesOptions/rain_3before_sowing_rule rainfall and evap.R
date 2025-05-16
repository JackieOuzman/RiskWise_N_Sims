library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)



Lock_Rain_Evap_Rule <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Lock_rain_Ev_rule_R_cals.csv")
str(Lock_Rain_Evap_Rule)

# 7 days ------------------------------------------------------------------

### split data in 7,4,2 day group and sum rainfall 3 days before

rainfall_exceeds_evaporation_7days <- Lock_Rain_Evap_Rule %>% 
  select(year, date, month, month_name, Threshold_7day_0, rain) 
  
### sum the rain 3 days before date

rainfall_exceeds_evaporation_7days <- rainfall_exceeds_evaporation_7days %>% 
  mutate(sum_rain_3_days = rollsumr(rain, k =3, fill= NA))

rainfall_exceeds_evaporation_7days_Spring <- rainfall_exceeds_evaporation_7days %>% 
  select(year, date, month, month_name,rain, Threshold_7day_0, sum_rain_3_days ) %>% 
  filter(month_name %in% c( "Apr" , "May" , "Jun" , "Jul")) %>% 
  filter(!is.na(Threshold_7day_0))
rainfall_exceeds_evaporation_7days_Spring



### keep the first occurrence by year - for each year in the spring period when did the first sowing break occur
first_occurance_Rain_Evap7Days <- rainfall_exceeds_evaporation_7days_Spring %>%
  group_by(year) %>%
  arrange(date) %>%
  filter(row_number()==1) 

first_occurance_Rain_Evap7Days$date <- as.POSIXct.Date(first_occurance_Rain_Evap7Days$date)
first_occurance_Rain_Evap7Days <- first_occurance_Rain_Evap7Days %>%  mutate(DOY = lubridate::yday(date))



ggplot(first_occurance_Rain_Evap7Days, aes(x = date, y =sum_rain_3_days )) + 
  geom_point()+
  theme_bw() +
  labs(
    color = NULL, #removes the legend title
    title = "Rainfall and evaporation rule. ",
    subtitle = "Sum of 7 days of rainfall and evaporation to define optimal conditions",
    x = "Date",
    y = "Sum rainfall 3 days before optimal sowing conditions",
    caption = "Data is: DOY per year that first occurance of optimal conditions has occured.
    This can be misleading the trigger is for 7 days but only showing 3"
  )


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files
ggsave(
  filename = paste0(path_saved_files,"/sum 3 days rain before trigger rule rain evap 7 days", ".png" ),
  width = 20, height = 12, units = "cm")



# 4 days ------------------------------------------------------------------

### split data in 7,4,2 day group and sum rainfall 3 days before

rainfall_exceeds_evaporation_4days <- Lock_Rain_Evap_Rule %>% 
  select(year, date, month, month_name, Threshold_4day_0, rain) 

### sum the rain 3 days before date

rainfall_exceeds_evaporation_4days <- rainfall_exceeds_evaporation_4days %>% 
  mutate(sum_rain_3_days = rollsumr(rain, k =3, fill= NA))

rainfall_exceeds_evaporation_4days_Spring <- rainfall_exceeds_evaporation_4days %>% 
  select(year, date, month, month_name,rain, Threshold_4day_0, sum_rain_3_days ) %>% 
  filter(month_name %in% c( "Apr" , "May" , "Jun" , "Jul")) %>% 
  filter(!is.na(Threshold_4day_0))
rainfall_exceeds_evaporation_4days_Spring



### keep the first occurrence by year - for each year in the spring period when did the first sowing break occur
first_occurance_Rain_Evap4Days <- rainfall_exceeds_evaporation_4days_Spring %>%
  group_by(year) %>%
  arrange(date) %>%
  filter(row_number()==1) 

first_occurance_Rain_Evap4Days$date <- as.POSIXct.Date(first_occurance_Rain_Evap4Days$date)
first_occurance_Rain_Evap4Days <- first_occurance_Rain_Evap4Days %>%  mutate(DOY = lubridate::yday(date))



ggplot(first_occurance_Rain_Evap4Days, aes(x = date, y =sum_rain_3_days )) + 
  geom_point()+
  theme_bw() +
  labs(
    color = NULL, #removes the legend title
    title = "Rainfall and evaporation rule. ",
    subtitle = "Sum of 4 days of rainfall and evaporation to define optimal conditions",
    x = "Date",
    y = "Sum rainfall 3 days before optimal sowing conditions",
    caption = "Data is: DOY per year that first occurance of optimal conditions has occured.
    This can be misleading the trigger is for 4 days but only showing 3"
  )


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files
ggsave(
  filename = paste0(path_saved_files,"/sum 3 days rain before trigger rule rain evap 4 days", ".png" ),
  width = 20, height = 12, units = "cm")



# 2 days ------------------------------------------------------------------

### split data in 7,4,2 day group and sum rainfall 3 days before

rainfall_exceeds_evaporation_2days <- Lock_Rain_Evap_Rule %>% 
  select(year, date, month, month_name, Threshold_2day_0, rain) 

### sum the rain 3 days before date

rainfall_exceeds_evaporation_2days <- rainfall_exceeds_evaporation_2days %>% 
  mutate(sum_rain_3_days = rollsumr(rain, k =3, fill= NA))

rainfall_exceeds_evaporation_2days_Spring <- rainfall_exceeds_evaporation_2days %>% 
  select(year, date, month, month_name,rain, Threshold_2day_0, sum_rain_3_days ) %>% 
  filter(month_name %in% c( "Apr" , "May" , "Jun" , "Jul")) %>% 
  filter(!is.na(Threshold_2day_0))
rainfall_exceeds_evaporation_2days_Spring



### keep the first occurrence by year - for each year in the spring period when did the first sowing break occur
first_occurance_Rain_Evap2Days <- rainfall_exceeds_evaporation_2days_Spring %>%
  group_by(year) %>%
  arrange(date) %>%
  filter(row_number()==1) 

first_occurance_Rain_Evap2Days$date <- as.POSIXct.Date(first_occurance_Rain_Evap2Days$date)
first_occurance_Rain_Evap2Days <- first_occurance_Rain_Evap2Days %>%  mutate(DOY = lubridate::yday(date))



ggplot(first_occurance_Rain_Evap2Days, aes(x = date, y =sum_rain_3_days )) + 
  geom_point()+
  theme_bw() +
  labs(
    color = NULL, #removes the legend title
    title = "Rainfall and evaporation rule. ",
    subtitle = "Sum of 2 days of rainfall and evaporation to define optimal conditions",
    x = "Date",
    y = "Sum rainfall 3 days before optimal sowing conditions",
    caption = "Data is: DOY per year that first occurance of optimal conditions has occured.
    This can be misleading the trigger is for 2 days but only showing 3"
  )


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files
ggsave(
  filename = paste0(path_saved_files,"/sum 3 days rain before trigger rule rain evap 2 days", ".png" ),
  width = 20, height = 12, units = "cm")

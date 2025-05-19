library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)



Lock_water_balance_Rule <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Lock_water_balance_rule_R_cals.csv")
str(Lock_water_balance_Rule)

# 10mm ------------------------------------------------------------------

### split data in 10mm and 5 mm group and sum rainfall 3 days before

Lock_water_balance_Rule10mm <- Lock_water_balance_Rule %>% 
  select(year, date, month, month_name, Threshold_WB_10mm, rain) 
  
### sum the rain 3 days before date

Lock_water_balance_Rule10mm <- Lock_water_balance_Rule10mm %>% 
  mutate(sum_rain_3_days = rollsumr(rain, k =3, fill= NA))

Lock_water_balance_Rule10mm_Spring <- Lock_water_balance_Rule10mm %>% 
  select(year, date, month, month_name,rain, Threshold_WB_10mm, sum_rain_3_days ) %>% 
  filter(month_name %in% c( "Apr" , "May" , "Jun" , "Jul")) %>% 
  filter(!is.na(Threshold_WB_10mm))
Lock_water_balance_Rule10mm_Spring



### keep the first occurrence by year - for each year in the spring period when did the first sowing break occur
first_occurance_Rain_10mm <- Lock_water_balance_Rule10mm_Spring %>%
  group_by(year) %>%
  arrange(date) %>%
  filter(row_number()==1) 

first_occurance_Rain_10mm$date <- as.POSIXct.Date(first_occurance_Rain_10mm$date)
first_occurance_Rain_10mm <- first_occurance_Rain_10mm %>%  mutate(DOY = lubridate::yday(date))



ggplot(first_occurance_Rain_10mm, aes(x = date, y =sum_rain_3_days )) + 
  geom_point()+
  theme_bw() +
  labs(
    color = NULL, #removes the legend title
    title = "Water balance rule. ",
    subtitle = "Water balance thershold of 10mm to define optimal conditions",
    x = "Date",
    y = "Sum rainfall 3 days before optimal sowing conditions",
    caption = "Data is: DOY per year that first occurance of optimal conditions has occured.
    This can be misleading the trigger is a water balance not just rainfall"
  )


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files
ggsave(
  filename = paste0(path_saved_files,"/sum 3 days rain before trigger rule water balance 10mm", ".png" ),
  width = 20, height = 12, units = "cm")



# 5mm ------------------------------------------------------------------

### split data in 10, 5mm rule group and sum rainfall 3 days before



Lock_water_balance_Rule5mm <- Lock_water_balance_Rule %>% 
  select(year, date, month, month_name, Threshold_WB_5mm, rain) 

### sum the rain 3 days before date

Lock_water_balance_Rule5mm <- Lock_water_balance_Rule5mm %>% 
  mutate(sum_rain_3_days = rollsumr(rain, k =3, fill= NA))

Lock_water_balance_Rule5mm_Spring <- Lock_water_balance_Rule5mm %>% 
  select(year, date, month, month_name,rain, Threshold_WB_5mm, sum_rain_3_days ) %>% 
  filter(month_name %in% c( "Apr" , "May" , "Jun" , "Jul")) %>% 
  filter(!is.na(Threshold_WB_5mm))
Lock_water_balance_Rule5mm_Spring



### keep the first occurrence by year - for each year in the spring period when did the first sowing break occur
first_occurance_Rain_5mm <- Lock_water_balance_Rule5mm_Spring %>%
  group_by(year) %>%
  arrange(date) %>%
  filter(row_number()==1) 

first_occurance_Rain_5mm$date <- as.POSIXct.Date(first_occurance_Rain_5mm$date)
first_occurance_Rain_5mm <- first_occurance_Rain_5mm %>%  mutate(DOY = lubridate::yday(date))



ggplot(first_occurance_Rain_5mm, aes(x = date, y =sum_rain_3_days )) + 
  geom_point()+
  theme_bw() +
  labs(
    color = NULL, #removes the legend title
    title = "Water balance rule. ",
    subtitle = "Water balance thershold of 5 mm to define optimal conditions",
    x = "Date",
    y = "Sum rainfall 3 days before optimal sowing conditions",
    caption = "Data is: DOY per year that first occurance of optimal conditions has occured.
    This can be misleading the trigger is water balance not just rainfall"
  )


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files
ggsave(
  filename = paste0(path_saved_files,"/sum 3 days rain before trigger rule water balance 5mm", ".png" ),
  width = 20, height = 12, units = "cm")








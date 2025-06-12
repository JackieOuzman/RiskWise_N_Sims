#More plots



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

### keep only the spring data and results of sowing break when 7 days of rain is more than 7 days of evap

rainfall_exceeds_evaporation_7days_Spring <- Lock_Rain_Evap_Rule %>% 
  select(year, date, month, month_name, Threshold_7day_0) %>% 
  filter(month_name %in% c("Apr" , "May" , "Jun" , "Jul")) %>% 
  filter(!is.na(Threshold_7day_0))
rainfall_exceeds_evaporation_7days_Spring

### keep the first occurance by year - for each year in the spring period when did the first sowing break occur
first_occurance_Rain_Evap7Days <- rainfall_exceeds_evaporation_7days_Spring %>%
  group_by(year) %>%
  arrange(date) %>%
  filter(row_number()==1) 

first_occurance_Rain_Evap7Days$date <- as.POSIXct.Date(first_occurance_Rain_Evap7Days$date)
first_occurance_Rain_Evap7Days <- first_occurance_Rain_Evap7Days %>%  mutate(DOY = lubridate::yday(date))


# 4 days ------------------------------------------------------------------
### keep only the spring data and results of sowing break when 4 days of rain is more than 4 days of evap

rainfall_exceeds_evaporation_4days_Spring <- Lock_Rain_Evap_Rule %>% 
  select(year, date, month, month_name, Threshold_4day_0) %>% 
  filter(month_name %in% c("Apr" , "May" , "Jun" , "Jul")) %>% 
  filter(!is.na(Threshold_4day_0))
rainfall_exceeds_evaporation_4days_Spring

### keep the first occurance by year - for each year in the spring period when did the first sowing break occur
first_occurance_Rain_Evap4Days <- rainfall_exceeds_evaporation_4days_Spring %>%
  group_by(year) %>%
  arrange(date) %>%
  filter(row_number()==1) 

first_occurance_Rain_Evap4Days$date <- as.POSIXct.Date(first_occurance_Rain_Evap4Days$date)
first_occurance_Rain_Evap4Days <- first_occurance_Rain_Evap4Days %>%  mutate(DOY = lubridate::yday(date))




# 2 days ------------------------------------------------------------------
### keep only the spring data and results of sowing break when 2 days of rain is more than 2 days of evap

rainfall_exceeds_evaporation_2days_Spring <- Lock_Rain_Evap_Rule %>% 
  select(year, date, month, month_name, Threshold_2day_0) %>% 
  filter(month_name %in% c("Apr" , "May" , "Jun" , "Jul")) %>% 
  filter(!is.na(Threshold_2day_0))
rainfall_exceeds_evaporation_2days_Spring

### keep the first occurance by year - for each year in the spring period when did the first sowing break occur
first_occurance_Rain_Evap2Days <- rainfall_exceeds_evaporation_2days_Spring %>%
  group_by(year) %>%
  arrange(date) %>%
  filter(row_number()==1) 

first_occurance_Rain_Evap2Days$date <- as.POSIXct.Date(first_occurance_Rain_Evap2Days$date)
first_occurance_Rain_Evap2Days <- first_occurance_Rain_Evap2Days %>%  mutate(DOY = lubridate::yday(date))




# 7,4, 2 days in one file ------------------------------------------------------------------

ungroup(first_occurance_Rain_Evap2Days)
ungroup(first_occurance_Rain_Evap4Days)
ungroup(first_occurance_Rain_Evap7Days)

names(first_occurance_Rain_Evap2Days)
names(first_occurance_Rain_Evap4Days)
names(first_occurance_Rain_Evap7Days)

first_occurance_Rain_Evap2Days <- first_occurance_Rain_Evap2Days %>% 
  mutate(Optimal_sowing_date = "Optimal_sowing2days") %>% 
  select(-"Threshold_2day_0")

first_occurance_Rain_Evap4Days <- first_occurance_Rain_Evap4Days %>% 
  mutate(Optimal_sowing_date = "Optimal_sowing4days") %>% 
  select(-"Threshold_4day_0")

first_occurance_Rain_Evap7Days <- first_occurance_Rain_Evap7Days %>% 
  mutate(Optimal_sowing_date = "Optimal_sowing7days") %>% 
  select(-"Threshold_7day_0")


first_occurance_Rain_Evap7_4_2Days <- rbind(first_occurance_Rain_Evap2Days, first_occurance_Rain_Evap4Days, first_occurance_Rain_Evap7Days)
first_occurance_Rain_Evap7_4_2Days

### PLOT
ggplot(first_occurance_Rain_Evap7_4_2Days, aes(x = DOY)) + 
  stat_ecdf(aes(color = Optimal_sowing_date),
    geom = "point") +
  theme_bw() +
  labs(
    color = NULL, #removes the legend title
    title = "Rainfall and evaporation rule. ",
    subtitle = "Sum of x days of rainfall and evaporation to define optimal conditions",
    x = "Day of year ",
    y = "Probability",
    caption = "Data is: DOY per year that first occurance of optimal conditions has occured"
  )

path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files
ggsave(
       filename = paste0(path_saved_files,"/rainfall_evap_sowing_rule_cdf", ".png" ),
       width = 20, height = 12, units = "cm")



###  cdf input for plots
path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
write_csv(first_occurance_Rain_Evap7_4_2Days, 
          paste0(path_saved_files, "/Lock_rain_Ev_rule_R_cdf_inputs.csv") )


first_occurance_Rain_Evap7_4_2Days

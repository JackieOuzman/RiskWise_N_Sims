library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)


Lock_climate <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Fost_details_18046.csv")
str(Lock_climate)



# year_analysis <- c(2014, 2015,2016,2017,2018,2019, 2020, 2021, 2022, 2023, 2024)
# Lock_climate2015_2024 <- Lock_climate %>% filter(year %in%  year_analysis)


Lock_climate <- Lock_climate %>% 
  mutate(sum_rain_3_days = rollsumr(rain, k =3, fill= NA))
         
         
Lock_climate <- Lock_climate %>% 
  mutate(Threshold_3day_15 = case_when(sum_rain_3_days >=15 ~ "Sowing_break")) 

Lock_climate
Lock_climate_yr_long_spring <- Lock_climate %>%
  select(
    month_name,
    day_of_month,
    year,
    date,
    sum_rain_3_days,
    Threshold_3day_15
  ) %>%
  filter(month_name %in% c("Apr" , "May" , "Jun" , "Jul"))

Rain_sum_rule <- Lock_climate_yr_long_spring %>% 
  ggplot(aes(x = date, sum_rain_3_days ))+
  geom_point()+
  theme_bw()+
  facet_wrap(vars(year), scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="bottom")+
  labs(title = "Sum rain 3 days",
       subtitle = "",
       x = "",
       y = "Sum of evaopration 3 days "
  )
Rain_sum_rule

Rain_sum_rule_2023_2024 <- Lock_climate_yr_long_spring %>% 
  filter(year %in% c(2023, 2024)) %>% 
  ggplot(aes(x = date, sum_rain_3_days ))+
  geom_point()+
  theme_bw()+
  facet_wrap(vars(year), scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="bottom")+
  labs(title = "Sum rain 3 days",
       subtitle = "",
       x = "",
       y = "Sum of evaopration 3 days "
  )
Rain_sum_rule_2023_2024


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files
# ggsave(plot = Rain_sum_rule,
#        filename = paste0(path_saved_files,"/Rain_sum_rule_2014_2024", ".png" ),
#        width = 20, height = 12, units = "cm")
# 
# ggsave(plot = Rain_sum_rule_2023_2024,
#        filename = paste0(path_saved_files,"/Rain_sum_rule_2023_2024", ".png" ),
#        width = 20, height = 12, units = "cm")




## but are these really different for example on the 
Lock_climate_yr_long_spring

Lock_climate_yr_long_spring <- Lock_climate_yr_long_spring %>% 
  mutate(optimal_sowing_3days = case_when(
    Threshold_3day_15 == "Sowing_break" ~ "40",
    .default = Threshold_3day_15
  ))
Lock_climate_yr_long_spring  
sowing_rules3 <- Lock_climate_yr_long_spring %>%  filter(!is.na(Threshold_3day_15)) %>% select(date, year, optimal_sowing_3days)
sowing_rules3

Lock_climate_yr_long_spring

Rain_sum_rule_2023_2024_3days <- Lock_climate_yr_long_spring %>% 
  filter(year %in% c(2023, 2024)) %>% 
  ggplot(aes(x = date, sum_rain_3_days ))+
  geom_line()+
  theme_bw()+
  facet_wrap(vars(year), scales = "free_x")+
  #scale_y_continuous(limits = c(0, 40))+
  geom_vline(data = filter(sowing_rules3, year %in% c(2023, 2024)), 
             aes(xintercept = date),  color = "grey")+
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
        legend.position="none"
  )+
  labs(title = "Sum rain 3 days 15mm or more rule",
       subtitle = "Optimal sowing rule",
       x = "",
       y = "sum of rain"
  )
Rain_sum_rule_2023_2024_3days


Rain_sum_rule_3days <- Lock_climate_yr_long_spring %>% 
  ggplot(aes(x = date, sum_rain_3_days ))+
  geom_line()+
  theme_bw()+
  facet_wrap(vars(year), scales = "free_x")+
  #scale_y_continuous(limits = c(0, 40))+
  geom_vline(data = sowing_rules3, 
             aes(xintercept = date),  color = "grey")+
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
        legend.position="none"
  )+
  labs(title = "Sum rain 3 days 15mm or more rule",
       subtitle = "Optimal sowing rule",
       x = "",
       y = "sum of rain"
  )
Rain_sum_rule_3days


# ggsave(plot = Rain_sum_rule_3days,
#        filename = paste0(path_saved_files,"/Rain_sum_rule_optimal_sowing_2014_2024", ".png" ),
#        width = 20, height = 12, units = "cm")

ggsave(plot = Rain_sum_rule_2023_2024_3days,
       filename = paste0(path_saved_files,"/Rain_sum_rule_optimal_sowing2023_2024", ".png" ),
       width = 20, height = 12, units = "cm")

write_csv(Lock_climate, paste0(path_saved_files, "/Lock_sum_rain_3days_15mm_rule_R_cals.csv") )

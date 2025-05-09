library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)


Lock_climate <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Fost_details_18046.csv")
str(Lock_climate)


year_analysis <- c(2014, 2015,2016,2017,2018,2019, 2020, 2021, 2022, 2023, 2024)
Lock_climate_yr <- Lock_climate %>% filter(year %in%  year_analysis)


Lock_climate_yr
# Did the conditions meet unkovich rule ----------------------------------------

Lock_climate_yr <- Lock_climate_yr %>% 
  mutate(sum_rain_7_days = rollsumr(rain, k =7, fill= NA),
         sum_evap_7_days = rollsumr(evap, k =7, fill= NA),
         
         sum_rain_4_days = rollsumr(rain, k =4, fill= NA),
         sum_evap_4_days = rollsumr(evap, k =4, fill= NA),
         
         sum_rain_2_days = rollsumr(rain, k =2, fill= NA),
         sum_evap_2_days = rollsumr(evap, k =2, fill= NA),
         
         rainfall_exceeds_evaporation_7days = sum_rain_7_days -sum_evap_7_days,
         rainfall_exceeds_evaporation_4days = sum_rain_4_days -sum_evap_4_days,
         rainfall_exceeds_evaporation_2days = sum_rain_2_days -sum_evap_2_days)

Lock_climate_yr <- Lock_climate_yr %>% 
  mutate(Threshold_7day_0 = case_when(rainfall_exceeds_evaporation_7days >0 ~ "Sowing_break")) %>% 
  
  mutate(Threshold_4day_0 = case_when(rainfall_exceeds_evaporation_4days >0 ~ "Sowing_break")) %>% 
  mutate(Threshold_2day_0 = case_when(rainfall_exceeds_evaporation_2days >0 ~ "Sowing_break"))  
  
str(Lock_climate_yr)
unique(Lock_climate_yr$month_name)  

Lock_climate_yr_long_spring <- Lock_climate_yr %>%
  select(
    month_name,
    day_of_month,
    year,
    date,
    sum_evap_7_days,
    sum_evap_4_days,
    sum_evap_2_days, 
    sum_rain_7_days,
    sum_rain_4_days,
    sum_rain_2_days, 
    Threshold_7day_0,
    Threshold_4day_0,
    Threshold_2day_0
  ) %>%
  filter(month_name %in% c("Apr" , "May" , "Jun" , "Jul"))
climate_yr_long <- Lock_climate_yr_long_spring %>% 
  pivot_longer(starts_with("sum_"),
               names_to = "period",
               values_to = "value"
  )
climate_yr_long

Unkovich_rule <- climate_yr_long %>% 
  filter(period %in% c("sum_evap_7_days" , "sum_evap_4_days" , "sum_evap_2_days" )) %>% 
  ggplot(aes(x = date, value , color= period))+
  geom_point()+
  theme_bw()+
  facet_wrap(vars(year), scales = "free_x")+
  #scale_y_continuous(limits = c(0, 40))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="bottom")+
  labs(title = "Unkovich rule using different sum periods",
       subtitle = "The amount of rainfall required to trigger optimal sowing ",
       x = "",
       y = "Sum of evaopration "
  )
Unkovich_rule

Unkovich_rule_2023_2024 <- climate_yr_long %>% 
  filter(year %in% c(2023, 2024)) %>% 
  filter(period %in% c("sum_evap_7_days" , "sum_evap_4_days" , "sum_evap_2_days" )) %>% 
  ggplot(aes(x = date, value , color= period))+
  geom_point()+
  theme_bw()+
  facet_wrap(vars(year), scales = "free_x")+
  scale_y_continuous(limits = c(0, 45))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="bottom")+
  labs(title = "Unkovich rule using different sum periods",
       subtitle = "The amount of rainfall required to trigger optimal sowing ",
       x = "",
       y = "Sum of evaopration "
  )
Unkovich_rule_2023_2024

climate_yr_long




path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files
ggsave(plot = Unkovich_rule,
       filename = paste0(path_saved_files,"/Unkovich_rule_sum_evap2014_2024", ".png" ),
       width = 20, height = 12, units = "cm")

ggsave(plot = Unkovich_rule_2023_2024,
       filename = paste0(path_saved_files,"/Unkovich_rule_sum_evap2023_2024", ".png" ),
       width = 20, height = 12, units = "cm")

## but are these really different for example on the 
climate_yr_long

climate_yr_long <- climate_yr_long %>% 
  mutate(optimal_sowing_7days = case_when(
    Threshold_7day_0 == "Sowing_break" ~ "40",
    .default = Threshold_7day_0
  ))
climate_yr_long  
sowing_rules7 <- climate_yr_long %>%  filter(!is.na(Threshold_7day_0)) %>% select(date, year, optimal_sowing_7days)
sowing_rules7

Unkovich_rule_2023_2024_7days <- climate_yr_long %>% 
  filter(year %in% c(2023, 2024)) %>% 
  filter(period %in% c("sum_evap_7_days" , "sum_rain_7_days"  )) %>% 
  ggplot(aes(x = date, value , color= period))+
  geom_line()+
  theme_bw()+
  facet_wrap(vars(year), scales = "free_x")+
  #scale_y_continuous(limits = c(0, 40))+
  geom_vline(data = filter(sowing_rules7, year %in% c(2023, 2024)), 
               aes(xintercept = date),  color = "grey")+
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
        legend.position="none"
          )+
  labs(title = "Unkovich rule 7 days using different sum periods",
       subtitle = "Optimal sowing rule",
       x = "",
       y = "sum of rain and sum of evap"
  )
Unkovich_rule_2023_2024_7days


ggsave(plot = Unkovich_rule_2023_2024_7days,
       filename = paste0(path_saved_files,"/Unkovich_rule_7day with optimal sowing2023_2024", ".png" ),
       width = 20, height = 12, units = "cm")



################################################################################
climate_yr_long <- climate_yr_long %>% 
  mutate(optimal_sowing_4days = case_when(
    Threshold_4day_0 == "Sowing_break" ~ 40,
    .default = 0
  ))
sowing_rules4 <- climate_yr_long %>%  filter(!is.na(Threshold_4day_0)) %>% select(date, year, optimal_sowing_4days)


Unkovich_rule_2023_2024_4days <- climate_yr_long %>% 
  filter(year %in% c(2023, 2024)) %>% 
  filter(period %in% c("sum_evap_4_days" , "sum_rain_4_days"  )) %>% 
  ggplot(aes(x = date, value , color= period))+
  geom_line()+
  theme_bw()+
  facet_wrap(vars(year), 
             scales = "free_x")+
  #scale_y_continuous(limits = c(0, 40))+
  geom_vline(data = filter(sowing_rules4, year %in% c(2023, 2024)), 
             aes(xintercept = date),  color = "grey")+
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
        legend.position="none"
  )+
  labs(title = "Unkovich rule 4 days using different sum periods",
       subtitle = "Optimal sowing rule",
       x = "",
       y = "sum of rain and sum of evap"
  )
Unkovich_rule_2023_2024_4days

ggsave(plot = Unkovich_rule_2023_2024_4days,
       filename = paste0(path_saved_files,"/Unkovich_rule_4day with optimal sowing2023_2024", ".png" ),
       width = 20, height = 12, units = "cm")

################################################################################
climate_yr_long <- climate_yr_long %>% 
  mutate(optimal_sowing_2days = case_when(
    Threshold_2day_0 == "Sowing_break" ~ 40,
    .default = 0
  ))
sowing_rules2 <- climate_yr_long %>%  filter(!is.na(Threshold_2day_0)) %>% select(date, year, optimal_sowing_2days)


Unkovich_rule_2023_2024_2days <- climate_yr_long %>% 
  filter(year %in% c(2023, 2024)) %>% 
  filter(period %in% c("sum_evap_2_days" , "sum_rain_2_days"  )) %>% 
  ggplot(aes(x = date, value , color= period))+
  geom_line()+
  theme_bw()+
  facet_wrap(vars(year), scales = "free_x")+
  #scale_y_continuous(limits = c(0, 40))+
  geom_vline(data = filter(sowing_rules2, year %in% c(2023, 2024)), 
             aes(xintercept = date),  color = "grey")+
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
        legend.position="none"
  )+
  labs(title = "Unkovich rule 2 days using different sum periods",
       subtitle = "Optimal sowing rule",
       x = "",
       y = "sum of rain and sum of evap"
  )
Unkovich_rule_2023_2024_2days

ggsave(plot = Unkovich_rule_2023_2024_2days,
       filename = paste0(path_saved_files,"/Unkovich_rule_2day with optimal sowing2023_2024", ".png" ),
       width = 20, height = 12, units = "cm")

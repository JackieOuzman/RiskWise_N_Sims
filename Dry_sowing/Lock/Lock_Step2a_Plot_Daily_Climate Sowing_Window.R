# This file is for plotting frost days for one year and grain yld and flowering date
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


Dry_sowing_Lock_factor_with_met <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/Results/Dry_sowing_Lock_factor_with_met_18046_v2.csv")
str(Dry_sowing_Lock_factor_with_met)

unique(Dry_sowing_Lock_factor_with_met$Wheat.Phenology.CurrentStageName)

## Filter just to plot one year and one sowing date - frost year with typical sowing date

Sim_met_may102024 <- Dry_sowing_Lock_factor_with_met %>% 
  filter(year == 2024) %>% 
  filter(Sowing_date == "10-may")
Sim_met_may102024

unique(Sim_met_may102024$Wheat.Phenology.CurrentStageName)
phenology_stages <- Sim_met_may102024 %>% filter(!is.na(Wheat.Phenology.CurrentStageName)) %>% 
  select(Clock.Today,Wheat.Phenology.CurrentStageName )
phenology_stages

str(Sim_met_may102024)
unique(Sim_met_may102024$frost_Sensitive_period)
Frost_senstive_period <- Sim_met_may102024 %>% 
  select(Clock.Today, frost_Sensitive_period ) %>% 
  filter(frost_Sensitive_period == "frost_sensitive_period" )
Frost_senstive_period

Frost_most_senstive_period <- Sim_met_may102024 %>% 
  select(Clock.Today, frost_most_Sensitive_period ) %>% 
  filter(frost_most_Sensitive_period == "frost_most_sensitive_period" )
Frost_most_senstive_period

phenology_stages

Anthesis_Flowering <- phenology_stages %>% filter(Wheat.Phenology.CurrentStageName== "Anthesis" )


plot1 <- Sim_met_may102024 %>% 
  ggplot(aes(x = Clock.Today, mint))+
 
  geom_vline(data = Frost_senstive_period, aes(xintercept = Clock.Today), color = "lightgrey", size = 2)+
  geom_vline(data = Anthesis_Flowering, aes(xintercept = Clock.Today), color = "darkgreen", size = 2)+
  annotate("text", x= as.Date("2024-08-22"), y=1.5, label="Flowering", color = "darkgreen")+ 
  geom_point()+
  
  theme_classic()+
  theme(legend.position = "none")+
  geom_hline(yintercept=1,  linetype="dashed", color = "red")+
  annotate("text", x= as.Date("2024-10-30"), y=1.5, label="Frost definition", color = "red")+ 
  geom_vline(data = phenology_stages, aes(xintercept = Clock.Today), linetype="dashed", color = "grey")+
  
  labs(title = "Climate station Lock 18046",
       subtitle = "Days from sowing to harvest only. Fixed sowing dates 10-May 2024",
       x = "",
       y = "Min temp",
       caption = "Dashed grey lines indicate phenology stages. Solid grey is frost senstive period (Phenology.Stage between 6.49 - 9.5)")
plot1


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing", "Results")

ggsave(plot = plot1,
       filename = paste0(path_saved_files,"/MinTemp_vs_days_Lock2023_v2", ".png" ),
       width = 20, height = 12, units = "cm")


#### grain yld vs flowering date



str(Dry_sowing_Lock_factor_with_met)
flowering_date <- Dry_sowing_Lock_factor_with_met %>% 
  filter(Wheat.Phenology.CurrentStageName == "Anthesis"  ) %>% 
  select(year, Clock.Today, Sowing_date) 

Yield <- Dry_sowing_Lock_factor_with_met %>% 
 group_by(year, Sowing_date) %>% 
  summarise(max_yld_t_ha = max(Yield_Adj_t_ha,na.rm = TRUE)) 


Yld_flowering <- left_join(flowering_date, Yield)
Yld_flowering <- Yld_flowering %>% mutate(Julian_days =Clock.Today )

Yld_flowering$Julian_days <- lubridate::yday(as.Date(Yld_flowering$Julian_days))

Yld_flowering$Sowing_date <- factor(Yld_flowering$Sowing_date , ordered = TRUE, 
                                            levels = c(
                                              "1-apr" ,
                                              "5-apr" ,
                                              "10-apr" ,
                                              "15-apr" ,
                                              "20-apr",
                                              "25-may",
                                              "1-may" ,
                                              "5-may" ,
                                              "10-may" ,
                                              "15-may" ,
                                              "20-may",
                                              "25-apr" 
                                            ))

Yld_flowering <- Yld_flowering %>% 
  mutate(optimal_flowering_start = paste0(year(Clock.Today), "-09-04"),
         optimal_flowering_end = paste0(year(Clock.Today), "-09-14")) 

Yld_flowering <- Yld_flowering %>% 
  mutate(Julian_optimal_flowering_start = lubridate::yday(as.Date(optimal_flowering_start)),
         Julian_optimal_flowering_end = lubridate::yday(as.Date(optimal_flowering_end)) )                                                       
names(Yld_flowering)

plot2 <- Yld_flowering %>% 
  ggplot(aes(x =Julian_days, max_yld_t_ha))+
  geom_vline(data = Yld_flowering, aes(xintercept = Julian_optimal_flowering_start), 
             color = "darkgreen", size = 1)+
  geom_vline(data = Yld_flowering, aes(xintercept = Julian_optimal_flowering_end), 
             color = "darkgreen", size = 1)+
  geom_point()+
  geom_point(data= Yld_flowering %>% filter(year == 2024), 
             aes(x =Julian_days, max_yld_t_ha), colour ="red")+
  facet_wrap(.~ Sowing_date)+
  
  theme_classic()+
  theme(legend.position = "none")+
  labs(title = "Yield vs flowering dates Lock 18046",
       subtitle = "Fixed sowing dates as facet",
       x = "Flowering dates Julian days",
       y = "Yield t/ha",
       caption = "Green line indicated optimal flowering dates. Red dot =2024")
plot2

ggsave(plot = plot2,
       filename = paste0(path_saved_files,"/FloweringJulianDays_vs_yiled_Lock_v2", ".png" ),
       width = 20, height = 12, units = "cm")

str(Yld_flowering)

plot3 <- Yld_flowering %>% 
  filter(year == 2024) %>% 
  ggplot(aes(x =Julian_days, max_yld_t_ha))+
  geom_vline(data = Yld_flowering, aes(xintercept = Julian_optimal_flowering_start), 
             color = "darkgreen", size = 1)+
  geom_vline(data = Yld_flowering, aes(xintercept = Julian_optimal_flowering_end), 
             color = "darkgreen", size = 1)+
  geom_point()+
  facet_wrap(.~ Sowing_date)+
  
  theme_classic()+
  theme(legend.position = "none")+
  labs(title = "Yield vs flowering dates Lock 18046",
       subtitle = "Year 2024. Sowing dates are faceted",
       x = "Flowering dates Julian days",
       y = "Yield t/ha",
       caption = "Green line indicated optimal flowering dates.")
plot3

ggsave(plot = plot3,
       filename = paste0(path_saved_files,"/FloweringJulianDays_vs_yiled_Lock2024_v2", ".png" ),
       width = 20, height = 12, units = "cm")

Yld_flowering

plot2_box <- Yld_flowering %>% 
  ggplot(aes(x =Sowing_date, max_yld_t_ha))+
  geom_boxplot()+
  geom_point(alpha =0.2)+
  geom_point(data= Yld_flowering %>% filter(year == 2024), aes(x =Sowing_date, max_yld_t_ha), colour ="red")+
  theme_classic()+
  theme(legend.position = "none")+
  ylim(0,8)+
  labs(title = "Yield vs sowing dates Lock 18046",
       y = "Yield t/ha",
       x ="",
       caption = "Red dot = 2024."
       )
plot2_box

ggsave(plot = plot2_box,
       filename = paste0(path_saved_files,"/Box_yield_Lock_v2", ".png" ),
       width = 20, height = 12, units = "cm")

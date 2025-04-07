# This file is for plotting frost days for one year
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


Dry_sowing_Lock_factor_with_met <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/Results/Dry_sowing_Lock_factor_with_met_18046.csv")
str(Dry_sowing_Lock_factor_with_met)

unique(Dry_sowing_Lock_factor_with_met$Wheat.Phenology.CurrentStageName)

## Filter just to plot one year and one sowing date - frost year with typical sowing date

Sim_met_may102023 <- Dry_sowing_Lock_factor_with_met %>% 
  filter(year == 2023) %>% 
  filter(Sowing_date == "10-may")
unique(Sim_met_may102023$Wheat.Phenology.CurrentStageName)
phenology_stages <- Sim_met_may102023 %>% filter(!is.na(Wheat.Phenology.CurrentStageName)) %>% 
  select(Clock.Today,Wheat.Phenology.CurrentStageName )
phenology_stages

str(Sim_met_may102023)
unique(Sim_met_may102023$frost_Sensitive_period)
Frost_senstive_period <- Sim_met_may102023 %>% 
  select(Clock.Today, frost_Sensitive_period ) %>% 
  filter(frost_Sensitive_period == "frost_sensitive_period" )
Frost_senstive_period

Frost_most_senstive_period <- Sim_met_may102023 %>% 
  select(Clock.Today, frost_most_Sensitive_period ) %>% 
  filter(frost_most_Sensitive_period == "frost_most_sensitive_period" )
Frost_most_senstive_period


plot1 <- Sim_met_may102023 %>% 
  ggplot(aes(x = Clock.Today, mint))+
 
  geom_vline(data = Frost_senstive_period, aes(xintercept = Clock.Today), color = "lightgrey", size = 2)+
  #geom_vline(data = Frost_most_senstive_period, aes(xintercept = Clock.Today), color = "darkgrey", size = 2)+
  geom_point()+
  
  theme_classic()+
  theme(legend.position = "none")+
  geom_hline(yintercept=1,  linetype="dashed", color = "red")+
  annotate("text", x= as.Date("2023-10-30"), y=1.5, label="Frost definition", color = "red")+ 
  geom_vline(data = phenology_stages, aes(xintercept = Clock.Today), linetype="dashed", color = "grey")+
  labs(title = "Climate station Lock 18046",
       subtitle = "Days from sowing to harvest only. Fixed sowing dates 10-May",
       x = "",
       y = "Min temp",
       caption = "Dashed grey lines indicate phenology stages. Solid grey is frost senstive period (Zadok.Stage between 6.49 - 9.5)")
plot1


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing", "Results")

ggsave(plot = plot1,
       filename = paste0(path_saved_files,"/MinTemp_vs_days_Lock2023", ".png" ),
       width = 20, height = 12, units = "cm")

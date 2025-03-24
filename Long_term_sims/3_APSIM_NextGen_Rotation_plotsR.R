# This file is for graph some long term sims Next Gen APISM sim files.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

# Read file ----
APSIM_NextGen_daily <-  read_csv("X:/Riskwi$e/Curyo/4_Long term sims/Results/APSIM_NextGen_Daily.csv" )

# Did a crop get sown every year? ----
crops <- APSIM_NextGen_daily %>%  distinct(Crop)
APSIM_NextGen_daily %>%  distinct(Crop)
Phenology <- distinct(APSIM_NextGen_daily,Phenology)


APSIM_NextGen_daily_no_fallow_sowing <- APSIM_NextGen_daily %>% 
  filter(Crop == "Wheat"|
         Crop == "Barley"| 
           Crop == "Chickpea"|
           Crop == "Canola") %>% 
  
  filter(Phenology == "Sowing") %>% 
  select(Date , Year ,Crop, Phenology  , FertNApplied, Soil_NO3, Soil_NH4) %>% 
  mutate(month = lubridate::month(Date, label = TRUE))
str(APSIM_NextGen_daily_no_fallow_sowing)



plot1 <-APSIM_NextGen_daily_no_fallow_sowing %>% 
  ggplot(aes(x = Year  , y = month, group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  labs(title = "Sowing treatment Nil",
       subtitle = "Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B.",
       caption = "Years 1960-2024",
       y = "Month of sowing",
       x = "Year of simulation")
       
plot1


APSIM_NextGen_daily_no_fallow_harvest <- APSIM_NextGen_daily %>% 
  filter(Crop == "Wheat"|
           Crop == "Barley"| 
           Crop == "Chickpea"|
           Crop == "Canola") %>% 
  
  filter(Phenology == "HarvestRipe") %>% 
  #select(Date , Year ,Crop, Phenology  , FertNApplied, Soil_NO3, Soil_NH4) %>% 
  mutate(month = lubridate::month(Date, label = TRUE))
str(APSIM_NextGen_daily_no_fallow_harvest)



Yield <-APSIM_NextGen_daily_no_fallow_harvest %>% 
  ggplot(aes(x = Year  , y = Yield, group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  labs(title = "Harvest-  treatment Nil",
       subtitle = "Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B.",
       caption = "Years 1960-2024",
       y = "Yield t/ha",
       x = "Year of simulation")
Yield

Protien    <-APSIM_NextGen_daily_no_fallow_harvest %>% 
  ggplot(aes(x = Year  , y = Protien, group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  labs(title = "Harvest-  treatment Nil",
       subtitle = "Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B.",
       caption = "Years 1960-2024",
       y = "Protien ",
       x = "Year of simulation")
Protien

N_applied    <-APSIM_NextGen_daily_no_fallow_harvest %>% 
  ggplot(aes(x = Year  , y = FertNApplied, group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  labs(title = "N applied at sowing-  treatment Nil",
       subtitle = "Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B.",
       caption = "Years 1960-2024",
       y = "FertNApplied",
       x = "Year of simulation")
N_applied


NO3_insoil_sowing    <-APSIM_NextGen_daily_no_fallow_harvest %>% 
  ggplot(aes(x = Year  , y = Soil_NO3, group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  labs(title = "N applied at sowing-  treatment Nil",
       subtitle = "Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B.",
       caption = "Years 1960-2024",
       y = "Soil_NO3",
       x = "Year of simulation")
NO3_insoil_sowing

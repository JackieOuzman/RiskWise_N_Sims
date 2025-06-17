# This file is for importing Next Gen APISM sim files.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

### create a list of files
### create a list of headers
### put file name into df

#################################################################################

# Daily output files -------------------------------------------------------

APSIM_NextGen_daily <- read_csv("X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Curyo_7_Rotation_N_bank std output Trial.DailyReport.csv")
str(APSIM_NextGen_daily)
unique(APSIM_NextGen_daily$Wheat.Phenology.CurrentStageName)

wheat<- APSIM_NextGen_daily %>% 
  filter(Wheat.Phenology.CurrentStageName == "HarvestRipe") %>% 
  select( Zone, CurrentState, WheatYieldkgha,  Wheat.Phenology.CurrentStageName )%>% 
  rename(Yieldkgha = WheatYieldkgha,
         Phenology.CurrentStageName = Wheat.Phenology.CurrentStageName) 
         

unique(APSIM_NextGen_daily$Barley.Phenology.CurrentStageName)
barley<- APSIM_NextGen_daily %>% 
  filter(Barley.Phenology.CurrentStageName == "HarvestRipe") %>% 
  select( Zone, CurrentState, BarleyYieldkgha, Barley.Phenology.CurrentStageName ) %>% 
  rename(Yieldkgha = BarleyYieldkgha,
         Phenology.CurrentStageName = Barley.Phenology.CurrentStageName)

unique(APSIM_NextGen_daily$CurrentState)
canola1<- APSIM_NextGen_daily %>% 
  filter(CurrentState == "Canola1") %>% 
  select( Zone, CurrentState, CanolaYieldkgha, Date, Canola.Phenology.CurrentStageName )  
canola <- canola1 %>% 
  group_by(Zone) %>% 
  summarise(max_yld = max(CanolaYieldkgha,na.rm = TRUE ))
  
ungroup(canola) 
canola <- canola %>% rename(Yieldkgha = max_yld) %>% 
         mutate (Phenology.CurrentStageName = "Max_yld",
                 CurrentState = "canola")



unique(APSIM_NextGen_daily$CurrentState)
Chickpea1<- APSIM_NextGen_daily %>% 
  filter(CurrentState == "Chickpea") %>% 
  select( Zone, CurrentState, ChickpeaYieldkgha, Date, Chickpea.Phenology.CurrentStageName )  
Chickpea <- Chickpea1 %>% 
  group_by(Zone) %>% 
  summarise(max_yld = max(ChickpeaYieldkgha,na.rm = TRUE ))

ungroup(Chickpea) 
Chickpea <- Chickpea %>% rename(Yieldkgha = max_yld) %>% 
  mutate (Phenology.CurrentStageName = "Max_yld",
CurrentState = "chickpea")

names(canola)
names(wheat)
crops_yld <- rbind(wheat, barley, 
                   canola, Chickpea
                   )
crops_yld <- crops_yld %>% 
  mutate(year = case_when(
   CurrentState  == "Wheat1"~ 2018,
   CurrentState  == "canola"~ 2019,
   CurrentState  == "Wheat2"~ 2020,
   CurrentState  == "Barley_1"~ 2021,
   CurrentState  == "Wheat3"~ 2022,
   CurrentState  == "chickpea"~ 2023,
   CurrentState  == "Barley_2"~ 2024  ))

write_csv(crops_yld, "X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/crops_yld_trial.csv")
rm(barley, wheat, wheat1, canola, canola1, canola_max_yld, Chickpea, Chickpea1)




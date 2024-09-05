# This file is for importing APISM sim files.
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



APSIM_NextGen <- read_excel("X:/Riskwi$e/Bute/2_Sims_post_Sep2024/2_NextGen/Bute_5_Rotation_N_bank.xlsx", 
                                       sheet = "TransitionReport")
str(APSIM_NextGen)


#look up starting values and sowing values

soil_water_start_value = APSIM_NextGen %>% 
  filter(StartYear  == 2022 & CurrentState == "S_fallow_1" & SimulationName == "Bute_Nil") %>% 
  select(SwStart)
soil_water_sowing_value = APSIM_NextGen %>% 
  filter(StartYear  == 2022 & CurrentState == "wheat_1" & SimulationName == "Bute_Nil") %>% 
  select(SwStart)


soil_NO3_start_value = APSIM_NextGen %>% 
  filter(StartYear  == 2022 & CurrentState == "S_fallow_1" & SimulationName == "Bute_Nil") %>% 
  select(NO3Start)
soil_NO3_sowing_value = APSIM_NextGen %>% 
  filter(StartYear  == 2022 & CurrentState == "wheat_1" & SimulationName == "Bute_Nil") %>% 
  select(NO3Start)

  
soil_NH4_start_value = APSIM_NextGen %>% 
  filter(StartYear  == 2022 & CurrentState == "S_fallow_1" & SimulationName == "Bute_Nil") %>% 
  select(NH4Start)
soil_NH4_sowing_value = APSIM_NextGen %>% 
  filter(StartYear  == 2022 & CurrentState == "wheat_1" & SimulationName == "Bute_Nil") %>% 
  select(NH4Start)


soil_water_start <- soil_water_start_value$SwStart 
soil_water_sowing <- soil_water_sowing_value$SwStart
soil_NO3_start<- soil_NO3_start_value$NO3Start
soil_NO3_sowing<- soil_NO3_sowing_value$NO3Start
soil_NH4_start<- soil_NH4_start_value$NH4Start
soil_NH4_sowing<- soil_NH4_sowing_value$NH4Start


APSIM_NextGen  <- APSIM_NextGen %>% 
  select(Year = StartYear, 
         #Source = "APSIM_NextG" #,
         Treatment = SimulationName ,
         InCropFert = NFertiliser ,
         Crop_temp = CurrentState ,
         #Cultivar = Variety ,
         
         #soil_water_start = SwStart,#when in fallow
         #soil_water_sowing = SwStart,#when in crop
         soil_water_harvest = SwEnd,#when in crop
         
         #soil_NO3_start = NO3Start,#when in fallow
         #soil_N03_sowing = NO3Start,#when in crop
         soil_NO3_harvest = NO3End,#when in crop
         
         #soil_NH4_start= NH4Start,#when in fallow
         #soil_NH4_sowing= NH4Start,#when in crop
         soil_NH4_harvest= NH4End,#when in crop
         
         Biomass = Biomass,
         Yield_kg = Yield ,
         InCropRain= Rainfall #umm not sure this is incrop rain
  )


## Just the nil treatment for now

APSIM_NextGen <- APSIM_NextGen %>%  filter(Treatment == "Bute_Nil")

# add some extra clms
APSIM_NextGen <- APSIM_NextGen %>% mutate(
  Source = "APSIM_NextG",
  #Treatment = "0N_applied",
  Yield = Yield_kg/1000,
  soil_water_start = soil_water_start,
  soil_water_sowing = soil_water_sowing,
  soil_NO3_start = soil_NO3_start,
  soil_NO3_sowing = soil_NO3_sowing,
  soil_NH4_start = soil_NH4_start,
  soil_NH4_sowing = soil_NH4_sowing,
  
  Crop = case_when(
    Year  == 2022 & Crop_temp == "wheat_1" ~ "Wheat",
    Year  == 2023 & Crop_temp == "barley_1"~ "Barley"),
  Cultivar = case_when(
    Year  == 2022 ~ "mace",
    Year  == 2023 ~ "commander"))

APSIM_NextGen <- APSIM_NextGen %>% mutate(
Soil_mineral_N_sowing = as.double(soil_NO3_sowing) + as.double(soil_NH4_sowing))

str(APSIM_NextGen)
 


#Just ordering it
APSIM_NextGen  <- APSIM_NextGen %>% 
  select(Year , 
         Source,
         Treatment  ,
         InCropFert  ,
         Crop ,
         Cultivar  ,
         
         soil_water_start,
         soil_water_sowing,
         soil_water_harvest,
         
         soil_NO3_start,
         soil_NO3_sowing, #umm is this NO3 + NH4?
         soil_NO3_harvest,
         
         soil_NH4_start,
         soil_NH4_sowing,
         soil_NH4_harvest,
         
         Soil_mineral_N_sowing,
         
         Biomass ,
         Yield  ,
         InCropRain
  )
str(APSIM_NextGen)


#remove the fallow rows
APSIM_NextGen  <- APSIM_NextGen %>% filter(!is.na(Crop ))


write.csv(APSIM_NextGen ,
          "X:/Riskwi$e/Bute/2_Sims_post_Sep2024/To_compare_etc/APSIM_NextGen.csv", row.names = FALSE )


################################################################################


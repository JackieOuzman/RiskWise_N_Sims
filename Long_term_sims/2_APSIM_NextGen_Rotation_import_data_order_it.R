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
                                  
APSIM_NextGen_daily <-  read_excel("X:/Riskwi$e/Curyo/4_Long term sims/Curyo_7_Rotation_N_bank_version4.xlsx", 
                                   col_types = c("text", "numeric", "text", 
                                                 "numeric", "text", "numeric", "text", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "text", "text", 
                                                 "text", "text"))
str(APSIM_NextGen_daily)

## Create a new clm called year
APSIM_NextGen_daily <- APSIM_NextGen_daily %>% 
  mutate(Year = year(Date), 
         Source = "NextGen",
         Treatment = Zone,
         
         ) 

str(APSIM_NextGen_daily)
unique(APSIM_NextGen_daily$CurrentState)
unique(APSIM_NextGen_daily$Treatment)

################################################################################
## details about the crop sequence

#add in crop and cultivar
APSIM_NextGen_daily <- APSIM_NextGen_daily %>% 
  mutate(
Crop =CurrentState ,

Cultivar = case_when(
  CurrentState  == "Wheat" ~ "scepter",
  CurrentState  == "Canola" ~ "GenericEarly",
  CurrentState  == "Barley" ~ "Spartacus CL",
  CurrentState  == "Chickpea" ~ "amethyst"), 

Phenology = case_when(
  CurrentState  == "Wheat" ~ W_Phenology ,
  CurrentState  == "Canola" ~ C_Phenology,
  CurrentState  == "Barley" ~ B_Phenology,
  CurrentState  == "Chickpea" ~ Chick_Phenology), 


Protien = case_when(
  CurrentState  == "Wheat" ~ Protein_W,
  CurrentState  == "Canola" ~ NA,
  CurrentState  == "Barley" ~ Protein_B,
  CurrentState  == "Chickpea" ~ NA), 

 
Biomass = case_when(
  CurrentState  == "Wheat" ~ AboveGroundW_WtKgha,
  CurrentState  == "Canola" ~ AboveGroundC_WtKgha,
  CurrentState  == "Barley" ~ AboveGroundB_WtKgha,
  CurrentState  == "Chickpea" ~ AboveGroundChick_WtKgha ),
  
 
Zadok = case_when(
  CurrentState  == "Wheat" ~ WheatZadok,
  CurrentState  == "Canola" ~ CanolaStage,
  CurrentState  == "Barley" ~ BarleyZadok,
  CurrentState  == "Chickpea" ~ ChickpeaStage ),
 
Yield = case_when(
  CurrentState  == "Wheat" ~ Yield_W/1000,
  CurrentState  == "Canola" ~ Yield_C/1000,
  CurrentState  == "Barley" ~ Yield_B/1000,
  CurrentState  == "Chickpea" ~ Yield_Chick/1000 ),
  
  
HarvestIndex = (Yield*1000)/Biomass

)

#just checking it has worked
names(APSIM_NextGen_daily) ## check for how many depth
str(APSIM_NextGen_daily)
unique(APSIM_NextGen_daily$Crop) #the NA are fallow
unique(APSIM_NextGen_daily$Cultivar)


max(APSIM_NextGen_daily$Zadok)
min(APSIM_NextGen_daily$Zadok)

max(APSIM_NextGen_daily$Yield)
min(APSIM_NextGen_daily$Yield)


## sum the soil water for all the depths and No 3 for all the depths

APSIM_NextGen_daily <- APSIM_NextGen_daily %>%
  mutate(
    NO3 = `NO3.kgha(1)` + `NO3.kgha(2)` +
      `NO3.kgha(3)` + `NO3.kgha(4)` +
      `NO3.kgha(5)` + `NO3.kgha(6)` + `NO3.kgha(7)`) %>%
      mutate(Soil_NO3 = NO3)
      
APSIM_NextGen_daily <- APSIM_NextGen_daily %>%
  mutate(
    NH4 = `NH4.kgha(1)` + `NH4.kgha(2)` +
      `NH4.kgha(3)` + `NH4.kgha(4)` +
      `NH4.kgha(5)` + `NH4.kgha(6)` + `NH4.kgha(7)`) %>%
  mutate(Soil_NH4 = NH4)


  APSIM_NextGen_daily <- APSIM_NextGen_daily %>%
    mutate(
      Soil_Water =
        `Soil.Water.Volumetric(1)` + `Soil.Water.Volumetric(2)` +
        `Soil.Water.Volumetric(3)` + `Soil.Water.Volumetric(4)` +
        `Soil.Water.Volumetric(5)` + `Soil.Water.Volumetric(6)` +
        `Soil.Water.Volumetric(7)`
    )
  
##
str(APSIM_NextGen_daily)
unique(APSIM_NextGen_daily$Crop)
max(APSIM_NextGen_daily$Yield)

#Just ordering it
APSIM_NextGen_daily  <- APSIM_NextGen_daily %>% 
  select( Date, 
          Year, 
          Source,           
          Treatment,            
          Crop,      
          Cultivar,
          Zadok ,
          Phenology,
          FertNApplied,
          #soil_water_start,
          Soil_Water,
         # soil_NO3_start,
          Soil_NO3,
          Soil_NH4,
          Biomass, 
          Yield, 
          Protien,
          HarvestIndex
         
  )



APSIM_NextGen_daily <- APSIM_NextGen_daily %>% mutate(Biomass = Biomass/1000)



write.csv(APSIM_NextGen_daily ,
          "X:/Riskwi$e/Curyo/4_Long term sims/Results/APSIM_NextGen_Daily.csv", row.names = FALSE )

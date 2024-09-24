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

APSIM_NextGen_daily <- read_excel("X:/Riskwi$e/Curyo/2_Sims_post_Sep2024/2_NextGen/Curyo_5_Rotation_N_bank.xlsx", 
                            sheet = "DailyReport")
str(APSIM_NextGen_daily)

## Create a new clm called year
APSIM_NextGen_daily <- APSIM_NextGen_daily %>% 
  mutate(Year = year(Date), 
         Source = "NextGen",
         Treatment = SimulationName,
         InCropFert = FertNApplied,#check this is the correct clm could be FertAmt 
         
         soil_water_start =SwStart ,
         soil_water_sowing = "NA",
         soil_water_harvest= "NA",
         
         soil_NO3_start = NO3Start,
         soil_NO3_sowing= "NA", 
         soil_NO3_harvest ="NA" ,
         Soil_mineral_N_sowing = "Just use NO3",
         
         soil_NH4_start ="not_reported",
         soil_NH4_sowing ="not_reported",
         soil_NH4_harvest ="not_reported"
         
         
         ) 

str(APSIM_NextGen_daily)
unique(APSIM_NextGen_daily$CurrentState)
unique(APSIM_NextGen_daily$CurrentState)

################################################################################
## details about the crop sequence

#add in crop and cultivar
APSIM_NextGen_daily <- APSIM_NextGen_daily %>% 
  mutate(
Crop = case_when(
  Year  == 2018 & CurrentState  == "wheat_1"~ "Wheat",
  Year  == 2021 & CurrentState  == "canola_1"~ "Canola",
  Year  == 2020 & CurrentState  == "wheat_2"~ "Wheat",
  Year  == 2021 & CurrentState  == "barley_1"~ "Barley",
  Year  == 2022 & CurrentState  == "wheat_3"~ "Wheat" ),
  
Cultivar = case_when(
  Year  == 2018 ~ "mace",
  Year  == 2021 ~ "GenericEarly",
  Year  == 2020 ~ "mace",
  Year  == 2021 ~ "commander",
  Year  == 2022 ~ "mace" ),
  
Biomass = case_when(
  Year  == 2018 ~ AboveGroundW_WtKgha,
  Year  == 2021 ~ AboveGroundC_WtKgha,
  Year  == 2020 ~ AboveGroundW_WtKgha,
  Year  == 2021 ~ AboveGroundB_WtKgha,
  Year  == 2022 ~ AboveGroundW_WtKgha ),
  
Zadok = case_when(
  Year  == 2018 ~ WheatZadok,
  Year  == 2021 ~ CanolaStage,
  Year  == 2020 ~ WheatZadok,
  Year  == 2021 ~ BarleyZadok,
  Year  == 2022 ~ WheatZadok ),
  
WaterStress = case_when(
  Year  == 2018 ~ W_WaterStress,
  Year  == 2021 ~ C_WaterStress,
  Year  == 2020 ~ W_WaterStress,
  Year  == 2021 ~ B_WaterStress,
  Year  == 2022 ~ W_WaterStress ),
  
NSTress = case_when(
  Year  == 2018 ~ W_NSTress,
  Year  == 2021 ~ C_NSTress,
  Year  == 2020 ~ W_NSTress,
  Year  == 2021 ~ B_NSTress,
  Year  == 2022 ~ W_NSTress ),
  
Yield = case_when(
  Year  == 2018 ~ Yield_W/100,
  Year  == 2021 ~ Yield_C/1000,
  Year  == 2020 ~ Yield_W/100,
  Year  == 2021 ~ Yield_B/1000,
  Year  == 2022 ~ Yield_W/100 ),
  
  
HarvestIndex = (Yield*1000)/Biomass

)

names(APSIM_NextGen_daily) ## check for how many depth
str(APSIM_NextGen_daily)

## sum the soil water for all the depths and No 3 for all the depths

APSIM_NextGen_daily <- APSIM_NextGen_daily %>%
  mutate(
    NO3 = `NO3.kgha(1)` + `NO3.kgha(2)` +
      `NO3.kgha(3)` + `NO3.kgha(4)` +
      `NO3.kgha(5)` + `NO3.kgha(6)` + `NO3.kgha(7)`) %>%
      mutate(Soil_mineral_N_sowing = NO3)
      
     
  APSIM_NextGen_daily <- APSIM_NextGen_daily %>%
    mutate(
      Soil_Water =
        `Soil.Water.Volumetric(1)` + `Soil.Water.Volumetric(2)` +
        `Soil.Water.Volumetric(3)` + `Soil.Water.Volumetric(4)` +
        `Soil.Water.Volumetric(5)` + `Soil.Water.Volumetric(6)` +
        `Soil.Water.Volumetric(7)`
    )
  
##

#Just ordering it
APSIM_NextGen_daily  <- APSIM_NextGen_daily %>% 
  select(Date,
         Year , 
         Source,
         Treatment  ,
         InCropFert  , # check this is the correct clm could be FertAmt 
         Crop ,
         Cultivar  ,
         
         soil_water_start,
         #soil_water_sowing,
         #soil_water_harvest,
         Soil_Water,
         
         soil_NO3_start,
         #soil_NO3_sowing, #umm is this NO3 + NH4?
         #soil_NO3_harvest,
         NO3,
         
         # soil_NH4_start,
         # soil_NH4_sowing,
         # soil_NH4_harvest,
         
         Soil_mineral_N_sowing,
         
         Biomass ,
         Yield  ,
         Zadok,
         WaterStress,
         NSTress,
         HarvestIndex
         
         
         
         #InCropRain
  )


write.csv(APSIM_NextGen_daily ,
          "X:/Riskwi$e/Curyo/2_Sims_post_Sep2024/To_compare_etc/APSIM_NextGen_Daily.csv", row.names = FALSE )

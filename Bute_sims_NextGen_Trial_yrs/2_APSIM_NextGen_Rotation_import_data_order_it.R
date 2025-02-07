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

# Daily output files -------------------------------------------------------



APSIM_NextGen_daily <- read_excel("X:/Riskwi$e/Bute/3_Sims_post_Nov2024/Bute_5_Rotation_N_bank_v3.xlsx", 
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

#add in crop and cultivar
unique(APSIM_NextGen_daily$CurrentState)

names(APSIM_NextGen_daily)

APSIM_NextGen_daily <- APSIM_NextGen_daily %>% 
  mutate(
Crop = case_when(
  Year  == 2022 & CurrentState  == "wheat_1" ~ "Wheat",
  Year  == 2023 & CurrentState  == "barley_1"~ "Barley",
  Year  == 2024 & CurrentState  == "Chickpea"~ "Chickpea"),
Cultivar = case_when(
  Year  == 2022 ~ "specter",
  Year  == 2023 ~ "commander",
  Year  == 2024 ~ "amethyst"),
Biomass = case_when(
  Year  == 2022 ~ AboveGroundW_WtKgha,
  Year  == 2023 ~ AboveGroundB_WtKgha,
  Year  == 2024 ~ AboveGroundChick_WtKgha),
Zadok = case_when(
  Year  == 2022 ~ WheatZadok,
  Year  == 2023 ~ BarleyZadok,
  Year  == 2024 ~ ChickpeaStage),
WaterStress = case_when(
  Year  == 2022 ~ W_WaterStress,
  Year  == 2023 ~ B_WaterStress,
  Year  == 2024 ~ Chick_WaterStress),
NSTress = case_when(
  Year  == 2022 ~ W_NSTress,
  Year  == 2023 ~ B_NSTress,
  Year  == 2024 ~ Chick_NSTress),
Yield = case_when(
  Year  == 2022 ~ Yield_W/1000,
  Year  == 2023 ~ Yield_B/1000,
  Year  == 2024 ~ Yield_Chick/1000),

HarvestIndex = (Yield*1000)/Biomass

)




## sum the soil water for all the depths and No 3 for all the depths

APSIM_NextGen_daily <- APSIM_NextGen_daily %>%
  mutate(
    NO3 = `NO3.kgha(1)`+`NO3.kgha(2)`+
      `NO3.kgha(3)`+`NO3.kgha(4)`+
      `NO3.kgha(5)`+`NO3.kgha(6)`) %>% 
  mutate(Soil_mineral_N_sowing = NO3)
     
APSIM_NextGen_daily <- APSIM_NextGen_daily %>%
  mutate(
    Soil_Water = 
      `Soil.Water.Volumetric(1)`+`Soil.Water.Volumetric(2)`+
      `Soil.Water.Volumetric(3)`+`Soil.Water.Volumetric(4)`+
      `Soil.Water.Volumetric(5)`+`Soil.Water.Volumetric(6)` )     

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

unique(APSIM_NextGen_daily$Treatment)


### make sure the sim names match the trial names - here I am forcing the sim names into the trial names

APSIM_NextGen_daily <- APSIM_NextGen_daily %>% mutate(
  Treatment = case_when(
    Treatment == "Bute_Control" ~ "Control" ,
    Treatment == "Bute_DP" ~ "District_Practice" ,
    Treatment == "Bute_NBank_Conservative" ~ "Nbank_Conservative" ,
    Treatment == "Bute_NBank_Profit" ~ "Nbank_Optimum_Profit" ,
    Treatment == "Bute_NBank_Optimum_Yld" ~ "Nbank_Optimum_Yield" ,
    Treatment == "Bute_YP_Decile1" ~ "YP_Decile1" ,
    Treatment == "Bute_YP_Decile2-3" ~ "YP_Decile2-3" ,
    Treatment == "Bute_YP_Decile5" ~ "YP_Decile5" ,
    Treatment == "Bute_YP_Decile7-8" ~ "YP_Decile7-8" ,
    Treatment == "Bute_BOM" ~ "YP_BOM" 
))



APSIM_NextGen_daily <- APSIM_NextGen_daily %>% mutate(Biomass = Biomass/1000)
APSIM_NextGen_daily <- APSIM_NextGen_daily %>% mutate(soil_NO3 = NO3,
                              zadok_stage = Zadok,
                              soil_water = Soil_Water)


names(APSIM_NextGen_daily)
unique(APSIM_NextGen_daily$Treatment)

write.csv(APSIM_NextGen_daily ,
          "X:/Riskwi$e/Bute/3_Sims_post_Nov2024/results/APSIM_NextGen_Daily.csv", row.names = FALSE )

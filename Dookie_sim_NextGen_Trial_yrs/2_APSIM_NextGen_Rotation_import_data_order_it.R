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

APSIM_NextGen_daily <- read_excel("X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/Dookie_5_Rotation_N_bank.xlsx", 
                            sheet = "DailyReport")
str(APSIM_NextGen_daily)
unique(APSIM_NextGen_daily$CurrentState)

## Create a new clm called year
APSIM_NextGen_daily <- APSIM_NextGen_daily %>% 
  mutate(Year = year(Date), 
         Source = "NextGen",
         Treatment = Zone,
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
  Year  == 2022 & CurrentState  == "canola"~ "canola",
  Year  == 2023 & CurrentState  == "wheat_1"~ "Wheat",
  Year  == 2024 & CurrentState  == "barley"~ "barley"),
  
Cultivar = case_when(
  Year  == 2022 ~ "GenericEarly",
  Year  == 2023 ~ "yitpi",
  Year  == 2024 ~ "Spartacus_CL"),
  
Biomass = case_when(
  Year  == 2022 ~ AboveGroundC_WtKgha,
  Year  == 2023 ~ AboveGroundW_WtKgha, 
  Year  == 2024 ~ AboveGroundB_WtKgha ),
  
Zadok = case_when(
  Year  == 2022 ~ CanolaStage,
  Year  == 2023 ~ WheatZadok,
  Year  == 2024 ~ BarleyZadok),
  
WaterStress = case_when(
  Year  == 2022 ~ C_WaterStress,
  Year  == 2023 ~ W_WaterStress,
  Year  == 2024 ~ B_WaterStress ),
  
NSTress = case_when(
  Year  == 2022 ~ C_NSTress,
  Year  == 2023 ~ W_NSTress,
  Year  == 2024 ~ B_NSTress),
  
Yield = case_when(
  Year  == 2022 ~ Yield_C/1000,
  Year  == 2023 ~ Yield_W/1000,
  Year  == 2024 ~ Yield_B/1000),
  
  
HarvestIndex = (Yield*1000)/Biomass

)

names(APSIM_NextGen_daily) ## check for how many depth 7
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


unique(APSIM_NextGen_daily$Treatment)


################################################################################

APSIM_NextGen_daily <- APSIM_NextGen_daily %>% mutate(soil_NO3 = NO3,
                              zadok_stage = Zadok,
                              soil_water = Soil_Water)



APSIM_NextGen_daily <- APSIM_NextGen_daily %>% select(
  Date, 
  Year, 
  Source,           
  Treatment,            
  Crop,      
  Cultivar,   
  soil_water, 
  soil_NO3,  
  Biomass, 
  Yield, 
  zadok_stage ,
  HarvestIndex,
  WaterStress,       
  NSTress,
  InCropFert)  # check this is the correct clm could be FertAmt 


APSIM_NextGen_daily <- APSIM_NextGen_daily %>% mutate(Biomass = Biomass/1000)


unique(APSIM_NextGen_daily$Treatment)


APSIM_NextGen_daily <- APSIM_NextGen_daily %>% mutate(
  Treatment = case_when(
    Treatment == "Nil" ~ "Control",
    Treatment == "National average" ~ "National_Av",
    Treatment == "Replacment_protein" ~ "Replacment",
    
    Treatment == "YP_Decile2_3" ~ "YP_Decile2_3",
    Treatment == "YP_Decile5" ~ "YP_Decile5",
    Treatment == "YP_Decile7_8" ~ "YP_Decile7_8",
    
    Treatment == "YP_BOM" ~ "YP_BOM",
    
    Treatment == "N_bank_conservative" ~ "Nbank_Conservative",
    Treatment == "N_bank_optimal_profit" ~ "Nbank_Optimum_Profit",
    Treatment == "N_bank_optimal_yield" ~ "Nbank_Optimum_Yield",
    
    
  )
)

unique(APSIM_NextGen_daily$Treatment)

write.csv(APSIM_NextGen_daily ,
          "X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/Results/APSIM_NextGen_Daily.csv", row.names = FALSE )

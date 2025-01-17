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
                 

APSIM_NextGen_daily <- read_excel("X:/Riskwi$e/Harden/3_Sims_post_Nov2024/Harden_5_Rotation_N_bank.xlsx", 
                            sheet = "DailyReport")
str(APSIM_NextGen_daily)

## Create a new clm called year
APSIM_NextGen_daily <- APSIM_NextGen_daily %>% 
  mutate(Year = year(Date), 
         Source = "NextGen",
         Treatment = Zone,
         InCropFert = FertNApplied,#check this is the correct clm could be FertAmt 
         
         soil_water_start =SwStart , #start when transition
         soil_water_sowing = "NA",
         soil_water_harvest= "NA",
         
         soil_NO3_start = NO3Start, #start when transition
         soil_NO3_sowing= "NA", 
         soil_NO3_harvest ="NA" ,
         Soil_mineral_N_sowing = "Just use NO3",
         
         soil_NH4_start ="not_reported",
         soil_NH4_sowing ="not_reported",
         soil_NH4_harvest ="not_reported",
         
         Rainfall ,
         FertNApplied
         
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
  Year  == 2022 & CurrentState  == "Wheat1"~ "Wheat",
  Year  == 2023 & CurrentState  == "Canola"~ "Canola"
   ),
  
Cultivar = case_when(
  Year  == 2022 ~ "Scepter",
  Year  == 2023 ~ "GenericEarly"),
  
Biomass = case_when(
  Year  == 2022 ~  AboveGroundW_WtKgha,
  Year  == 2023 ~  AboveGroundC_WtKgha),
  
Zadok = case_when(
  Year  == 2022 ~ WheatZadok,
  Year  == 2023 ~ CanolaStage ),
  
WaterStress = case_when(
  Year  == 2022 ~ W_WaterStress ,
  Year  == 2023 ~ C_WaterStress),
  
NSTress = case_when(
  Year  == 2022 ~ W_NSTress ,
  Year  == 2023 ~ C_NSTress ),
  
Yield = case_when(
  Year  == 2022 ~ Yield_W /1000,
  Year  == 2023 ~ Yield_C /1000 ),
  
  
HarvestIndex = (Yield*1000)/Biomass

)

#just checking it has worked
names(APSIM_NextGen_daily) ## check for how many depth
str(APSIM_NextGen_daily)
unique(APSIM_NextGen_daily$Crop)
unique(APSIM_NextGen_daily$Cultivar)


max(APSIM_NextGen_daily$Zadok)
min(APSIM_NextGen_daily$Zadok)

max(APSIM_NextGen_daily$Yield)
min(APSIM_NextGen_daily$Yield)


## sum the soil water for all the depths and No 3 for all the depths

APSIM_NextGen_daily <- APSIM_NextGen_daily %>%
  mutate(
    NO3 = 
      (`NO3.kgha(1)` + 
      `NO3.kgha(2)` +
      `NO3.kgha(3)` + 
      `NO3.kgha(4)` +
      `NO3.kgha(5)` + 
      `NO3.kgha(6)` +
      `NO3.kgha(7)` +
      `NO3.kgha(8)` +
      `NO3.kgha(9)` +
      `NO3.kgha(10)` +
      `NO3.kgha(11)` 
        
      ) )%>% 
    mutate( 
      NH4 = 
      (  `NH4.kgha(1)` + 
         `NH4.kgha(2)` +
         `NH4.kgha(3)` + 
         `NH4.kgha(4)` +
         `NH4.kgha(5)` + 
         `NH4.kgha(6)` +
         `NH4.kgha(7)`+
         `NH4.kgha(8)`+
         `NH4.kgha(9)`+
         `NH4.kgha(10)`+
         `NH4.kgha(11)`
         ))%>%
  mutate(Soil_mineral_N_sowing = NO3 + NH4)
      
     
  APSIM_NextGen_daily <- APSIM_NextGen_daily %>%
    mutate(
      Soil_Water =
        `Soil.Water.Volumetric(1)` + `Soil.Water.Volumetric(2)` +
        `Soil.Water.Volumetric(3)` + `Soil.Water.Volumetric(4)` +
        `Soil.Water.Volumetric(5)` + `Soil.Water.Volumetric(6)` +
        `Soil.Water.Volumetric(7)` + `Soil.Water.Volumetric(8)` +
        `Soil.Water.Volumetric(9)` + `Soil.Water.Volumetric(10)` +
        `Soil.Water.Volumetric(11)`
    )
  
##
str(APSIM_NextGen_daily)
unique(APSIM_NextGen_daily$Crop)
max(APSIM_NextGen_daily$Yield)

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
         NH4 ,
         
         # soil_NH4_start,
         # soil_NH4_sowing,
         # soil_NH4_harvest,
         
         Soil_mineral_N_sowing,
         
         Biomass ,
         Yield  ,
         Zadok,
         WaterStress,
         NSTress,
         HarvestIndex,
         Rainfall,
         FertNApplied
  )

names(APSIM_NextGen_daily)
APSIM_NextGen_daily <- APSIM_NextGen_daily %>% mutate(soil_NO3 = NO3,
                                                      soil_NH4 = NH4,
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
  soil_NH4,
  Soil_mineral_N_sowing,
  Biomass, 
  Yield, 
  zadok_stage ,
  HarvestIndex,
  WaterStress,       
  NSTress,
  Rainfall,
  FertNApplied)

APSIM_NextGen_daily <- APSIM_NextGen_daily %>% mutate(Biomass = Biomass/1000)


# Names of treatments need to match ---------------------------------------

unique(APSIM_NextGen_daily$Treatment)
# APSIM_NextGen_daily %>% filter(Treatment == "YP_100") %>% 
#   summarise(max_biomass = max(Biomass),
#             max_yld = max( Yield))
# 
# APSIM_NextGen_daily %>% filter(Treatment == "Maint_150") %>% 
#   summarise(max_biomass = max(Biomass),
#             max_yld = max( Yield))

APSIM_NextGen_daily <- APSIM_NextGen_daily %>% mutate(
  Treatment = case_when(
    Treatment == "1_Nil N Control" ~                          "Control",
    Treatment == "2_National Average_45kgN_ha" ~     "National_Average",
    Treatment == "3_YP Lite_Decile1" ~              "YP_Decile_1",
    Treatment == "4_YP Lite_Decile2_3" ~            "YP_Decile_2_3",
    Treatment == "5_YP Lite_Decile5" ~              "YP_Decile_5",
    Treatment == "6_YP Lite_Decile7_8" ~            "YP_Decile_7_8",
    Treatment == "7_YP Lite_BOM" ~                   "YP_BOM",
    Treatment == "8_N_Bank_25kg_LESS_Optimum" ~           "N_Bank_Less_Optimum",
    Treatment == "9_N_Bank_Profit_Optimal" ~                 "N_Bank_Profit",
    Treatment == "10_N_Bank_Yield_Optimal" ~         "N_Bank_Yield_Optimal"
    
  )
)


write.csv(APSIM_NextGen_daily ,
          "X:/Riskwi$e/Harden/3_Sims_post_Nov2024/Results/APSIM_NextGen_Daily.csv", row.names = FALSE )


# This file is for importing Trial data.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# Trial data --------------------------------------------------------------



Trial_setup_outputs <- read_excel("X:/Riskwi$e/Curyo/2_Sims_post_Sep2024/Curyo_trial_setup_outputs.xlsx", 
                                       sheet = "Treatments_Jackie", skip = 6)
names(Trial_setup_outputs)

df_selection  <- Trial_setup_outputs %>% 
  select(Year , 
         #"Trial" = Source)#,
         Treatment = `!Treatment`, #was System 
         InCropFert =  `Total N applied at sowing and inseason` ,
         Crop = `Crop type` ,
         Cultivar = Variety ,
         soil_N03_sowing =`Amount of NO3 at sowing (kg/ha) - Soil` ,
         soil_NH4_sowing = `Amount of NH4 at sowing (kg/ha) - Soil`,  
         
         Soil_mineral_N_sowing =`Amount of total mineral N at sowing (kg/ha) -Soil`, #umm is this NO3 + NH4?
          
         Biomass = `Biomass flowering (t/ha)` ,
         Yield = `Yield (t/ha)`, #,
         soil_water_sowing = `Sowing total water (mm) - soil`, 
         
         #InCropRain= "Not_provided"
         
         #DM_Anthesis = "Not_provided" ,#'Dry matter at anthesis (t/ha)'
         #Biomass = "Not_provided", #'Dry matter at harvest (t/ha)', 
         #Harvest_Index = "Not_provided" #'Harvest index' 
  )

df_selection  <- df_selection %>% 
  mutate(
    Source = "Trial" ,
    
    
    soil_water_start = "0.885", #sum value in 2018 check this
    #soil_water_sowing = "Not_provided", provided in some years
    soil_water_harvest = "Not_provided",
    
    soil_NO3_start = "47",
   # soil_N03_sowing = "Not_provided",provided in some years
    soil_NO3_harvest = "Not_provided",
    
    soil_NH4_start= "0",
    #soil_NH4_sowing= "Not_provided", provided in some years
    soil_NH4_harvest= "Not_provided",
    
    
    
    InCropRain= "Not_provided"
  )


str(df_selection)

#Just ordering it
df_selection  <- df_selection %>% 
  select(Year , 
         Source,
         Treatment  ,
         InCropFert  ,
         Crop ,
         Cultivar  ,
         
         soil_water_start ,
         soil_water_sowing,
         soil_water_harvest ,
         
         soil_NO3_start ,
         soil_N03_sowing, #umm is this NO3 + NH4?
         soil_NO3_harvest ,
         
          soil_NH4_start,
          soil_NH4_sowing,
          soil_NH4_harvest,
         Soil_mineral_N_sowing,
          
         Biomass ,
         Yield  ,
         InCropRain#,
         #DM_Anthesis ,
         #Harvest_Index 
  )


str(df_selection)
 
write.csv(df_selection , "X:/Riskwi$e/Curyo/2_Sims_post_Sep2024/To_compare_etc/APSIM_Trial.csv", row.names = FALSE )

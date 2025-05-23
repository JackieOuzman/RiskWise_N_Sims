
# This file is for importing Trial data.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# Trial data --------------------------------------------------------------



Bute_trial_setup_outputs <- read_excel("X:/Riskwi$e/Bute/3_Sims_post_Nov2024/Bute_trial_setup_outputs.xlsx", 
                                       sheet = "Treatments_Jackie", skip = 2)
str(Bute_trial_setup_outputs)
names(Bute_trial_setup_outputs)

Bute_selection  <- Bute_trial_setup_outputs %>% 
  select(Year , 
         #"Trial" = Source)#,
         Treatment = `Treatment description` ,
         InCropFert =  `Total N applied at sowing and inseason` ,
         Crop = `Crop type` ,
         Cultivar = Variety ,
         
         #soil_water_start = "0.396",
         #soil_water_sowing = "Not_provided",
         #soil_water_harvest = "Not_provided",
         
         #soil_NO3_start = "116",
         #soil_N03_sowing = "Not_provided"
         #soil_NO3_harvest = "Not_provided",
         
         # soil_NH4_start= "30",
         # soil_NH4_sowing= "Not_provided",
         # soil_NH4_harvest= "Not_provided",
         
         Soil_mineral_N_sowing =`Soil mineral N (kg/ha) - prior to sowing`, #umm is this NO3 + NH4? -Yes it is
         # 
         # Biomass = "Not_provided",
         Yield = `Grain yield (machine) (area and moisture  corrected) (t/ha)` ,
         #InCropRain= "Not_provided"
         
         DM_Anthesis = 'Dry matter at anthesis (t/ha)',
         Biomass ='Dry matter at harvest (quadrat) (t/ha)' # is this the same?
  )

Bute_selection  <- Bute_selection %>% 
  mutate(
    Source = "Trial" ,
    
    
    soil_water_start = "0.396",
    soil_water_sowing = "Not_provided",
    soil_water_harvest = "Not_provided",
    
    soil_NO3_start = "116",
    soil_N03_sowing = "Not_provided",
    
    soil_NO3_harvest = "Not_provided",
    
    soil_NH4_start= "30",
    soil_NH4_sowing= "Not_provided",
    soil_NH4_harvest= "Not_provided",
    
    
    
    InCropRain= "Not_provided"
  )


str(Bute_selection)

#Just ordering it
Bute_selection  <- Bute_selection %>% 
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
         InCropRain,
         DM_Anthesis 
  )


str(Bute_selection)

write.csv(Bute_selection , "X:/Riskwi$e/Bute/3_Sims_post_Nov2024/results/APSIM_Trial_.csv", row.names = FALSE )

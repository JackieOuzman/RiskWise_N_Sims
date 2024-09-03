
# This file is for importing APISM sim files.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# File path for formatted outputs -----------------------------------------

path <- "X:/Riskwi$e/Bute/2_Sims_post_Sep2024/To_compare_etc/"


list_sim_out_file <-
  list.files(
    path = path,
    pattern = ".csv",
    all.files = FALSE,
    full.names = FALSE
  )

list_sim_out_file

# SIM and Trial data --------------------------------------------------------------

Calssic <- read_csv(paste0(path, "APSIM_Classic_Rotations.csv"))
NextG <- read_csv(paste0(path, "APSIM_NextGen.csv"))
Trial <- read_csv(paste0(path, "APSIM_Trial_.csv"))

# Check it matches --------------------------------------------------------------

str(Calssic)
names(Calssic)
names(NextG) #missing NH4 at sowing
names(Trial)



# Make sure treatments are matching ---------------------------------------

Trial <- Trial %>% filter(Treatment == "Control")




# Join the 2 data set together --------------------------------------------

#format so they are all the same data type


Trial <- Trial %>%
  mutate(across(-c(Source, Treatment, Crop, Cultivar ), as.numeric))
NextG <- NextG %>%
  mutate(across(-c(Source, Treatment, Crop, Cultivar ), as.numeric))
Calssic <- Calssic %>%
  mutate(across(-c(Source, Treatment, Crop, Cultivar ), as.numeric))


# Year              
# #Source     #chr      
# #Treatment  #chr      
# InCropFert        
# #Crop       #chr      
# #Cultivar   #chr     
# soil_water_start  
# soil_water_sowing 
# soil_water_harvest
# soil_NO3_start    
# soil_N03_sowing   
# soil_NO3_harvest  
# soil_NH4_start    
# soil_NH4_sowing   
# soil_NH4_harvest  
# Biomass           
# Yield             
# InCropRain        



merge_sim_trial <- bind_rows(Calssic, NextG, Trial)

write.csv(merge_sim_trial , 
          "X:/Riskwi$e/Bute/2_Sims_post_Sep2024/To_compare_etc/merge_sim_trial.csv", row.names = FALSE )




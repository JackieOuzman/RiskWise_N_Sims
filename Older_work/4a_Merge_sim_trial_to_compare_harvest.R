
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

Classic <- read_csv(paste0(path, "APSIM_Classic_Rotations.csv"))
NextG <- read_csv(paste0(path, "APSIM_NextGen.csv"))
Trial <- read_csv(paste0(path, "APSIM_Trial_.csv"))

# Check it matches --------------------------------------------------------------


names(Classic)
names(NextG) #missing NH4 at sowing
names(Trial)



# The trial data has some extra clms --------------------------------------

Trial <- Trial %>% select(- DM_Anthesis)

Classic <- Classic %>% mutate(Biomass = Biomass/1000)
NextG <- NextG %>% mutate(Biomass = Biomass/1000)

Classic <- Classic %>% mutate(Harvest_Index = (Yield*1000)/ Biomass)
NextG <- NextG %>% mutate(Harvest_Index = (Yield*1000)/ Biomass)






# Join the 2 data set together --------------------------------------------

#format so they are all the same data type


Trial <- Trial %>%
  mutate(across(-c(Source, Treatment, Crop, Cultivar ), as.numeric))
NextG <- NextG %>%
  mutate(across(-c(Source, Treatment, Crop, Cultivar ), as.numeric))
Classic <- Classic %>%
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



# Names of treatments need to match ---------------------------------------
unique(Classic$Treatment)
unique(NextG$Treatment)
unique(Trial$Treatment)

Classic <- Classic %>% mutate(
  Treatment = case_when(
    Treatment == "Bute Rotation_Bute_N0" ~ "Control",
    Treatment == "Bute Rotation_Bute_N_DP" ~ "District_Practice",
    Treatment == "Bute Rotation_Bute_N_Bank_Conservative" ~ "Nbank_Conservative",
    Treatment == "Bute Rotation_Bute_N_Bank_Profit" ~ "Nbank_Optimum_Profit",
    Treatment == "Bute Rotation_Bute_N_Bank_OptimumYld" ~ "Nbank_Optimum_Yield"
  )
)
NextG <- NextG %>% mutate(
  Treatment = case_when(
    Treatment == "Bute_Control" ~ "Control",
    Treatment == "Bute_DP" ~ "District_Practice",
    Treatment == "Bute_NBank_Conservative" ~ "Nbank_Conservative",
    Treatment == "Bute_NBank_Profit" ~ "Nbank_Optimum_Profit",
    Treatment == "Bute_NBank_Optimum_Yld" ~ "Nbank_Optimum_Yield"
  )
)

# choose only the treatments I modelled ---------------------------------------

Trial <- Trial %>% filter(
  Treatment == "Control" |
    Treatment == "District_Practice" |
    Treatment == "Nbank_Conservative" |
    Treatment == "Nbank_Optimum_Profit" |
    Treatment == "Nbank_Optimum_Yield"
)





merge_sim_trial <- bind_rows(Classic, NextG, Trial)







write.csv(merge_sim_trial , 
          "X:/Riskwi$e/Bute/2_Sims_post_Sep2024/To_compare_etc/merge_sim_trial_harvest.csv", row.names = FALSE )




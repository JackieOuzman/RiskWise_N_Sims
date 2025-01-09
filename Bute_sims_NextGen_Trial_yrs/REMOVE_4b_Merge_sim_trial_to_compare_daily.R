
# This file is for importing APISM sim files - DAILY
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# File path for formatted outputs -----------------------------------------

path <- "X:/Riskwi$e/Bute/3_Sims_post_Nov2024/results/"


list_sim_out_file <-
  list.files(
    path = path,
    pattern = ".csv",
    all.files = FALSE,
    full.names = FALSE
  )

list_sim_out_file

# SIM and Trial data --------------------------------------------------------------


NextG_D <- read_csv(paste0(path, "APSIM_NextGen_Daily.csv"))



# check the Format the date clm  ------------------------------------------
str(NextG_D$Date)


NextG_D <- NextG_D %>% mutate(soil_NO3 = NO3,
                              zadok_stage = Zadok,
                              soil_water = Soil_Water)


NextG_D <- NextG_D %>% select(
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
  NSTress)



##These were not included
#"InCropFert"   
# "soil_water_start"     
# "soil_NO3_start" 
# "NO3"     
# "Soil_mineral_N_sowing"
# ""     
 
 


# The units data has some extra clms --------------------------------------

NextG_D <- NextG_D %>% mutate(Biomass = Biomass/1000)



NextG_D <- NextG_D %>% mutate(
  Treatment = case_when(
    Treatment == "Bute_Control" ~ "Control",
    Treatment == "Bute_DP" ~ "District_Practice",
    Treatment == "Bute_NBank_Conservative" ~ "Nbank_Conservative",
    Treatment == "Bute_NBank_Profit" ~ "Nbank_Optimum_Profit",
    Treatment == "Bute_NBank_Optimum_Yld" ~ "Nbank_Optimum_Yield"
  )
)







write.csv(NextG_D , 
          "X:/Riskwi$e/Bute/3_Sims_post_Nov2024/results/merge_sim_Daily.csv", row.names = FALSE )




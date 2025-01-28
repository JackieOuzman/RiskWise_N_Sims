
# This file is for importing APISM sim files - DAILY
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# File path for formatted outputs -----------------------------------------

path <- "X:/Riskwi$e/Curyo/2_Sims_post_Sep2024/To_compare_etc/"


list_sim_out_file <-
  list.files(
    path = path,
    pattern = ".csv",
    all.files = FALSE,
    full.names = FALSE
  )

list_sim_out_file

# SIM and Trial data --------------------------------------------------------------

Classic_D <- read_csv(paste0(path, "APSIM_Classic_OperationScheduleDaily.csv"))
NextG_D <- read_csv(paste0(path, "APSIM_NextGen_Daily.csv"))



# Format the date clm in Classic ------------------------------------------
str(NextG_D$Date)
str(Classic_D$dd_mm_yyyy)


Classic_D$dd_mm_yyyy <- as.Date(Classic_D$dd_mm_yyyy, format = "%d_%m_%Y")

# Check it matches --------------------------------------------------------------


names(Classic_D)
names(NextG_D) 

Classic_D <- Classic_D %>% mutate(Date =dd_mm_yyyy,
                                  WaterStress = sw_stress_photo,
                                  NSTress = n_stress_photo)

NextG_D <- NextG_D %>% mutate(soil_NO3 = NO3,
                              zadok_stage = Zadok,
                              soil_water = Soil_Water)

Classic_D <- Classic_D %>% select(
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
 
 


# The matching units data has some extra clms --------------------------------------



Classic_D <- Classic_D %>% mutate(Biomass = Biomass/1000)
NextG_D <- NextG_D %>% mutate(Biomass = Biomass/1000)







# Join the 2 data set together --------------------------------------------





# Names of treatments need to match ---------------------------------------
unique(Classic_D$Treatment)
unique(NextG_D$Treatment)


Classic_D <- Classic_D %>% mutate(
  Treatment = case_when(
    Treatment == "Nil" ~ "Control",
    Treatment == "National_Average" ~ "National_Av",
    Treatment == "Maint100" ~ "Maint_100",
    Treatment == "Maint125" ~ "Maint_125",
    Treatment == "Maint150" ~ "Maint_150"
    
     # Treatment == "N_DP" ~ "District_Practice",
    # Treatment == "N_Bank_Conservative" ~ "Nbank_Conservative",
    # Treatment == "N_Bank_Profit" ~ "Nbank_Optimum_Profit",
    # Treatment == "N_Bank_OptimumYld" ~ "Nbank_Optimum_Yield"
  )
)
NextG_D <- NextG_D %>% mutate(
      Treatment = case_when(
      Treatment == "Nil" ~ "Control",
      Treatment == "National_Av" ~ "National_Av",
      Treatment == "Maint_100" ~ "Maint_100",
      Treatment == "Maint_125" ~ "Maint_125",
      Treatment == "Maint_150" ~ "Maint_150"
    # Treatment == "Curyo_Nil" ~ "Control" #,
    # Treatment == "Bute_DP" ~ "District_Practice",
    # Treatment == "Bute_NBank_Conservative" ~ "Nbank_Conservative",
    # Treatment == "Bute_NBank_Profit" ~ "Nbank_Optimum_Profit",
    # Treatment == "Bute_NBank_Optimum_Yld" ~ "Nbank_Optimum_Yield"
  )
)




# str(Classic_D)
# 
# Classic_D$Date <- as.character(Classic_D$Date)
# Classic_D$Date <- sub("_", "-", Classic_D$Date)
# Classic_D$Date <- sub("_", "-", Classic_D$Date)
# Classic_D$Date <-   format(as.Date(Classic_D$Date), "%d-%m-%Y")
# str(Classic_D)



merge_sim_trial <- bind_rows(Classic_D, NextG_D)







write.csv(merge_sim_trial , 
          "X:/Riskwi$e/Curyo/2_Sims_post_Sep2024/To_compare_etc/merge_sim_Daily.csv", row.names = FALSE )




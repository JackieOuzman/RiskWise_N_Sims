
# This file is for importing APISM sim files.
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

# Trial data from excel sheet to get some dates---------------------------------
Bute_trial_setup_outputs <- read_excel("X:/Riskwi$e/Bute/3_Sims_post_Nov2024/Bute_trial_setup_outputs.xlsx", 
                                       sheet = "Treatments_Jackie", skip = 2)
str(Bute_trial_setup_outputs)
#get the harvest dates
Harvest_Dates2022 <- Bute_trial_setup_outputs %>% 
  filter(Year == 2022) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`))

Harvest_Dates2023 <- Bute_trial_setup_outputs %>% 
  filter(Year == 2023) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`))


Anthesis_Dates2022 <- Bute_trial_setup_outputs %>% 
  filter(Year == 2022) %>% 
  select(`Anthesis date`) %>% 
  summarise(date = max(`Anthesis date`))


Anthesis_Dates2023 <- Bute_trial_setup_outputs %>% 
  filter(Year == 2023) %>% 
  select(`Anthesis date`) %>% 
  summarise(date = max(`Anthesis date`))

Harvest_Dates2022 <- Harvest_Dates2022$Harvest_date
Harvest_Dates2023 <- Harvest_Dates2023$Harvest_date
Anthesis_Dates2022 <- Anthesis_Dates2022$date
Anthesis_Dates2023 <- Anthesis_Dates2023$date



# Trial data with 'correct' formatting in R ------------------------------------

Bute_selection  <- Bute_trial_setup_outputs %>% 
  select(Year , 
         #"Trial" = Source)#,
         Treatment = System ,
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
         
         Soil_mineral_N_sowing =`Soil mineral N (kg/ha) - prior to sowing`, #umm is this NO3 + NH4?
         # 
         # Biomass = "Not_provided",
         Yield = `Yield (t/ha)` ,
         #InCropRain= "Not_provided"
         
         DM_Anthesis = 'Dry matter at anthesis (t/ha)',
         Biomass ='Dry matter at harvest (t/ha)', # is this the same?
         Harvest_Index ='Harvest index' 
         
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
         DM_Anthesis ,
         Harvest_Index 
  )


Trial <- Bute_selection
Trial <- Trial %>%  mutate(index = row_number())
rm(Bute_selection)

# Stuff around getting DM Anthesis into its own dataset and rename biomass to match trial data-------------------

names(Trial)
Anthesis <- Trial %>% select(-Biomass)

Anthesis <- Anthesis %>% mutate(
  Date=case_when(
    Year == 2022 ~ Anthesis_Dates2022,
    Year == 2023 ~ Anthesis_Dates2023
  ))

Anthesis <- Anthesis %>% rename(Biomass = DM_Anthesis)

Anthesis <- Anthesis %>%  filter( !is.na(Date))



# Remove anthesis clm from trial data set and add a date clm ---------------------------------

Trial <- Trial %>% select(-DM_Anthesis)
Trial <- Trial %>% mutate(
  Date=case_when(
    Year == 2022 ~ Harvest_Dates2022,
    Year == 2023 ~ Harvest_Dates2023
  ))

names(Trial)
names(Anthesis)

Trail_df <- bind_rows(Trial, Anthesis)



# Check it matches --------------------------------------------------------------



names(Trail_df)


unique(Trail_df$Treatment)


# choose only the treatments I modelled ---------------------------------------

Trail_df <- Trail_df %>% filter(
  Treatment == "Control" |
    Treatment == "District_Practice" |
    Treatment == "Nbank_Conservative" |
    Treatment == "Nbank_Optimum_Profit" |
    Treatment == "Nbank_Optimum_Yield" |
    
    Treatment == "YP_Decile1" |
    Treatment == "YP_Decile2-3" |
    Treatment == "YP_Decile5" |
    Treatment == "YP_Decile7-8" |
    Treatment == "YP_BOM"
)



unique(Trail_df$Treatment)
names(Trail_df)



write.csv(Trail_df , 
          "X:/Riskwi$e/Bute/3_Sims_post_Nov2024/results/merge_trial_harvest_and_anthesis.csv", row.names = FALSE )




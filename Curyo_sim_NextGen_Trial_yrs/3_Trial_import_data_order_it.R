
# This file is for importing Trial data.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# Trial data --------------------------------------------------------------



Trial_setup_outputs <- read_excel("X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Curyo_trial_setup_outputs.xlsx", 
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
          
         Dry_matter_Anthesis = `Dry matter at Anthesis` ,
         Yield = `Yield (t/ha)`, #,
         #soil_water_sowing = `Sowing total water (mm) - soil`, #This no longer exists in the 2025 dataset Melb Uni
         `Harvest Date`
         
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
    
    soil_NO3_start = "69.7",
   # soil_N03_sowing = "Not_provided",provided in some years
    soil_NO3_harvest = "Not_provided",
    
    soil_NH4_start= "14.5",
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
         #soil_water_sowing,
         soil_water_harvest ,
         
         soil_NO3_start ,
         soil_N03_sowing, #umm is this NO3 + NH4?
         soil_NO3_harvest ,
         
          soil_NH4_start,
          soil_NH4_sowing,
          soil_NH4_harvest,
         Soil_mineral_N_sowing,
          
         Dry_matter_Anthesis ,
         Yield  ,
         InCropRain,
         #DM_Anthesis ,
         #Harvest_Index,
         `Harvest Date`
         
         
  )


str(df_selection)



###############################################################################
#remove the empty row
Curyo_trial_setup_outputs <-df_selection %>% filter(!is.na(Year))

## years of trial 2018, 2019, 2020, 2021, 2022

str(Curyo_trial_setup_outputs)
#get the harvest dates
Harvest_Dates2018 <- Curyo_trial_setup_outputs %>% 
  filter(Year == 2018) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`,na.rm = TRUE ))
Harvest_Dates2019 <- Curyo_trial_setup_outputs %>% 
  filter(Year == 2019) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`,na.rm = TRUE ))
Harvest_Dates2020 <- Curyo_trial_setup_outputs %>% 
  filter(Year == 2020) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`,na.rm = TRUE ))
Harvest_Dates2021 <- Curyo_trial_setup_outputs %>% 
  filter(Year == 2021) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`,na.rm = TRUE ))
Harvest_Dates2022 <- Curyo_trial_setup_outputs %>% 
  filter(Year == 2022) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`,na.rm = TRUE ))
Harvest_Dates2023 <- Curyo_trial_setup_outputs %>% 
  filter(Year == 2023) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`,na.rm = TRUE ))
Harvest_Dates2024 <- Curyo_trial_setup_outputs %>% 
  filter(Year == 2024) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`,na.rm = TRUE ))


Harvest_Dates2018
Harvest_Dates2019
Harvest_Dates2020
Harvest_Dates2021
Harvest_Dates2022
Harvest_Dates2023
Harvest_Dates2024

Harvest_Dates2018 <- Harvest_Dates2018$Harvest_date
Harvest_Dates2019 <- Harvest_Dates2019$Harvest_date
Harvest_Dates2020 <- Harvest_Dates2020$Harvest_date
Harvest_Dates2021 <- Harvest_Dates2021$Harvest_date
Harvest_Dates2022 <- Harvest_Dates2022$Harvest_date
Harvest_Dates2023 <- Harvest_Dates2023$Harvest_date
Harvest_Dates2024 <- Harvest_Dates2024$Harvest_date



Dry_matter_Anthesis_date_2018 <- as.POSIXct(as.Date("2018-10-10")) #can now get it from the Melb uni data
Dry_matter_Anthesis_date_2019 <- NA #as.POSIXct(as.Date(""))
Dry_matter_Anthesis_date_2020 <- NA #as.POSIXct(as.Date(""))
Dry_matter_Anthesis_date_2021 <- NA #as.POSIXct(as.Date(""))
Dry_matter_Anthesis_date_2022 <- NA #as.POSIXct(as.Date(""))
Dry_matter_Anthesis_date_2023 <- NA #as.POSIXct(as.Date(""))
Dry_matter_Anthesis_date_2024 <- as.POSIXct(as.Date("2024-09-02"))


# Trial data with formatting in R --------------------------------------------------------------
Trial <- df_selection


# Stuff around getting DM Anthesis / flowering into its own dataset and rename biomass to match trial data-------------------
#1. df with flowering dates and clm called biomass
names(Trial)

Anthesis <- Trial #%>% select(-Biomass) #if I had biomass data at harvest then I would use this

Anthesis <- Anthesis %>% mutate(
  Date=case_when(
    Year == 2018 ~ Dry_matter_Anthesis_date_2018,
    Year == 2019 ~ Dry_matter_Anthesis_date_2019,
    Year == 2020 ~ Dry_matter_Anthesis_date_2020,
    Year == 2021 ~ Dry_matter_Anthesis_date_2021,
    Year == 2022 ~ Dry_matter_Anthesis_date_2022,
    Year == 2023 ~ Dry_matter_Anthesis_date_2023,
    Year == 2024 ~ Dry_matter_Anthesis_date_2024,
    
  ))

Anthesis <- Anthesis %>% rename(Biomass = Dry_matter_Anthesis)






#2. df with harevest dates and clm called biomass

#Trial <- Trial %>% select(-DM_Anthesis)
Trial <- Trial %>% mutate(
  Date=case_when(
    Year == 2019 ~ Harvest_Dates2019,
    Year == 2020 ~ Harvest_Dates2020,
    Year == 2021 ~ Harvest_Dates2021,
    Year == 2022 ~ Harvest_Dates2022,
    Year == 2023 ~ Harvest_Dates2023,
    Year == 2024 ~ Harvest_Dates2024
    
  ))
## all of the biomass data is flowering 
Trial <- Trial %>% mutate(Biomass = NA) %>% select(- Dry_matter_Anthesis)

names(Trial)


Trail_df <- bind_rows(Trial, Anthesis)

# rename the treatments so it matches the modelled values ---------------------------------------
unique(Trail_df$Treatment)

Trail_df <- Trail_df %>% mutate(
  Treatment = case_when(
    
    Treatment == "01_Nil" ~ "Control",
    Treatment == "03_National_Average" ~ "National_Av",
    Treatment == "04_100kgMaint" ~ "Maint_100",
    Treatment == "05_125kgMaint" ~ "Maint_125",
    Treatment == "06_150kgMaint" ~ "Maint_150",
    
    Treatment == "07_100%YP" ~ "YP_100",
    Treatment == "08_75%YP" ~ "YP_75",
    Treatment == "09_50%YP" ~ "YP_50",
    Treatment == "10_25%YP" ~ "YP_25",
    
    Treatment == "02_Replacement" ~ "Replacment"#,
    
  )
)







write.csv(Trail_df , 
          "X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Results/merge_trial_harvest.csv", row.names = FALSE )
 


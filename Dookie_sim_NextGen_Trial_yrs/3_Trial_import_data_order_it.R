
# This file is for importing Trial data.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# Trial data --------------------------------------------------------------



Trial_setup_outputs <- read_excel("X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/Dookie_trial_setup_outputs.xlsx", 
                                       sheet = "Treatments_Jackie", skip = 3)
names(Trial_setup_outputs)
str(Trial_setup_outputs)

Trial_setup_outputs$`Biomass harvest date` <- as.Date(Trial_setup_outputs$`Biomass harvest date`)
unique(Trial_setup_outputs$`Biomass harvest date`)


df_selection  <- Trial_setup_outputs %>% 
  select(Year , 
         Date = `Harvest Date`,
         #"Trial" = Source)#,
         Treatment = `Jax_ID`, #was System 
         InCropFert =  `Total N applied at sowing and inseason` ,
         Crop = `Crop type` ,
         Cultivar = Variety ,
         soil_N03_sowing =`Amount of NO3 at sowing (kg/ha) - Soil` ,
         soil_NH4_sowing = `Amount of NH4 at sowing (kg/ha) - Soil`,  
         
         Soil_mineral_N_sowing =`Amount of total mineral N at sowing (kg/ha) -Soil`, 
          
         Biomass_flowering = `Biomass flowering (t/ha)` ,#no data in this clm
         Yield = `Yield (t/ha)`, #,
         soil_water_sowing = `Sowing total water (mm) - soil`, 
                                                                    
         Biomass_harvest = `Biomass harvest (t/ha)`,
         Biomass_harvest_date =`Biomass harvest date` 
         
         
         #InCropRain= "Not_provided"
         
         #DM_Anthesis = "Not_provided" ,#'Dry matter at anthesis (t/ha)'
         #Biomass = "Not_provided", #'Dry matter at harvest (t/ha)', 
         #Harvest_Index = "Not_provided" #'Harvest index' 
  )

## remove missing data

df_selection  <- df_selection %>%
  filter(!is.na(Treatment))

df_selection  <- df_selection %>% 
  mutate(
    Source = "Trial" ,
    
    
    soil_water_start = "1.982", #sum value in 2018 check this
    #soil_water_sowing = "Not_provided", provided in some years
    soil_water_harvest = "Not_provided",
    
    soil_NO3_start = "197",
   # soil_N03_sowing = "Not_provided",provided in some years
    soil_NO3_harvest = "Not_provided",
    
    soil_NH4_start= "14",
    #soil_NH4_sowing= "Not_provided", provided in some years
    soil_NH4_harvest= "Not_provided",
    
    
    
    InCropRain= "Not_provided"
  )


str(df_selection)

#Just ordering it
df_selection  <- df_selection %>% 
  select(Year , 
         Date,
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
          
         Biomass_flowering ,
         Yield  ,
         InCropRain,
         #DM_Anthesis ,
         #Harvest_Index,
         Biomass_harvest ,
         Biomass_harvest_date 
  )


str(df_selection)
################################################################################

#remove the empty row
Dookie_trial_setup_outputs <-df_selection %>% filter(!is.na(Year))

## years of trial 2022, 2023, 2024

str(Dookie_trial_setup_outputs)
#get the harvest dates
Harvest_Dates2022 <- Dookie_trial_setup_outputs %>% 
  filter(Year == 2022) %>% 
  select(Date) %>% # `Harvest Date`
  summarise(Harvest_date = max(Date,na.rm = TRUE ))
Harvest_Dates2023 <- Dookie_trial_setup_outputs %>% 
  filter(Year == 2023) %>% 
  select(Date) %>% 
  summarise(Harvest_date = max(Date,na.rm = TRUE ))
Harvest_Dates2024 <- Dookie_trial_setup_outputs %>% 
  filter(Year == 2024) %>% 
  select(Date) %>% 
  summarise(Harvest_date = max(Date,na.rm = TRUE ))

Harvest_Dates2022 #missing
Harvest_Dates2023
Harvest_Dates2024 #missing

Harvest_Dates2022 <- Harvest_Dates2022$Harvest_date
Harvest_Dates2023 <- Harvest_Dates2023$Harvest_date
Harvest_Dates2024 <- Harvest_Dates2024$Harvest_date



Biomass_at_harvest_date_2022 <- Dookie_trial_setup_outputs %>% 
  filter(Year == 2022) %>% 
  select(Biomass_harvest_date) %>% 
  summarise(Biomass_harvest_date = max(Biomass_harvest_date,na.rm = TRUE ))  
  
Biomass_at_harvest_date_2023 <- Dookie_trial_setup_outputs %>% 
  filter(Year == 2023) %>% 
  select(Biomass_harvest_date) %>% 
  summarise(Biomass_harvest_date = max(Biomass_harvest_date,na.rm = TRUE ))  
Biomass_at_harvest_date_2024 <- Dookie_trial_setup_outputs %>% 
  filter(Year == 2024) %>% 
  select(Biomass_harvest_date) %>% 
  summarise(Biomass_harvest_date = max(Biomass_harvest_date,na.rm = TRUE ))  

Biomass_at_harvest_date_2022 <- Biomass_at_harvest_date_2022$Biomass_harvest_date
Biomass_at_harvest_date_2023 <- Biomass_at_harvest_date_2023$Biomass_harvest_date
Biomass_at_harvest_date_2024 <- Biomass_at_harvest_date_2024$Biomass_harvest_date


Biomass_at_harvest_date_2022
Biomass_at_harvest_date_2023
Biomass_at_harvest_date_2024

# Flowering_date_2018 <- as.POSIXct(as.Date("2018-10-10"))
# Flowering_date_2019 <- NA #as.POSIXct(as.Date(""))
# Flowering_date_2020 <- NA #as.POSIXct(as.Date(""))
# Flowering_date_2021 <- NA #as.POSIXct(as.Date(""))
# Flowering_date_2022 <- NA #as.POSIXct(as.Date(""))


# Anthesis_Dates2022 <- Curyo_trial_setup_outputs %>% 
#   filter(Year == 2022) %>% 
#   select(`Anthesis date`) %>% 
#   summarise(date = max(`Anthesis date`))
# 
# 
# Anthesis_Dates2023 <- Curyo_trial_setup_outputs %>% 
#   filter(Year == 2023) %>% 
#   select(`Anthesis date`) %>% 
#   summarise(date = max(`Anthesis date`))

# Anthesis_Dates2022 <- Anthesis_Dates2022$date
# Anthesis_Dates2023 <- Anthesis_Dates2023$date



# Trial data with formatting in R --------------------------------------------------------------
Trial <- df_selection


# Stuff around getting DM Anthesis / flowering into its own dataset and rename biomass to match trial data-------------------
#1. df with flowering dates and clm called biomass
names(Trial)

## Only biomass data at Dookie is at harvest
 Harvest_BM <- Trial %>% select(-Biomass_harvest) #if I had biomass data at harvest then I would use this
# 
 Harvest_BM <- Harvest_BM %>% mutate(
   Date=case_when(
     Year == 2022 ~ Biomass_at_harvest_date_2022,
     Year == 2023 ~ Biomass_at_harvest_date_2023,
     Year == 2024 ~ Biomass_at_harvest_date_2024
   ))
# 
# Flowering <- Flowering %>% rename(Biomass = Biomass_flowering)


#Anthesis <- Anthesis %>% rename(Biomass = DM_Anthesis)

# Anthesis <- Trial %>% select(-Biomass)
# 
# Anthesis <- Anthesis %>% mutate(
#   Date=case_when(
#     Year == 2022 ~ Anthesis_Dates2022,
#     Year == 2023 ~ Anthesis_Dates2023
#   ))
# 
# Anthesis <- Anthesis %>% rename(Biomass = DM_Anthesis)
# 
# 



#2. df with harevest dates and clm called biomass

#Trial <- Trial %>% select(-DM_Anthesis)
# Trial <- Trial %>% mutate(
#   Date=case_when(
#     Year == 2019 ~ Harvest_Dates2019,
#     Year == 2020 ~ Harvest_Dates2020,
#     Year == 2021 ~ Harvest_Dates2021,
#     Year == 2022 ~ Harvest_Dates2022
#     
#   ))
## all of the biomass data is flowering 
Trial <- Trial %>% 
  #mutate(Biomass = NA) %>% 
  select(- Biomass_flowering)

names(Trial)


#Trail_df <- bind_rows(Trial, Flowering)

Trail_df <- Trial

# Check it matches --------------------------------------------------------------
unique(Trail_df$Treatment)


Trail_df <- Trail_df %>% mutate(
  Treatment = case_when(
    Treatment == "01_Nil" ~ "Control",
    Treatment == "02_National Average" ~ "National_Av",
    Treatment == "03_Replacement + Protein" ~ "Replacment",
    
    Treatment == "04_YP Lite Decile 2-3" ~ "YP_Decile2_3",
    Treatment == "05_YP Lite Decile 5" ~ "YP_Decile5",
    Treatment == "06_YP Lite Decile 7-8" ~ "YP_Decile7_8",
    
    Treatment == "07_YP Lite BOM" ~ "YP_BOM",
    
    Treatment == "08_N Bank Conservative (NB250)" ~ "Nbank_Conservative",
    Treatment == "09_N Bank Optimal Profit (NB275)" ~ "Nbank_Optimum_Profit",
    Treatment == "10_N Bank Optimal Yield (NB300)" ~ "Nbank_Optimum_Yield",
    
    
  )
)

unique(Trail_df$Treatment)



write.csv(Trail_df , "X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/Results/APSIM_Trial.csv", row.names = FALSE )

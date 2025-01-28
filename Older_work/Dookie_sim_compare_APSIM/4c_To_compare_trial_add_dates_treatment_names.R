
# This file is for importing APISM sim files.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# File path for formatted outputs -----------------------------------------

path <- "X:/Riskwi$e/Dookie/2_Sims_post_Sep2024/To_compare_etc/"


list_sim_out_file <-
  list.files(
    path = path,
    pattern = ".csv",
    all.files = FALSE,
    full.names = FALSE
  )

list_sim_out_file

# Get dates from Trial data from excel sheet--------------------------------------------------------------
Dookie_trial_setup_outputs <- read_excel("X:/Riskwi$e/Dookie/2_Sims_post_Sep2024/Dookie_trial_setup_outputs.xlsx", 
                                       sheet = "Treatments_Jackie", skip = 2)

#remove the empty row
Dookie_trial_setup_outputs <-Dookie_trial_setup_outputs %>% filter(!is.na(Year))

## years of trial 2018, 2019, 2020, 2021, 2022

str(Dookie_trial_setup_outputs)
#get the harvest dates
Harvest_Dates2022 <- Dookie_trial_setup_outputs %>% 
  filter(Year == 2022) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`,na.rm = TRUE ))
Harvest_Dates2023 <- Dookie_trial_setup_outputs %>% 
  filter(Year == 2023) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`,na.rm = TRUE ))


Harvest_Dates2022
Harvest_Dates2023


Harvest_Dates2022 <- Harvest_Dates2022$Harvest_date
Harvest_Dates2023 <- Harvest_Dates2023$Harvest_date




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
Trial <- read_csv(paste0(path, "APSIM_Trial.csv"))


# Stuff around getting DM Anthesis / flowering into its own dataset and rename biomass to match trial data-------------------
#1. df with flowering dates and clm called biomass
names(Trial)

## No biomass data for Dookie
# Flowering <- Trial #%>% select(-Biomass) #if I had biomass data at harvest then I would use this
# 
# Flowering <- Flowering %>% mutate(
#   Date=case_when(
#     Year == 2018 ~ Flowering_date_2018,
#     Year == 2019 ~ Flowering_date_2019,
#     Year == 2020 ~ Flowering_date_2020,
#     Year == 2021 ~ Flowering_date_2021,
#     Year == 2022 ~ Flowering_date_2022,
#   ))
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
Trial <- Trial %>% mutate(Biomass = NA) %>% select(- Biomass_flowering)

names(Trial)


#Trail_df <- bind_rows(Trial, Flowering)

Trail_df <- Trial

# Check it matches --------------------------------------------------------------



names(Trail_df)

unique(Trail_df$Treatment)

#Nil


# rename the treatments so it matches the modelled values ---------------------------------------


Trail_df <- Trail_df %>% mutate(
  Treatment = case_when(
    
      Treatment == "01_Nil" ~ "Control",
      Treatment == "02_National Average" ~ "National_Av",
      Treatment == "08_N Bank Conservative" ~ "Nbank_Conservative",
      Treatment == "09_N Bank Optimal Profit" ~ "Nbank_Optimum_Profit",
      Treatment == "10_N Bank Optimal Yield" ~ "Nbank_Optimum_Yield"
    
    #Treatment == "01_Nil" ~ "Control"#,
    # Treatment == "N_DP" ~ "District_Practice",
    # Treatment == "N_Bank_Conservative" ~ "Nbank_Conservative",
    # Treatment == "N_Bank_Profit" ~ "Nbank_Optimum_Profit",
    # Treatment == "N_Bank_OptimumYld" ~ "Nbank_Optimum_Yield"
  )
)
# choose only the treatments I modelled ---------------------------------------

Trail_df <- Trail_df %>% filter(
    Treatment == "Control" |
    Treatment == "National_Av" |
    Treatment == "Nbank_Conservative" |
    Treatment == "Nbank_Optimum_Profit" |
    Treatment == "Nbank_Optimum_Yield"
)








write.csv(Trail_df , 
          "X:/Riskwi$e/Dookie/2_Sims_post_Sep2024/To_compare_etc/merge_trial_harvest.csv", row.names = FALSE )




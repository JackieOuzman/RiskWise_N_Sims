
# This file is for importing APISM sim files.
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

# Get dates from Trial data from excel sheet--------------------------------------------------------------
Curyo_trial_setup_outputs <- read_excel("X:/Riskwi$e/Curyo/2_Sims_post_Sep2024/Curyo_trial_setup_outputs.xlsx", 
                                       sheet = "Treatments_Jackie", skip = 6)

## years of trial 2018, 2019, 2020, 2021, 2022

str(Curyo_trial_setup_outputs)
#get the harvest dates
Harvest_Dates2019 <- Curyo_trial_setup_outputs %>% 
  filter(Year == 2019) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`))
Harvest_Dates2020 <- Curyo_trial_setup_outputs %>% 
  filter(Year == 2020) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`))
Harvest_Dates2021 <- Curyo_trial_setup_outputs %>% 
  filter(Year == 2021) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`))
Harvest_Dates2022 <- Curyo_trial_setup_outputs %>% 
  filter(Year == 2022) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`))


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

Harvest_Dates2019 <- Harvest_Dates2019$Harvest_date
Harvest_Dates2020 <- Harvest_Dates2020$Harvest_date
Harvest_Dates2021 <- Harvest_Dates2021$Harvest_date
Harvest_Dates2022 <- Harvest_Dates2022$Harvest_date

# Anthesis_Dates2022 <- Anthesis_Dates2022$date
# Anthesis_Dates2023 <- Anthesis_Dates2023$date



# Trial data with formatting in R --------------------------------------------------------------
Trial <- read_csv(paste0(path, "APSIM_Trial.csv"))


# Stuff around getting DM Anthesis into its own dataset and rename biomass to match trial data-------------------

names(Trial)
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
# 
# 
# 
# # Remove anthesis clm from trial data set and add a date clm ---------------------------------
# 
# Trial <- Trial %>% select(-DM_Anthesis)
Trial <- Trial %>% mutate(
  Date=case_when(
    Year == 2019 ~ Harvest_Dates2019,
    Year == 2020 ~ Harvest_Dates2020,
    Year == 2021 ~ Harvest_Dates2021,
    Year == 2022 ~ Harvest_Dates2022
    
  ))

names(Trial)


#Trail_df <- bind_rows(Trial, Anthesis)



# Check it matches --------------------------------------------------------------



names(Trial)

unique(Trial$Treatment)

#Nil


# rename the treatments so it matches the modelled values ---------------------------------------


Trial <- Trial %>% mutate(
  Treatment = case_when(
    Treatment == "01_Nil" ~ "Control"#,
    # Treatment == "N_DP" ~ "District_Practice",
    # Treatment == "N_Bank_Conservative" ~ "Nbank_Conservative",
    # Treatment == "N_Bank_Profit" ~ "Nbank_Optimum_Profit",
    # Treatment == "N_Bank_OptimumYld" ~ "Nbank_Optimum_Yield"
  )
)
# choose only the treatments I modelled ---------------------------------------

Trial <- Trial %>% filter(
    Treatment == "Control" #|
    # Treatment == "District_Practice" |
    # Treatment == "Nbank_Conservative" |
    # Treatment == "Nbank_Optimum_Profit" |
    # Treatment == "Nbank_Optimum_Yield"
)








write.csv(Trial , 
          "X:/Riskwi$e/Curyo/2_Sims_post_Sep2024/To_compare_etc/merge_trial_harvest.csv", row.names = FALSE )




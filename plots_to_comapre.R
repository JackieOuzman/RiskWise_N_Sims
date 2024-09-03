
# This file is for importing APISM sim files.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
# SIM data --------------------------------------------------------------

sim_output <- read_csv("X:/Riskwi$e/Bute/2_Sims_post_Sep2024/APSIM_Classic_output_long.csv")

# Trial data --------------------------------------------------------------



# Plot --------------------------------------------------------------------

join_long %>% 
  ggplot(aes(x = as.factor(Year), y = yield_t_ha, colour =source ))+
  geom_point()

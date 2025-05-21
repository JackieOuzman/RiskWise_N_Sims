library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


Dry_sowing_Lock_factor_with_met <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Dry_sowing_Lock_factor_with_met_18046_v2_gs.csv")
Decile_18046 <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Deciles/deciles per year_Lock18046.csv")

str(Dry_sowing_Lock_factor_with_met)
str(Decile_18046)

### merge the data

Dry_sowing_Lock_factor_with_met_decile <- left_join(Dry_sowing_Lock_factor_with_met, Decile_18046)
str(Dry_sowing_Lock_factor_with_met_decile)

Yld_Dry_sowing_Lock_factor_with_met_decile_summary <- Dry_sowing_Lock_factor_with_met_decile %>% 
   filter(Wheat.Phenology.CurrentStageName == "HarvestRipe") %>% 
   select(year, Sowing_date, decile, Yield_Adj_t_ha, Wheat.Phenology.CurrentStageName ) 

Yld_Dry_sowing_Lock_factor_with_met_decile_summary


## cal some stats for each decile what is the bottom 10% quantile 

Quantile_sow_date_decile <- Yld_Dry_sowing_Lock_factor_with_met_decile_summary %>% 
  group_by(Sowing_date,decile ) %>%
  summarize(quantile10=quantile(Yield_Adj_t_ha,probs=0.10),
            quantile50=quantile(Yield_Adj_t_ha,probs=0.5),
            quantile90=quantile(Yield_Adj_t_ha,probs=0.90))


Quantile_sow_date_decile <- Quantile_sow_date_decile %>% 
  mutate(site ="Lock_18046",
        Yrs_included_decile =  "1957 to 2024",
        GS_def_decile = "1/4 to 1/11",
        APSIM_sow_rule = "water balance TH 5mm") %>% 
  rename(dry_Sowing_date =Sowing_date)
Quantile_sow_date_decile



write_csv(Quantile_sow_date_decile, 
          file =paste0("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/for_Decile_cal", 
                       "/APSIM_yld_Decile_waterBalnce_TB5mm_Lock18046", ".csv"))

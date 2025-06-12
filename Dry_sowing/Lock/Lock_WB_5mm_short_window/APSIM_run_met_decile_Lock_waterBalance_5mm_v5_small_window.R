library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


Dry_sowing_Lock_factor_with_met <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/Results/short_window_5mm/Short_window_Dry_sowing_Lock_factor_with_met_18046_v2_gs.csv")
Decile_18046 <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/Deciles/deciles per year_Lock18046.csv")

str(Dry_sowing_Lock_factor_with_met)
str(Decile_18046)

### merge the data

Dry_sowing_Lock_factor_with_met_decile <- left_join(Dry_sowing_Lock_factor_with_met, Decile_18046, by = "year")
str(Dry_sowing_Lock_factor_with_met_decile)


## side step add germination to one data files
germination_date <- Dry_sowing_Lock_factor_with_met_decile %>% 
  filter(Wheat.Phenology.CurrentStageName == "Germination") %>% 
  select(year, Sowing_date, Clock.Today ) %>% 
  rename(germination_date = Clock.Today)

Yld_Dry_sowing_Lock_factor_with_met_decile_summary <- Dry_sowing_Lock_factor_with_met_decile %>% 
   filter(Wheat.Phenology.CurrentStageName == "HarvestRipe") %>% 
   select(year, Sowing_date, decile, Yield_Adj_t_ha, Wheat.Phenology.CurrentStageName ) 

Yld_Dry_sowing_Lock_factor_with_met_decile_summary_germ_date <- left_join(
  Yld_Dry_sowing_Lock_factor_with_met_decile_summary, germination_date)


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
        APSIM_sow_rule = "water balance TH 5mm 5day window") %>% 
  rename(dry_Sowing_date =Sowing_date)
Quantile_sow_date_decile



write_csv(Quantile_sow_date_decile, 
          file =paste0("X:/Riskwi$e/Dry_sowing/Lock/for_Decile_cal/Lock_WB_5mm_small_window", 
                       "/APSIM_yld_Decile_waterBalnce_TB5mm_Lock18046_small_window", ".csv"))









## before writing out the un summaries data with germination date. Just add a few things to help plot
str(Yld_Dry_sowing_Lock_factor_with_met_decile_summary_germ_date)
Yld_Dry_sowing_Lock_factor_with_met_decile_summary_germ_date <- Yld_Dry_sowing_Lock_factor_with_met_decile_summary_germ_date %>% 
  mutate(DOY_germination = lubridate::yday(germination_date),
         CommonDate = ymd(paste0("2000-",str_sub(as.character(germination_date),-5))))

           
           
write_csv(Yld_Dry_sowing_Lock_factor_with_met_decile_summary_germ_date, 
          file =paste0("X:/Riskwi$e/Dry_sowing/Lock/for_Decile_cal/Lock_WB_5mm_small_window", 
                       "/APSIM_yld_Decile_GermDate_waterBalnce_TB5mm_Lock18046_No_summary_small_window", ".csv"))


### force germination date into 'windows'
Yld_Dry_sowing_Lock_factor_with_met_decile_summary_germ_date


Yld_Dry_sowing_Lock_factor_with_met_decile_summary_germ_date <- Yld_Dry_sowing_Lock_factor_with_met_decile_summary_germ_date %>% 
  mutate(germination_between = case_when(
    between(CommonDate, as.Date("2000-04-01"), as.Date("2000-04-05"))     ~ "1 to 5 Apr",
    between(CommonDate, as.Date("2000-04-05"), as.Date("2000-04-10"))     ~ "5 to 10 Apr",
    between(CommonDate, as.Date("2000-04-10"), as.Date("2000-04-15"))     ~ "10 to 15 Apr",
    between(CommonDate, as.Date("2000-04-15"), as.Date("2000-04-20"))     ~ "15 to 20 Apr",
    between(CommonDate, as.Date("2000-04-20"), as.Date("2000-04-25"))     ~ "20 to 25 Apr",
    between(CommonDate, as.Date("2000-04-25"), as.Date("2000-04-30"))     ~ "25 to 30 Apr",
    
    between(CommonDate, as.Date("2000-05-01"), as.Date("2000-05-05"))     ~ "1 to 5 May",
    between(CommonDate, as.Date("2000-05-05"), as.Date("2000-05-10"))     ~ "5 to 10 May",
    between(CommonDate, as.Date("2000-05-10"), as.Date("2000-05-15"))     ~ "10 to 15 May",
    between(CommonDate, as.Date("2000-05-15"), as.Date("2000-05-20"))     ~ "15 to 20 May",
    between(CommonDate, as.Date("2000-05-20"), as.Date("2000-05-25"))     ~ "20 to 25 May",
    between(CommonDate, as.Date("2000-05-25"), as.Date("2000-05-31"))     ~ "25 to 31 May",
    
    between(CommonDate, as.Date("2000-06-01"), as.Date("2000-06-05"))     ~ "1 to 5 Jun",
    between(CommonDate, as.Date("2000-06-05"), as.Date("2000-06-10"))     ~ "5 to 10 Jun",
    between(CommonDate, as.Date("2000-06-10"), as.Date("2000-06-15"))     ~ "10 to 15 Jun",
    between(CommonDate, as.Date("2000-06-15"), as.Date("2000-06-20"))     ~ "15 to 20 Jun",
    between(CommonDate, as.Date("2000-06-20"), as.Date("2000-06-25"))     ~ "20 to 25 Jun",
    between(CommonDate, as.Date("2000-06-25"), as.Date("2000-06-30"))     ~ "25 to 30 Jun",
    
    between(CommonDate, as.Date("2000-06-30"), as.Date("2000-07-30"))     ~ "Jul",
    TRUE  ~ "Unknown"
    
  ))

Yld_Dry_sowing_Lock_factor_with_met_decile_summary_germ_date$germination_between <- factor(Yld_Dry_sowing_Lock_factor_with_met_decile_summary_germ_date$germination_between , ordered = TRUE, 
                                                   levels = c(
                                                     "1 to 5 Apr",
                                                     "5 to 10 Apr",
                                                     "10 to 15 Apr",
                                                     "15 to 20 Apr",
                                                     "20 to 25 Apr",
                                                     "25 to 30 Apr",
                                                     
                                                     
                                                     "1 to 5 May",
                                                     "5 to 10 May",
                                                     "10 to 15 May",
                                                     "15 to 20 May",
                                                     "20 to 25 May",
                                                     "25 to 31 May",
                                                     
                                                     "1 to 5 Jun",
                                                     "5 to 10 Jun",
                                                     "10 to 15 Jun",
                                                     "15 to 20 Jun",
                                                     "20 to 25 Jun",
                                                     "25 to 30 Jun",
                                                     "Jul"
                                                   ))



write_csv(Yld_Dry_sowing_Lock_factor_with_met_decile_summary_germ_date, 
          file =paste0("X:/Riskwi$e/Dry_sowing/Lock/for_Decile_cal/Lock_WB_5mm_small_window", 
                       "/APSIM_yld_Decile_GermDate_waterBalnce_TB5mm_Lock18046_No_summary", ".csv"))

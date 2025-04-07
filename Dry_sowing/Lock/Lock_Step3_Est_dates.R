library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)





# Download dry sowing with met made in step 3 -------------------------------------------------------

Dry_sowing_Lock_factor_with_met <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/Results/Dry_sowing_Lock_factor_with_met_18046.csv")
str(Dry_sowing_Lock_factor_with_met)

unique(Dry_sowing_Lock_factor_with_met$Wheat.Phenology.CurrentStageName)

### pull out the establishment date
names(Dry_sowing_Lock_factor_with_met)
Dry_sowing_Lock_factor_with_met <- Dry_sowing_Lock_factor_with_met %>% 
  select(year,
         Sowing_date,
         Clock.Today,
         Wheat.Phenology.CurrentStageName,
         Yield_Adj_t_ha)

phenology_sowing <- Dry_sowing_Lock_factor_with_met %>% 
  filter(Wheat.Phenology.CurrentStageName == "Sowing") %>% 
  select(year,Clock.Today,Sowing_date ) %>% 
  rename(Date = Clock.Today, Sim = Sowing_date) %>% 
  mutate(Phenology = "Sowing")
phenology_sowing 
  

phenology_emergence <- Dry_sowing_Lock_factor_with_met %>% 
  filter(Wheat.Phenology.CurrentStageName == "Emergence") %>% 
  select(year,Clock.Today,Sowing_date ) %>% 
  rename(Date = Clock.Today, Sim = Sowing_date) %>% 
  mutate(Phenology = "emergence")
phenology_emergence 

phenology_harvest <- Dry_sowing_Lock_factor_with_met %>% 
  filter(Wheat.Phenology.CurrentStageName == "HarvestRipe") %>% 
  select(year,Clock.Today,Sowing_date ) %>% 
  rename(Date = Clock.Today, Sim = Sowing_date) %>% 
  mutate(Phenology = "harvest")
phenology_harvest 

yld_harvest <- Dry_sowing_Lock_factor_with_met %>% 
  filter(Wheat.Phenology.CurrentStageName == "HarvestRipe") %>% 
  select(year,Yield_Adj_t_ha,Sowing_date, Clock.Today ) %>% 
  rename(Date = Clock.Today, Sim = Sowing_date) 
  
yld_harvest 

### wide format

phenology_sowing_w <- phenology_sowing %>% rename(Date_sowing = Date) %>% select(-Phenology)
phenology_sowing_w
phenology_emergence_w <- phenology_emergence %>% rename(Date_emergence = Date) %>% select(-Phenology)
phenology_emergence_w
phenology_harvest_w <- phenology_harvest %>% rename(Date_harvest = Date) %>% select(-Phenology)
phenology_harvest_w

phenology_wide <- left_join(phenology_sowing_w, phenology_emergence_w)
phenology_wide <- left_join(phenology_wide, phenology_harvest_w)
phenology_wide <- phenology_wide %>% select(year, Sim, Date_sowing, Date_emergence, Date_harvest)
phenology_wide <- left_join(phenology_wide, yld_harvest)
phenology_wide <- phenology_wide %>% select(-Date)

phenology_wide

### count the days between sowing and establishment 

phenology_wide <- phenology_wide %>% 
  mutate(from_sowing_to_emergence =lubridate::as.difftime(Date_emergence- Date_sowing))

################################################################################
# order the deciles to help with plotting
unique(phenology_wide$Sim)
phenology_wide$Sim <- factor(phenology_wide$Sim , ordered = TRUE, 
                                      levels = c(
                                        "1-apr" ,
                                        "5-apr" ,
                                        "10-apr" ,
                                        "15-apr" ,
                                        "20-apr",
                                        "25-may",
                                        "1-may" ,
                                        "5-may" ,
                                        "10-may" ,
                                        "15-may" ,
                                        "20-may",
                                        "25-apr" 
                                      ))



### does every year get a sowing date?
number_year_per_sim <- phenology_wide %>%  
  count(Sim)  
number_year_per_sim

plot1 <- phenology_wide %>% 
  ggplot(aes(x = year, from_sowing_to_emergence))+
  geom_point()+
  facet_wrap(. ~ Sim)+ 
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Days from sowing to emergence",
       #subtitle = "Days from sowing to harvest only. Fixed sowing dates 10-May",
       x = "years",
       y = "Number of days",
       caption = "Note; each sim has 68 years"
       )
plot1

plot2 <- phenology_wide %>% 
  ggplot(aes(x = Date_sowing, Date_emergence))+
  geom_point()+
  facet_wrap(. ~ Sim)+ 
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Days from sowing to emergence",
       #subtitle = "Days from sowing to harvest only. Fixed sowing dates 10-May",
       x = "Sowing date",
       y = "emergence date",
       caption = "Note; each sim has 68 years"
  )
plot2




### narrow

phenology_sowing 
phenology_emergence
phenology_harvest

phenology_long <- rbind(phenology_sowing, phenology_emergence,phenology_harvest )
str(phenology_long)



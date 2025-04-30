## Plant Av water
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)




# Download dry sowing with phenology made in step 3 -------------------------------------------------------

Dry_sowing_Lock_factor_with_met <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/Results/Summary_output_phenology_change.csv")
str(Dry_sowing_Lock_factor_with_met)

unique(Dry_sowing_Lock_factor_with_met$Wheat.Phenology.CurrentStageName)

### pull out the establishment date
names(Dry_sowing_Lock_factor_with_met)
Dry_sowing_Lock_factor_with_met <- Dry_sowing_Lock_factor_with_met %>% 
  select(year,
         Sowing_date,
         Clock.Today,
         Wheat.Phenology.CurrentStageName,
         Wheat.Phenology.Stage,
         Yield_Adj_t_ha,
         PAW_Sum)


## Filter just to plot one year and one sowing date - frost year with typical sowing date

Sim_met_may102023 <- Dry_sowing_Lock_factor_with_met %>% 
  filter(year == 2023) %>% 
  filter(Sowing_date == "10-may")
Sim_met_may102023

unique(Sim_met_may102023$Wheat.Phenology.CurrentStageName)
phenology_stages <- Sim_met_may102023 %>% filter(!is.na(Wheat.Phenology.CurrentStageName)) %>% 
  select(Clock.Today,Wheat.Phenology.CurrentStageName )
phenology_stages

str(Sim_met_may102023)
unique(Sim_met_may102023$frost_Sensitive_period)
Frost_senstive_period <- Sim_met_may102023 %>% 
  select(Clock.Today, frost_Sensitive_period ) %>% 
  filter(frost_Sensitive_period == "frost_sensitive_period" )
Frost_senstive_period

Frost_most_senstive_period <- Sim_met_may102024 %>% 
  select(Clock.Today, frost_most_Sensitive_period ) %>% 
  filter(frost_most_Sensitive_period == "frost_most_sensitive_period" )
Frost_most_senstive_period

phenology_stages

Anthesis_Flowering <- phenology_stages %>% filter(Wheat.Phenology.CurrentStageName== "Anthesis" )

str(Sim_met_may102024$Clock.Today)
str(Anthesis_Flowering)
Anthesis_Flowering

#### I want to label all the V lines
#harvest is duplicated it comes out of APISM like that why?
phenology_stages <- phenology_stages %>% 
  distinct(Wheat.Phenology.CurrentStageName, .keep_all = TRUE)
plot1 <- Sim_met_may102024 %>% 
  ggplot(aes(x = Clock.Today, PAW_Sum))+
  #geom_vline(data = Anthesis_Flowering, aes(xintercept = Clock.Today), color = "darkgreen", size = 2)+
  geom_point()+
  
  theme_classic()+
  theme(legend.position = "none")+
  geom_vline(data = phenology_stages, aes(xintercept = Clock.Today), linetype="dashed", color = "grey")+
  
  labs(title = "Climate station Lock 18046",
       subtitle = "Fixed sowing dates 10-May 2024",
       x = "",
       y = "Sum of PAW",
       caption = "Dashed grey lines indicate phenology stages.")
plot1

plot1_labels <- plot1 + geom_text(data = phenology_stages, 
              mapping = aes(x = Clock.Today, y = c(0.3, 0.5, 0.4, 0.4, 0.4, 0.5, 0.4, 0.4, 0.4,0.4 ,0.4 ), 
                            label = Wheat.Phenology.CurrentStageName, angle = 90),
              inherit.aes = FALSE,
              hjust = 1.0
              )
plot1_labels


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing", "Results")

ggsave(plot = plot1_labels,
       filename = paste0(path_saved_files,"/PAW with phen stages_Lock2024", ".png" ),
       width = 20, height = 12, units = "cm")

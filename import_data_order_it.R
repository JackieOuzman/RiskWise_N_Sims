# This file is for importing APISM sim files.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

### create a list of files
### create a list of headers
### put file name into df

#################################################################################

site <-"/Bute_Rotation"
#""X:\Riskwi$e\Bute\2_Sims_post_Sep2024\1_Classic""
file_directory <- paste0("X:/Riskwi$e/Bute/2_Sims_post_Sep2024/1_Classic",site)

file_directory

################################################################################
list_sim_out_file <-
  list.files(
    path = file_directory,
    pattern = ".out",
    all.files = FALSE,
    full.names = FALSE
  )

list_sim_out_file #
#list_sim_out_file <- list_sim_out_file[c(1:3)]

# create a empty data frame -----------------------------------------------

df_for_all_data <- data.frame(
  dd_mm_yyyy		= character(),
  sow_NO3		= double(),
  sow_NH4		= double(),
  sow_sw		= double(),
  Wheat_biomass		= double(),
  Barley_biomass		= double(),
  Wheat_yield		= double(),
  Barley_yield		= double(),
  harv_NO3		= double(),
  harv_NH4		= double(),
  harv_sw		= double(),
  InCropFert		= double(),
  startSim_sw		= double(),
  startSim_NO3		= double(),
  startSim_NH4		= double(),
  InCropRain		= double(),
  APSIM_Version 		= character(),
  title		= character()



  
)


### set the heading names for the data you want to import
### heading names for file 2,3,5
heading <- c("dd_mm_yyyy",
             "sow_NO3",
             "sow_NH4",
             "sow_sw",
             "Wheat_biomass",
             "Barley_biomass",
             "Wheat_yield",
             "Barley_yield",
             "harv_NO3",
             "harv_NH4",
             "harv_sw",
             "InCropFert",
             "startSim_sw",
             "startSim_NO3",
             "startSim_NH4",
             "InCropRain",
             "APSIM_Version ",
             "title")
             



# As a Loop ---------------------------------------------------------------





## worked example
list_sim_out_file <- "Bute Rotation_YO.out"


for (list_sim_out_file in list_sim_out_file){
  
  
  
  df <- read_table(paste0(file_directory,"/",
                          list_sim_out_file ), #change this back to list when your ready to run as loop (list_sim_out_file OR file_name
                   col_names = FALSE, skip = 4)
  
  
  colnames(df)<- heading # add the column headings (as set above - before the loop)
  
  ### pull out the simulation infomation so I can create some new clms
  
  version = read.csv(paste0(file_directory,"/",
                            list_sim_out_file), skip = 0, header = F, nrows = 1, as.is = T)
  
  
  title_a = read.csv(paste0(file_directory,"/",
                            list_sim_out_file), skip = 1, header = F, nrows = 1, as.is = T)
  
  
  ### formatting the above information
  APSIM_version <- version[1,1]
  APSIM_version<-gsub("ApsimVersion = ","",as.character(APSIM_version))
  
  
  title = title_a[1,1]
  title<-gsub("Title = ","",as.character(title))
  
  
  
  ### get sim settings into the df
  df <- df %>% 
    mutate( APSIM_Version = APSIM_version,
            title = title
              )
  names(df)
  
  
  df_for_all_data <- rbind(df_for_all_data, df)
  
}



# Make a bit neater -------------------------------------------------------
rm(list=ls()[ls()!= "df_for_all_data"])


str(df_for_all_data)
names(df_for_all_data)
df_for_all_data <- df_for_all_data %>% mutate_at(c("sow_NO3",
                                                   "sow_NH4",
                                                   "sow_sw",
                                                   "Wheat_biomass",
                                                   "Barley_biomass",
                                                   "Wheat_yield",
                                                   "Barley_yield",
                                                   "harv_NO3",
                                                   "harv_NH4",
                                                   "harv_sw",
                                                   "InCropFert",
                                                   "startSim_sw",
                                                   "startSim_NO3",
                                                   "startSim_NH4",
                                                   "InCropRain"), as.numeric)


#Add year clm

df_for_all_data <- df_for_all_data %>% 
  separate(dd_mm_yyyy, into = c("dd", "mm", "Year"), remove=FALSE) %>% 
  select(-dd, -mm)%>% 
  mutate(
    Event =case_when(
      dd_mm_yyyy == "03_06_2022"~ "sow",
      dd_mm_yyyy == "21_11_2022" ~ "harvest",
      dd_mm_yyyy == "31_12_2022" ~ "end_year",
      dd_mm_yyyy == "16_05_2023"~ "sow",
      dd_mm_yyyy == "16_10_2023" ~ "harvest",
      dd_mm_yyyy == "31_12_2023" ~ "end_year"))



    

df_for_all_data_long <- df_for_all_data %>% 
  pivot_longer(
    cols = sow_NO3:InCropRain,
    names_to = "variable",
    values_to = "value"
  )
df_for_all_data_long

df_for_all_data_long <- df_for_all_data_long %>% 
  mutate(variable= case_when(
    variable == "Barley_biomass"~ "biomass",
    variable == "Wheat_biomass"~ "biomass",
    variable == "Barley_yield"~ "yield",
    variable == "Wheat_yield"~ "yield",
         .default = variable
  )
  )
 

## filter out rows that I don't need   Its very messy but that will do!     
df_for_all_data_long <- df_for_all_data_long %>% 
  filter(!is.na(value)) %>% 
  filter(value!=0)


df_for_all_data_long <- df_for_all_data_long %>% 
  mutate(treatment = "Control",
         for_join = paste0(Year ,"_", treatment))

df_for_all_data_long

write.csv(df_for_all_data_long , "X:/Riskwi$e/Bute/2_Sims_post_Sep2024/APSIM_Classic_output_long.csv", row.names = FALSE )

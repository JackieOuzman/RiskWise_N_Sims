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
list_sim_out_file



list_sim_out_file <- c("Bute Rotation_Bute_N_Bank_Conservative.out",
                        "Bute Rotation_Bute_N_Bank_OptimumYld.out"  ,
                        "Bute Rotation_Bute_N_Bank_Profit.out"  ,
                        "Bute Rotation_Bute_N_DP.out"   ,          
                        "Bute Rotation_Bute_N0.out"   )
                         

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
#list_sim_out_file <- "Bute Rotation_YO.out"


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

df_for_all_data_1 <- df_for_all_data %>%
  separate(dd_mm_yyyy,
           into = c("dd", "mm", "Year"),
           remove = FALSE) %>%
  select(
    dd_mm_yyyy,
    Year,
    soil_water_start =   startSim_sw,
    soil_water_sowing =  sow_sw,
    soil_water_harvest = harv_sw,
    
    soil_NO3_start =   startSim_NO3,
    soil_N03_sowing =  sow_NO3,
    soil_NO3_harvest = harv_NO3,
    
    soil_NH4_start =   startSim_NH4,
    soil_NH4_sowing =  sow_NH4,
    soil_NH4_harvest = harv_NH4,
    
    Wheat_biomass,
    Barley_biomass,
    
    Wheat_yield,
    Barley_yield,
    
    InCropFert,
    InCropRain,
    title
  )


str(df_for_all_data_1)

# add in 'Soil mineral N' clm NO3+NH4

df_for_all_data_1 <- df_for_all_data_1 %>% 
  mutate(Soil_mineral_N_sowing = as.double(soil_N03_sowing) + as.double(soil_NH4_sowing))



## keep only one row with the harvest dates this is manual
harvest_dates <- c("31_12_2022", "16_10_2023")


### filter based on harvest dates


df_for_all_data_1 <- df_for_all_data_1 %>% 
  filter(dd_mm_yyyy %in%  harvest_dates)
  
str(df_for_all_data_1)
df_for_all_data_1 <- df_for_all_data_1 %>% mutate_at(c(
  "Year",
    "soil_water_start",
    "soil_water_sowing",
    "soil_water_harvest",
    
    "soil_NO3_start",
    "soil_N03_sowing",
    "soil_NO3_harvest",
    
    "soil_NH4_start",
    "soil_NH4_sowing",
    "soil_NH4_harvest",
  
     "Soil_mineral_N_sowing",
    
    "Wheat_biomass",
    "Barley_biomass",
    
    "Wheat_yield",
    "Barley_yield",
    
    "InCropFert"), as.numeric)


#Add some clm
str(df_for_all_data_1)

df_for_all_data_1 <- df_for_all_data_1 %>% mutate(
  Source = "APISM_Classic_Rotation",
  Treatment = title,
  Crop = case_when(
    Year  == 2022 ~ "Wheat",
    Year  == 2023 ~ "Barley"),
  Cultivar = case_when(
    Year  == 2022 ~ "mace",
    Year  == 2023 ~ "commander"),
  Biomass = case_when(
    Year  == 2022 ~ Wheat_biomass,
    Year  == 2023 ~ Barley_biomass),
  Yield = case_when(
    Year  == 2022 ~ Wheat_yield/1000,
    Year  == 2023 ~ Barley_yield/1000)
  )

# drop some clms
str(df_for_all_data_1)
names(df_for_all_data_1)
 
df_for_all_data_1 <- df_for_all_data_1 %>% select(
  "Year",
  "Source",
  "Treatment",
  "InCropFert" ,
  "Crop",
  "Cultivar",
  
  "soil_water_start" ,  "soil_water_sowing",    "soil_water_harvest",
  "soil_NO3_start" ,    "soil_N03_sowing",      "soil_NO3_harvest", 
  "soil_NH4_start",     "soil_NH4_sowing",      "soil_NH4_harvest" ,
  "Soil_mineral_N_sowing",
  "Biomass",            "Yield",  "InCropRain"
  
)

   

write.csv(df_for_all_data_1 , "X:/Riskwi$e/Bute/2_Sims_post_Sep2024/To_compare_etc/APSIM_Classic_Rotations.csv", row.names = FALSE )




# Daily output files -------------------------------------------------------
site <-"/Bute_Rotation"
file_directory <- paste0("X:/Riskwi$e/Bute/2_Sims_post_Sep2024/1_Classic",site)
file_directory

list_sim_out_file <-
  list.files(
    path = file_directory,
    pattern = ".out",
    all.files = FALSE,
    full.names = FALSE
  )
list_sim_out_file

list_sim_out_file <- c("Bute Rotation_Bute_N_Bank_Conservative outputfileDaily.out",
                       "Bute Rotation_Bute_N_Bank_OptimumYld outputfileDaily.out"  ,
                       "Bute Rotation_Bute_N_Bank_Profit outputfileDaily.out"  ,
                       "Bute Rotation_Bute_N_DP outputfileDaily.out"   ,          
                       "Bute Rotation_Bute_N0 outputfileDaily.out"   )


#Test approach
#list_sim_out_file <- c("Bute Rotation_Bute_N_Bank_Conservative outputfileDaily.out")
# create a empty data frame -----------------------------------------------

df_for_all_data <- data.frame(
  dd_mm_yyyy		= character(),
  `sw()`          = double(),
  Rotation_Bute_N_Bank_Conservative.Wheat.biomass 	= double(),
  Rotation_Bute_N_Bank_Conservative.barley.biomass  = double(),
  Rotation_Bute_N_Bank_Conservative.Wheat.biomass = double(),
  
  Rotation_Bute_N_Bank_Conservative.barley.biomass	= double(),
  `no3()`	= double(),
  
  # Rotation_Bute_N_Bank_Conservative.Wheat.sw_stress_photo	= double(),
  # Rotation_Bute_N_Bank_Conservative.barley.sw_stress_photo	= double(),
  # Rotation_Bute_N_Bank_Conservative.Wheat.n_stress_photo	= double(),
  # Rotation_Bute_N_Bank_Conservative.barley.n_stress_photo	= double(),
  
  Rotation_Bute_N_Bank_Conservative.Wheat.zadok_stage	= double(),
  Rotation_Bute_N_Bank_Conservative.barley.zadok_stage	= double(),
  Rotation_Bute_N_Bank_Conservative.Wheat.dlt_dm	= double(),
  Rotation_Bute_N_Bank_Conservative.barley.dlt_dm = double(),
  
  Rain = double(),	
  Rotation_Bute_N_Bank_Conservative.Wheat.swdef_photo	= double(),
  Rotation_Bute_N_Bank_Conservative.barley.swdef_photo	= double(),
  Rotation_Bute_N_Bank_Conservative.Wheat.nfact_photo	= double(),
  Rotation_Bute_N_Bank_Conservative.barley.nfact_photo= double(),
  
  
  APSIM_Version 		= character(),
  title		= character()
)


### set the heading names for the data you want to import #The order is important
### heading names for file 2,3,5
heading <- c("dd_mm_yyyy",
             "SW",
             "Wheat_biomass",
             "Barley_biomass",
             "NO3",
             "Wheat.zadok_stage",
             "barley.zadok_stage",
             "Wheat_dlt_dm",
             "barley_dlt_dm",
             "Wheat_yield",
             "Barley_yield",
             "Rain",
             "Wheat_sw_stress_photo",
             "barley_sw_stress_photo",
             "Wheat_n_stress_photo",
             "barley_n_stress_photo",
             
             "APSIM_Version ",
             "title")




# As a Loop ---------------------------------------------------------------
## worked example
#list_sim_out_file <- "Bute Rotation_YO.out"


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
  APSIM_version
  
  title = title_a[1,1]
  title_a<-gsub("Title = ","",as.character(title))
  title_a
  title_b<-gsub("outputfileDaily","",as.character(title_a)) #daily output
  title_b
  title_c<-gsub("Bute Rotation_Bute_","",as.character(title_b)) #this is the leading sim name - will need to change for different sites
  title_c
  title_d <- trimws(title_c) #trim the white space
  title_d
  
  ### get sim settings into the df
  df <- df %>% 
    mutate( APSIM_Version = APSIM_version,
            title = title_d
    )
  names(df)
  
  
  df_for_all_data <- rbind(df_for_all_data, df)
  
}



# Make a bit neater -------------------------------------------------------
rm(list=ls()[ls()!= "df_for_all_data"])

str(df_for_all_data)
names(df_for_all_data)

df_for_all_data_1 <- df_for_all_data %>%
  separate(dd_mm_yyyy,
           into = c("dd", "mm", "Year"),
           remove = FALSE) %>%
  select(
    dd_mm_yyyy,
    Year,
    soil_water = SW,
    # soil_water_start =   startSim_sw,
    # soil_water_sowing =  sow_sw,
    # soil_water_harvest = harv_sw,
    
    soil_NO3 = NO3,
    # soil_NO3_start =   startSim_NO3,
    # soil_N03_sowing =  sow_NO3,
    # soil_NO3_harvest = harv_NO3,
    
    # soil_NH4_start =   startSim_NH4,
    # soil_NH4_sowing =  sow_NH4,
    # soil_NH4_harvest = harv_NH4,
    
    Wheat_biomass,
    Barley_biomass,
    
    Wheat_yield,
    Barley_yield,
    
    Wheat_sw_stress_photo,
    barley_sw_stress_photo,
    Wheat_n_stress_photo,
    barley_n_stress_photo,
    
    Wheat.zadok_stage,
    barley.zadok_stage,
    Wheat_dlt_dm,
    barley_dlt_dm,
    Rain,
    
    title
  )


str(df_for_all_data_1)



#Add some clm
str(df_for_all_data_1)

df_for_all_data_1 <- df_for_all_data_1 %>% mutate(
  Source = "APISM_Classic_Rotation",
  Treatment = title,
  Crop = case_when(
    Year  == 2022 ~ "Wheat",
    Year  == 2023 ~ "Barley"),
  Cultivar = case_when(
    Year  == 2022 ~ "mace",
    Year  == 2023 ~ "commander"),
  Biomass = case_when(
    Year  == 2022 ~ Wheat_biomass,
    Year  == 2023 ~ Barley_biomass),
  Yield = case_when(
    Year  == 2022 ~ Wheat_yield/1000,
    Year  == 2023 ~ Barley_yield/1000),
  sw_stress_photo= case_when(
    Year  == 2022 ~ Wheat_sw_stress_photo,
    Year  == 2023 ~ barley_sw_stress_photo),
  n_stress_photo= case_when(
    Year  == 2022 ~ Wheat_n_stress_photo,
    Year  == 2023 ~ barley_n_stress_photo),
  zadok_stage = case_when(
    Year  == 2022 ~ Wheat.zadok_stage,
    Year  == 2023 ~ barley.zadok_stage),
  dlt_dm = case_when(
    Year  == 2022 ~ Wheat_dlt_dm,
    Year  == 2023 ~ barley_dlt_dm ),
  HarvestIndex = (Yield*1000)/Biomass
  
)

# drop some clms
str(df_for_all_data_1)
names(df_for_all_data_1)

df_for_all_data_1 <- df_for_all_data_1 %>% select(
  "dd_mm_yyyy" ,
  "Year",
  "Source",
  "Treatment",
  
  "Crop",
  "Cultivar",
  
  "soil_water",
  "soil_NO3"  ,
  
  # "soil_water_start" ,  "soil_water_sowing",    "soil_water_harvest",
  # "soil_NO3_start" ,    "soil_N03_sowing",      "soil_NO3_harvest", 
  # "soil_NH4_start",     "soil_NH4_sowing",      "soil_NH4_harvest" ,
  # "Soil_mineral_N_sowing",
  "Biomass",            "Yield",  
  "sw_stress_photo" , "n_stress_photo",
  "zadok_stage" , "dlt_dm", "HarvestIndex", "Rain"
  
)



write.csv(df_for_all_data_1 , "X:/Riskwi$e/Bute/2_Sims_post_Sep2024/To_compare_etc/APSIM_Classic_RotationsDaily.csv", row.names = FALSE )


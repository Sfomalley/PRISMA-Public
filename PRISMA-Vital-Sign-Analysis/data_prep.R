#*****************************************************************************
#*Vital Status Analysis Report
#*****************************************************************************
#*
#*#*****************************************************************************
rm(list = ls())

library(tidyverse)
library(lubridate)
library(naniar)

#read data
uploadDate = "2023-08-04"

setwd(paste0("~/github/Vital-Signs-Analysis/", uploadDate))

load(paste0("Z:/Processed Data/", uploadDate, "/MatData_Wide_", uploadDate, ".RData"))
load(paste0("Z:/Processed Data/", uploadDate, "/MatData_Anc_Visits_", uploadDate, ".RData"))
# load(paste0("Z:/Merged Data/2023-04-21/MatData_Wide_2023-04-21.RData"))
# load(paste0("Z:/Merged Data/2023-04-21/MatData_Anc_Visits_2023-04-21.RData"))

#*****************************************************************************
#check original data
# mnh06_ke <- read.csv("Z:/SynapseCSVs/Kenya/2023-04-20/mnh06.csv") 
# mnh06_pa <- read.csv("Z:/SynapseCSVs/Pakistan/2023-04-14/mnh06.csv") 
# mnh06_gh <- read.csv("Z:/SynapseCSVs/Ghana/2023-04-21/mnh06.csv") 

#define matData and remove duplicates
matData <- MatData_Wide %>% 
  # matData <- MatData_Anc_Visits %>% 
  distinct() %>% 
  # keep PRiSMA participants only
  filter(M02_CONSENT_IEORRES_1 == 1) %>% 
  left_join(MatData_Anc_Visits %>% select(MOMID, PREGID, EDD), by = c("MOMID", "PREGID")) %>% 
  distinct() %>% 
  mutate(
    bestedd = case_when(
      M01_US_OHOSTDAT_1 < EDD ~ EDD,
      M01_US_OHOSTDAT_1 >= EDD ~ dmy("1907-07-07")
    )
  )

save(matData, file = "derived_data/matData.rda")

#prep data by keeping related variable
prep_vital <- matData %>% 
  select(SCRNID, MOMID, PREGID, SITE, bestedd,
    starts_with("M06_DIAG_VSDAT"),
    starts_with("M06_BP_"),
    starts_with("M06_TEMP_"), 
    starts_with("M06_MHR_"),
    starts_with("M06_RR_"),
    starts_with("M06_PULSEOX_"), 
    starts_with("M06_PALLOR_"),
    starts_with("M06_SPHB_"),
    starts_with("M06_FHR_"), 
    ) %>% 
  replace_with_na_all(condition = ~.== -7) %>% 
  replace_with_na_all(condition = ~.== "1907-07-07")


#change data to long format
data_vital_long <- prep_vital %>% 
  pivot_longer(
    -c("SCRNID","MOMID","PREGID","SITE","bestedd"),#variables to be longer
    names_to = c(".value", "visit_type"), #. in names pattern (.+) to new names, and visit_type as colname of value of suffix(\\d+)
    names_pattern = "^M\\d{2}_(.+)_(\\d+)"#,_MNH06
  ) %>% 
  replace_with_na_at(.vars = c("PALLOR_VSORRES"),
                     condition = ~.x == 77) %>%
  mutate(
    ga_wks = (280 - as.numeric(bestedd - DIAG_VSDAT))/7,
  ) %>% 
  rowwise() %>% 
  mutate(
    bp_sys_mean = mean(c(BP_SYS_VSORRES_1, BP_SYS_VSORRES_2, BP_SYS_VSORRES_3), na.rm = TRUE),
    bp_dia_mean = mean(c(BP_DIA_VSORRES_1, BP_DIA_VSORRES_2, BP_DIA_VSORRES_3), na.rm = TRUE),
  ) %>% 
  ungroup() %>% 
  #remove the cases if ga not available or smaller than 2 weeks
  filter(!is.na(ga_wks) & ga_wks > 2) 

#define long data for blood pressure since we have two types
data_bp_long <- data_vital_long %>% 
  select(SCRNID, MOMID, PREGID, SITE, ga_wks, bp_sys_mean, bp_dia_mean) %>% 
  pivot_longer(
    -c("SCRNID","MOMID","PREGID","SITE","ga_wks"),#variables to be longer
    names_to = c("bp_type"), #. in names pattern (.+) to new names, and visit_type as colname of value of suffix(\\d+)
    names_pattern = "bp_(.*)_mean",#,_MNH06
    values_to = "bp"
  ) %>% 
  mutate(bp_type = case_when(
    bp_type == "sys" ~ "Systolic",
    bp_type == "dia" ~ "Diastolic",
  )) 

#define long data for fhr combing all fetus information
data_fhr_long <- data_vital_long %>% 
  #!!! tempe solution for Ghana is using 77 as default NA
  mutate(
    FHR_VSORRES_1 = ifelse(FHR_VSSTAT_1 != 1, NA, FHR_VSORRES_1),
    FHR_VSORRES_2 = ifelse(FHR_VSSTAT_2 != 1, NA, FHR_VSORRES_2),
    FHR_VSORRES_3 = ifelse(FHR_VSSTAT_3 != 1, NA, FHR_VSORRES_3),
    FHR_VSORRES_4 = ifelse(FHR_VSSTAT_4 != 1, NA, FHR_VSORRES_4)
  ) %>%
  select(SCRNID, MOMID, PREGID, SITE, ga_wks, starts_with("FHR_VSORRES_")) %>% 
  pivot_longer(
    -c("SCRNID","MOMID","PREGID","SITE","ga_wks"),#variables to be longer
    names_to = c("fetus"), #. in names pattern (.+) to new names, and visit_type as colname of value of suffix(\\d+)
    names_pattern = "FHR_VSORRES_(.*)",#,_MNH06
    values_to = "fhr"
  ) %>% distinct()

#save data
save(data_vital_long, file = "derived_data/data_vital_long.rda")
save(data_bp_long, file = "derived_data/data_bp_long.rda")
save(data_fhr_long, file = "derived_data/data_fhr_long.rda")

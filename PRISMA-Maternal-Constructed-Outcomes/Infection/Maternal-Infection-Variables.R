#*****************************************************************************
#* PRISMA Maternal Infection
#* Drafted: 25 October 2023, Stacie Loisate
#* Last updated: 25 October 2023

#The first section, CONSTRUCTED VARIABLES GENERATION, below, the code generates datasets for 
#each form with additional variables that will be used for multiple outcomes. For example, mnh01_constructed 
#is a dataset taht will be used for several outcomes. 

#*****************************************************************************
#*****************************************************************************
#* Data Setup 
#*****************************************************************************
library(tidyverse)
library(readr)
library(dplyr)
library(readxl)
library(gmodels)
library(kableExtra)
library(lubridate)
library(growthstandards) ## INTERGROWTH PACKAGE


# UPDATE EACH RUN # 
# set upload date 
UploadDate = "2023-10-13"

# set path to data
path_to_data = paste0("~/Monitoring Report/data/stacked/",UploadDate)

# set path to save 
path_to_save <- "D:/Users/stacie.loisate/Box/PRISMA-Analysis/Maternal-Constructed-Variables/data/"

# import forms 
mnh01 <- read_csv(paste0(path_to_data,"/", "mnh01_merged.csv")) 
mnh02 <- read_csv(paste0(path_to_data,"/", "mnh02_merged.csv")) 
mnh04 <- read_csv(paste0(path_to_data,"/", "mnh04_merged.csv")) 
mnh06 <- read_csv(paste0(path_to_data,"/", "mnh06_merged.csv")) 
mnh08 <- read_csv(paste0(path_to_data,"/", "mnh08_merged.csv")) 
#*****************************************************************************
#* PULL IDS OF PARTICIPANTS WHO ARE ENROLLED 
# ENROLLED = meet eligibility criteria in MNH02; Section A; Questions 4-8
#*****************************************************************************

enrolled_ids <- mnh02 %>% 
  mutate(ENROLL = ifelse(M02_AGE_IEORRES == 1 & 
                           M02_PC_IEORRES == 1 & 
                           M02_CATCHMENT_IEORRES == 1 & 
                           M02_CATCH_REMAIN_IEORRES == 1 & 
                           M02_CONSENT_IEORRES == 1, 1, 0)) %>% 
  select(SITE, SCRNID, MOMID, PREGID,ENROLL, M02_AGE_IEORRES, M02_PC_IEORRES, M02_CATCHMENT_IEORRES,M02_CATCH_REMAIN_IEORRES, M02_CONSENT_IEORRES) %>% 
  filter(ENROLL == 1) %>% 
  select(SITE, MOMID, PREGID, ENROLL) %>%
  distinct()

enrolled_ids_vec <- as.vector(enrolled_ids$PREGID)

write.csv(enrolled_ids, paste0(path_to_save, "enrolled_ids" ,".csv"), row.names=FALSE)

## if a participant is missing an enrollment form then they will be EXCLULDED from the following analyses
#*****************************************************************************
#### CONSTRUCTED VARIABLES GENERATION: ####
# Add constructed vars to forms that will be used across outcomes (10/05)
# FORM MISSING [varname: Mxx_MISSING]
# FORM COMPLETED [varname: Mxx_FORM_COMPLETE]
#*****************************************************************************

## MNH04 ## 
mnh04_enroll = mnh04 %>% filter(M04_TYPE_VISIT==1) %>% 
  full_join(enrolled_ids, by = c("SITE", "MOMID", "PREGID"))

mnh04_all_visits <- enrolled_ids %>% 
  full_join(mnh04, by = c("SITE", "MOMID", "PREGID")) 


## MNH06 ## 
mnh06_enroll = mnh06 %>% filter(M06_TYPE_VISIT==1) %>% 
  full_join(enrolled_ids, by = c("SITE", "MOMID", "PREGID"))

mnh06_all_visits <- enrolled_ids %>% 
  full_join(mnh06, by = c("SITE", "MOMID", "PREGID")) 

## MNH08 ## 
mnh08_enroll = mnh08 %>% filter(M08_TYPE_VISIT==1) %>% 
  full_join(enrolled_ids, by = c("SITE", "MOMID", "PREGID"))

mnh08_all_visits <- enrolled_ids %>% 
  full_join(mnh08, by = c("SITE", "MOMID", "PREGID")) 

#*****************************************************************************
#* MATERNAL INFECTION 
# Table 1. Data missingness 
# Table 2. STIs 
# a. Diagnosed variables (MNH04)
# b. Measured variables (MNH06)
# Table 3. Other Infections 
# Table 4. All infections combined 
#*****************************************************************************
#*****************************************************************************
#### Table 1. DATA MISSINGNESS FOR INFECTIONS ####
# dataframe name: mat_infection_missingness
#*****************************************************************************
mnh04_constructed_completeness <- mnh04_enroll %>% 
  # MNH04 form missing 
  mutate(M04_MISSING = ifelse(is.na(M04_MAT_VISIT_MNH04), 1, 0)) %>% 
  # MNH08 denominator if form is completed 
  mutate(M04_FORM_COMPLETE = ifelse(!is.na(M04_MAT_VISIT_MNH04), 1, 0)) 

mnh06_constructed_completeness <- mnh06_enroll %>% 
  # MNH06 form missing 
  mutate(M06_MISSING = ifelse(is.na(M06_MAT_VISIT_MNH06), 1, 0)) %>% 
  # MNH06 denominator if form is completed 
  mutate(M06_FORM_COMPLETE = ifelse(!is.na(M06_MAT_VISIT_MNH06), 1, 0)) 

mnh08_constructed_completeness <- mnh08_enroll %>% 
  # MNH08 form missing 
  mutate(M08_MISSING = ifelse(is.na(M08_MAT_VISIT_MNH08), 1, 0)) %>% 
  # MNH08 denominator if form is completed 
  mutate(M08_FORM_COMPLETE = ifelse(!is.na(M08_MAT_VISIT_MNH08), 1, 0)) 


table(mnh04_constructed_completeness$M04_MISSING, mnh04_constructed_completeness$SITE)
table(mnh06_constructed_completeness$M06_MISSING, mnh06_constructed_completeness$SITE)
table(mnh08_constructed_completeness$M08_MISSING, mnh08_constructed_completeness$SITE)

# save data set
write.csv(mnh04_constructed_completeness, paste0(path_to_save, "mnh04_constructed_completeness" ,".csv"), row.names=FALSE)
write.csv(mnh06_constructed_completeness, paste0(path_to_save, "mnh06_constructed_completeness" ,".csv"), row.names=FALSE)
write.csv(mnh08_constructed_completeness, paste0(path_to_save, "mnh08_constructed_completeness" ,".csv"), row.names=FALSE)

# mat_infection_missingness <- enrolled_ids %>% 
#   full_join(mnh04_constructed_enroll, by = c("SITE", "MOMID", "PREGID")) %>%
#   filter(M04_TYPE_VISIT==1 | is.na(M04_TYPE_VISIT)) %>%
#   # join in mnh06 
#   full_join(mnh06_constructed, by = c("SITE", "MOMID", "PREGID")) %>%
#   filter(M06_TYPE_VISIT==1 | is.na(M06_TYPE_VISIT)) %>%
#   # join in MNH08 
#   full_join(mnh08_constructed, by = c("SITE", "MOMID", "PREGID")) %>% 
#   select(SITE, MOMID, PREGID, M04_TYPE_VISIT,M04_MAT_VISIT_MNH04, M04_MISSING, M04_FORM_COMPLETE, 
#          M06_TYPE_VISIT, M06_MISSING,M06_FORM_COMPLETE, M08_TYPE_VISIT, M08_MISSING, M08_FORM_COMPLETE) 
# 
# # save data set
# write.csv(mat_infection_missingness, paste0(path_to_save, "mat_infection_missingness" ,".csv"), row.names=FALSE)

#*****************************************************************************
#### Table 2. STIs ####
# dataframe name: mat_infection_sti
#*****************************************************************************
## Step 1. generate constructed vars for MNH04 diagnosed variables
mat_infection_diagnosed <- mnh04_constructed_completeness %>% 
  # filter(M04_TYPE_VISIT == 1) %>% 
  select(SITE, MOMID, PREGID, M04_TYPE_VISIT,M04_MAT_VISIT_MNH04, M04_HIV_EVER_MHOCCUR, M04_SYPH_MHOCCUR, M04_OTHR_STI_MHOCCUR, M04_GONORRHEA_MHOCCUR,
         M04_CHLAMYDIA_MHOCCUR, M04_GENULCER_MHOCCUR, M04_STI_OTHR_MHOCCUR, M04_FORM_COMPLETE) %>% 
  # Is there av valid result reported?
  ## QUESTION: what do we want to code don't know as?
  mutate(SYPH_DIAG_RESULT = ifelse(M04_SYPH_MHOCCUR %in% c(1,0), 1, 55),
         HIV_DIAG_RESULT = ifelse(M04_HIV_EVER_MHOCCUR %in% c(1,0), 1, 55),
         GON_DIAG_RESULT = ifelse(M04_OTHR_STI_MHOCCUR == 1 & M04_GONORRHEA_MHOCCUR %in% c(1,0) | 
                                    (M04_OTHR_STI_MHOCCUR == 0), 1, 55),
         CHL_DIAG_RESULT = ifelse(M04_OTHR_STI_MHOCCUR == 1 & M04_CHLAMYDIA_MHOCCUR %in% c(1,0) | 
                                    (M04_OTHR_STI_MHOCCUR == 0), 1, 55),
         GENU_DIAG_RESULT = ifelse(M04_OTHR_STI_MHOCCUR == 1 & M04_GENULCER_MHOCCUR %in% c(1,0)| 
                                     (M04_OTHR_STI_MHOCCUR == 0), 1, 55),
         OTHR_DIAG_RESULT = ifelse(M04_STI_OTHR_MHOCCUR %in% c(1,0) | 
                                     (M04_OTHR_STI_MHOCCUR == 0), 1, 55)
  ) %>% 
  
  ## Is the test result missing among those with a completed form?
  mutate(SYPH_DIAG_MISSING = ifelse(SYPH_DIAG_RESULT == 55 & M04_FORM_COMPLETE==1,55,0),
         HIV_DIAG_MISSING = ifelse(HIV_DIAG_RESULT == 55 & M04_FORM_COMPLETE==1,55,0),
         GON_DIAG_MISSING = ifelse(GON_DIAG_RESULT == 55 & M04_OTHR_STI_MHOCCUR==1,55,0), # STI skip pattern - if M04_OTHR_STI_MHOCCUR=1, then this question should be answered
         CHL_DIAG_MISSING = ifelse(CHL_DIAG_RESULT == 55 & M04_OTHR_STI_MHOCCUR==1,55,0), # STI skip pattern - if M04_OTHR_STI_MHOCCUR=1, then this question should be answered
         GENU_DIAG_MISSING = ifelse(GENU_DIAG_RESULT == 55 & M04_OTHR_STI_MHOCCUR==1,55,0), # STI skip pattern - if M04_OTHR_STI_MHOCCUR=1, then this question should be answered
         OTHR_DIAG_MISSING = ifelse(OTHR_DIAG_RESULT == 55 & M04_OTHR_STI_MHOCCUR==1,55,0)) %>% # STI skip pattern - if M04_OTHR_STI_MHOCCUR=1, then this question should be answered
  ## generate variable for any measured STI 
  mutate(ANY_DIAG_STI = ifelse(M04_SYPH_MHOCCUR==1| M04_HIV_EVER_MHOCCUR==1 | M04_GONORRHEA_MHOCCUR==1 |
                                 M04_CHLAMYDIA_MHOCCUR==1 | M04_GENULCER_MHOCCUR==1| M04_STI_OTHR_MHOCCUR==1, 1, 0)) 




## Step 2. generate constructed vars for MNH06 measured variables
mat_infection_measured<- mnh06_constructed_completeness %>%
  # filter(M06_TYPE_VISIT == 1) %>% 
  select(SITE, MOMID, PREGID, 
         M06_TYPE_VISIT, M06_MAT_VISIT_MNH06, M06_HIV_POC_LBORRES, M06_SYPH_POC_LBORRES, M06_FORM_COMPLETE) %>% 
  # Is there av valid result reported?
  mutate(SYPH_MEAS_RESULT = ifelse( M06_SYPH_POC_LBORRES %in% c(1,0), 1, 55),
         HIV_MEAS_RESULT = ifelse(M06_HIV_POC_LBORRES %in% c(1,0), 1, 55)
  ) %>% 
  ## Is the test result missing among those with a completed form?
  mutate(SYPH_MEAS_MISSING = ifelse(SYPH_MEAS_RESULT == 55 & M06_FORM_COMPLETE==1,55,0),
         HIV_MEAS_MISSING = ifelse(HIV_MEAS_RESULT == 55 & M06_FORM_COMPLETE==1,55,0)) %>% 
  ## generate variable for any measured STI 
  mutate(ANY_MEAS_STI = ifelse(M06_HIV_POC_LBORRES == 1 | M06_SYPH_POC_LBORRES == 1 , 1, 0))

## Step 4. for Syphilis only - add a prevalence variables for ANY visit during ANC 
# rename type visit variables 
mnh04_constructed_syph = mnh04_all_visits %>%  rename("TYPE_VISIT" = "M04_TYPE_VISIT") 
mnh06_constructed_syph = mnh06_all_visits %>%  rename("TYPE_VISIT" = "M06_TYPE_VISIT") 

mat_infection_sti_any_visit <- mnh04_constructed_syph %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, M04_SYPH_MHOCCUR) %>% 
  # merge in mnh06 to extract rdt results 
  full_join(mnh06_constructed_syph[c("SITE", "MOMID", "PREGID", "TYPE_VISIT", "M06_SYPH_POC_LBORRES")], 
            by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT")) %>% 
  # generate new var for any positive result at any visit 
  mutate(SYPH_POSITIVE_1 = ifelse(TYPE_VISIT == 1 & (M04_SYPH_MHOCCUR == 1 | M06_SYPH_POC_LBORRES == 1), 1, 0),
         SYPH_POSITIVE_2 = ifelse(TYPE_VISIT == 2 & (M04_SYPH_MHOCCUR == 1 | M06_SYPH_POC_LBORRES == 1), 1, 0),
         SYPH_POSITIVE_3 = ifelse(TYPE_VISIT == 3 & (M04_SYPH_MHOCCUR == 1 | M06_SYPH_POC_LBORRES == 1), 1, 0),
         SYPH_POSITIVE_4 = ifelse(TYPE_VISIT == 4 & (M04_SYPH_MHOCCUR == 1 | M06_SYPH_POC_LBORRES == 1), 1, 0),
         SYPH_POSITIVE_5 = ifelse(TYPE_VISIT == 5 & (M04_SYPH_MHOCCUR == 1 | M06_SYPH_POC_LBORRES == 1), 1, 0)
  ) %>% 
  group_by(SITE, MOMID, PREGID) %>% 
  summarise(SYPH_POSITIVE_ANY_VISIT = ifelse(SYPH_POSITIVE_1==1 | SYPH_POSITIVE_2==1 | SYPH_POSITIVE_3 ==1 | 
                                               SYPH_POSITIVE_5 ==1 | SYPH_POSITIVE_5 ==1,1,0)) %>% 
  filter(SYPH_POSITIVE_ANY_VISIT == 1) %>% 
  distinct()

# save data set
write.csv(mat_infection_sti_any_visit, paste0(path_to_save, "mat_infection_sti_any_visit" ,".csv"), row.names=FALSE)

## Step 4. bind and add "any measurement" variables
mat_infection_sti = full_join(mat_infection_diagnosed, mat_infection_measured, by = c("SITE", "MOMID", "PREGID")) %>% 
  ## Positive test results by either RDT or Diagnosed (only for syphilis and hiv)
  mutate(HIV_POSITIVE_ENROLL = ifelse(M04_HIV_EVER_MHOCCUR==1| M06_HIV_POC_LBORRES == 1, 1, 0),
         SYPH_POSITIVE_ENROLL = ifelse(M04_SYPH_MHOCCUR==1 | M06_SYPH_POC_LBORRES == 1, 1, 0), 
  ) %>% 
  # generate new var for any sti by any measurement 
  mutate(STI_ANY_METHOD = ifelse(ANY_DIAG_STI == 1 | ANY_MEAS_STI == 1, 1, 0), 
         STI_ANY_METHOD_DENOM = ifelse(M04_FORM_COMPLETE == 1 |M06_FORM_COMPLETE == 1, 1, 0)
  )  %>% 
  full_join(mat_infection_sti_any_visit, by = c("SITE", "MOMID", "PREGID"))

# save data set
write.csv(mat_infection_sti, paste0(path_to_save, "mat_infection_sti" ,".csv"), row.names=FALSE)
#*****************************************************************************
#### Table 3. Other Infections ####
# dataframe names: mat_infection_other
# Malaria: MNH04 (MALARIA_EVER_MHOCCUR), MNH06 (HIV_POC_LBPERF)
# Hep B: MNH06 (HBV_POC_LBORRES) 
# Hep C: MNH06 (HCV_POC_LBORRES)
# TB: MNH04 (TB_MHOCCUR, M04_TB_CETERM_1, M04_TB_CETERM_2, M04_TB_CETERM_3,M04_TB_CETERM_4,M04_TB_CETERM_77), MNH08 (TB_CNFRM_LBORRES)
# Covid: MNH04 (M04_COVID_LBORRES), MNH06 (COVID_POC_LBORRES)
#*****************************************************************************

## Step 1. generate constructed vars for MNH04 diagnosed variables
mat_other_infection_mnh04  <- mnh04_constructed_completeness %>% 
  filter(M04_TYPE_VISIT==1) %>% 
  select(SITE, MOMID, PREGID, M04_TYPE_VISIT,M04_FORM_COMPLETE, M04_MALARIA_EVER_MHOCCUR,
         M04_TB_MHOCCUR, M04_COVID_LBORRES, 
         M04_TB_CETERM_1, M04_TB_CETERM_2, M04_TB_CETERM_3,M04_TB_CETERM_4,M04_TB_CETERM_77) %>% 
  # Is there av valid result reported?
  mutate(MAL_DIAG_RESULT = ifelse(M04_MALARIA_EVER_MHOCCUR %in% c(1,0), 1, 55),
         TB_DIAG_RESULT = ifelse(M04_TB_MHOCCUR %in% c(1,0), 1, 55),
         COVID_DIAG_RESULT = ifelse(M04_COVID_LBORRES %in% c(1,0), 1, 55)
  ) %>% 
  ## Is the test result missing among those with a completed form?  
  mutate(MAL_DIAG_MISSING = ifelse(MAL_DIAG_RESULT == 55 & M04_FORM_COMPLETE==1,55,0),
         TB_DIAG_MISSING = ifelse(TB_DIAG_RESULT == 55 & M04_FORM_COMPLETE==1,55,0),
         COVID_DIAG_MISSING = ifelse(COVID_DIAG_RESULT == 55 & M04_FORM_COMPLETE==1,55,0)) %>% 
  ## TB variables
  # a. total with at least 1 symptom in W4SS in MNH04 (1=At least 1 symptom reported, 0=No symptoms)
  mutate(W4SS_SYMPTOMS_ANY = ifelse(M04_TB_CETERM_1==1 | M04_TB_CETERM_2==1 | M04_TB_CETERM_3==1| M04_TB_CETERM_4==1, 1, 0)) %>% 
  # b. total with a response to at least 1 question
  mutate(W4SS_RESPONSE = ifelse(M04_TB_CETERM_1 %in% c(1,0) | M04_TB_CETERM_2 %in% c(1,0) |
                                  M04_TB_CETERM_3 %in% c(1,0) | M04_TB_CETERM_4 %in% c(1,0) |
                                  M04_TB_CETERM_77 %in% c(1,0), 1, 0)) %>% 
  # c. total number missing ALL symptoms -- right now use this &  
  mutate(W4SS_MISSING_SYMP = ifelse(M04_TB_CETERM_1 %in% c(55,77) & M04_TB_CETERM_2 %in% c(55,77) &
                                      M04_TB_CETERM_3 %in% c(55,77) & M04_TB_CETERM_4 %in% c(55,77) &
                                      M04_TB_CETERM_77 %in% c(55,77), 1, 0)) %>% 
  ## generate summary any infection variables
  mutate(OTHER_INFECTION_DIAG_ANY = ifelse(M04_MALARIA_EVER_MHOCCUR==1 | M04_TB_MHOCCUR==1 | M04_COVID_LBORRES==1,1,0))



## Step 2. generate constructed vars for MNH06 measured variables
mat_other_infection_mnh06 <- mnh06_constructed_completeness %>% 
  filter(M06_TYPE_VISIT==1) %>% 
  select(SITE, MOMID, PREGID, 
         M06_TYPE_VISIT, M06_FORM_COMPLETE, M06_MALARIA_POC_LBORRES, M06_HIV_POC_LBPERF,
         M06_HBV_POC_LBORRES, M06_HCV_POC_LBORRES, M06_COVID_POC_LBORRES) %>% 
  # Is there av valid result reported?
  # Malaria, HBV, HCV, COVID
  mutate(MAL_MEAS_RESULT = ifelse(M06_MALARIA_POC_LBORRES %in% c(1,0), 1, 55),
         HBV_MEAS_RESULT = ifelse(M06_HBV_POC_LBORRES %in% c(1,0), 1, 55),
         HCV_MEAS_RESULT = ifelse(M06_HCV_POC_LBORRES %in% c(1,0), 1, 55),
         COVID_MEAS_RESULT = ifelse(M06_COVID_POC_LBORRES %in% c(1,0), 1, 55)
         
  ) %>% 
  ## Is the test result missing among those with a completed form? 
  mutate(MAL_MEAS_MISSING = ifelse(MAL_MEAS_RESULT == 55 & M06_FORM_COMPLETE==1,55,0),
         HBV_MEAS_MISSING = ifelse(HBV_MEAS_RESULT == 55 & M06_FORM_COMPLETE==1,55,0),
         HCV_MEAS_MISSING = ifelse(HCV_MEAS_RESULT == 55 & M06_FORM_COMPLETE==1,55,0),
         COVID_MEAS_MISSING = ifelse(COVID_MEAS_RESULT == 55 & M06_FORM_COMPLETE==1,55,0)
  ) %>% 
  ## generate summary any infection variables
  mutate(OTHER_INFECTION_MEAS_ANY = ifelse(M06_MALARIA_POC_LBORRES==1 | M06_HBV_POC_LBORRES==1 |
                                             M06_HCV_POC_LBORRES==1 | M06_COVID_POC_LBORRES==1,1,0))


## Step 3. generate constructed vars for MNH08 lab variables
mat_other_infection_mnh08 <- mnh08_constructed_completeness %>% 
  filter(M08_TYPE_VISIT==1 ) %>% 
  select(SITE, MOMID, PREGID, 
         M08_TYPE_VISIT, M08_FORM_COMPLETE, M08_TB_CNFRM_LBORRES) %>% 
  ## ADD MNH04 ANYONE WHO REPORTED AT LEAST ONE SYMPTOM 
  full_join(mat_other_infection_mnh04[c("SITE", "MOMID", "PREGID", "W4SS_SYMPTOMS_ANY")], by = c("SITE", "MOMID", "PREGID")) %>% 
  # Is there av valid result reported?
  # Malaria, HBV, HCV, COVID
  mutate(TB_LAB_RESULT = ifelse(M08_TB_CNFRM_LBORRES %in% c(1,2,0), 1, 55)
  ) %>% 
  ## Is the test result missing among those who had at least with one symptom (in MNH04)? 
  mutate(TB_LAB_MISSING = ifelse(TB_LAB_RESULT == 55 & W4SS_SYMPTOMS_ANY==1,55,0)
  )  %>% 
  ## generate summary any infection variables
  mutate(OTHER_INFECTION_LAB_ANY = ifelse(M08_TB_CNFRM_LBORRES==1,1,0)) %>% 
  select(-W4SS_SYMPTOMS_ANY)


## Step 4. bind diagnosed, measured, and lab dataframes together and generate "any infection" variable
mat_infection_other <- full_join(mat_other_infection_mnh04, mat_other_infection_mnh06,by = c("SITE", "MOMID", "PREGID")) %>%
  # merge in mnh08 
  full_join(mat_other_infection_mnh08, by = c("SITE", "MOMID", "PREGID")) %>% 
  ## generate summary any infection variables
  mutate(OTHER_INFECTION_ANY_METHOD = ifelse(OTHER_INFECTION_DIAG_ANY==1 | OTHER_INFECTION_MEAS_ANY==1 |
                                               OTHER_INFECTION_LAB_ANY==1, 1, 0)
  )

# save data set
write.csv(mat_infection_other, paste0(path_to_save, "mat_infection_other" ,".csv"), row.names=FALSE)
#*****************************************************************************
#### Table 4. All infections combined ####
#*****************************************************************************
mat_infections_combined <- full_join(mat_infection_sti, mat_infection_other, by = c("SITE", "MOMID", "PREGID", "M04_TYPE_VISIT",
                                                                                    "M04_FORM_COMPLETE", "M06_TYPE_VISIT",
                                                                                    "M06_FORM_COMPLETE")) %>% 
  # generate variables for any infection diagnosed 
  mutate(ANY_INFECTION_DIAGNOSED = ifelse(M04_SYPH_MHOCCUR==1| M04_HIV_EVER_MHOCCUR==1 | M04_GONORRHEA_MHOCCUR==1 |
                                            M04_CHLAMYDIA_MHOCCUR==1 | M04_GENULCER_MHOCCUR==1| M04_OTHR_STI_MHOCCUR==1 |
                                            M04_MALARIA_EVER_MHOCCUR==1 | M04_TB_MHOCCUR==1 | M04_COVID_LBORRES==1, 1, 0),
         # generate variables for any infection diagnosed 
         ANY_INFECTION_MEASURED = ifelse(M06_HIV_POC_LBORRES == 1 | M06_SYPH_POC_LBORRES == 1 | 
                                           M06_MALARIA_POC_LBORRES==1 | M06_HBV_POC_LBORRES==1 |
                                           M06_HCV_POC_LBORRES==1 | M06_COVID_POC_LBORRES==1 | 
                                           M08_TB_CNFRM_LBORRES == 1, 1, 0), 
         # generate variables for any infection with either method 
         INFECTION_ANY_METHOD = ifelse(ANY_INFECTION_DIAGNOSED == 1 | ANY_INFECTION_MEASURED==1, 1, 0)
  ) %>% 
  # generate denominators for any infection diagnosed by either method
  mutate(INFECTION_ANY_METHOD_DENOM = ifelse(M04_FORM_COMPLETE==1 | M06_FORM_COMPLETE==1, 1, 0))

# save data set
write.csv(mat_infections_combined, paste0(path_to_save, "mat_infections_combined" ,".csv"), row.names=FALSE)


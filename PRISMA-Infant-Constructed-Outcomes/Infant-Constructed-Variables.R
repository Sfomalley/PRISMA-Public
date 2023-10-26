#*****************************************************************************
#* PRISMA Infant Outcomes
#* Drafted: 21 September 2023, Stacie Loisate
#* Last updated: 25 October 2023

# If you copy and paste the following, it will take you to that section:
# 1. Low birth-weight 
# 2. Pre-term birth
# 3. Size for Gestational Age (SGA)
# 4. Neonatal Mortality
# 5. Infant mortality 
# 6. Stillbirth

#The first section, CONSTRUCTED VARIABLES GENERATION, below, the code generates datasets for 
#each form with additional variables that will be used for multiple outcomes. For example, mnh01_constructed 
#is a dataset taht will be used for several outcomes. 

# UPDATE LINES 35, 38, and 41 EACH RUN
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
#path_to_data <- paste("Z:/Stacked Data/",UploadDate, sep = "")
path_to_data = paste0("~/Monitoring Report/data/stacked/" ,UploadDate)

# set path to save 
path_to_save <- "D:/Users/stacie.loisate/Box/PRISMA-Analysis/Infant-Constructed-Variables/data/"

# import forms 
mnh01 <- read_csv(paste0(path_to_data,"/", "mnh01_merged.csv")) 
mnh02 <- read_csv(paste0(path_to_data,"/", "mnh02_merged.csv")) 
mnh04 <- read_csv(paste0(path_to_data,"/", "mnh04_merged.csv")) 
mnh08 <- read_csv(paste0(path_to_data,"/", "mnh08_merged.csv")) 

mnh09 <- read_csv(paste0(path_to_data,"/", "mnh09_merged.csv")) 
mnh11 <- read_csv(paste0(path_to_data,"/", "mnh11_merged.csv")) 
mnh13 <- read_csv(paste0(path_to_data,"/", "mnh13_merged.csv")) 
mnh14 <- read_csv(paste0(path_to_data,"/", "mnh14_merged.csv")) 
mnh15 <- read_csv(paste0(path_to_data,"/", "mnh15_merged.csv")) 
mnh24 <- read_csv(paste0(path_to_data,"/", "mnh24_merged.csv")) 

## For sites that are not reporting MOMID/PREGID in MNH01 for enrollment visits, we will merge these IDs from MNH02 into MNH01
## zambia ids
mnh02_zam_ids <- mnh02 %>% filter(SITE == "Zambia") %>% select(SCRNID, MOMID, PREGID) ## export mnh02 ids
mnh01_zam <- mnh01 %>% filter(SITE == "Zambia", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>%  # pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
  left_join(mnh02_zam_ids, by = c("SCRNID"))
mnh01_all <- mnh01 %>% filter(SITE != "Zambia") # extract site-specific from merged data 
mnh01 <- bind_rows(mnh01_zam, mnh01_all) # rebind data 

## kenya ids
mnh02_ke_ids <- mnh02 %>% filter(SITE == "Kenya") %>% select(SCRNID, MOMID, PREGID) ## export mnh02 ids
mnh01_ke <- mnh01 %>% filter(SITE == "Kenya", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>% # pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
  left_join(mnh02_ke_ids, by = c("SCRNID"))
mnh01_all <- mnh01 %>% filter(SITE != "Kenya") # extract site-specific from merged data 
mnh01 <- bind_rows(mnh01_ke, mnh01_all) # rebind data 

## CMC ids
mnh02_cmc_ids <- mnh02 %>% filter(SITE == "India-CMC") %>% select(SCRNID, MOMID, PREGID) ## export mnh02 ids
mnh01_cmc <- mnh01 %>% filter(SITE == "India-CMC", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>% # pull site-specific data & merge mnh01 and mnh02 by scrnid to get momid/pregid in mnh01
  left_join(mnh02_cmc_ids, by = c("SCRNID"))
mnh01_all <- mnh01 %>% filter(SITE != "India-CMC") # extract site-specific from merged data 
mnh01 <- bind_rows(mnh01_cmc, mnh01_all) # rebind data 

mnh01$M01_US_OHOSTDAT <- as.Date(mnh01$M01_US_OHOSTDAT, format = "%Y-%m-%d")
mnh01 <-mnh01 %>% filter(M01_US_GA_DAYS_AGE_FTS1 != 'hence G.A more than 25weeks"') %>% 
  mutate(M01_US_GA_DAYS_AGE_FTS1 = as.numeric(M01_US_GA_DAYS_AGE_FTS1))

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
  select(SITE, MOMID, PREGID, ENROLL)

enrolled_ids_vec <- as.vector(enrolled_ids$PREGID)
## if a participant is missing an enrollment form then they will be EXCLULDED from the following analyses

#*****************************************************************************
#* CONSTRUCTED VARIABLES GENERATION:
# Add constructed vars to forms that will be used across outcomes (09/01)
#*****************************************************************************
### MNH01 ###
## add constructed vars for: 
# BOE_EDD, [varname: EDD_BOE]
# BOE_GA, [varnames: BOE_GA_WKS, BOE_GA_DAYS]
# Estimate conception date [varname: EST_CONCEP_DATE]

mnh01_constructed <- mnh01 %>% 
  # extract participants who are enrolled
  filter(PREGID %in% enrolled_ids_vec) %>% 
  ## only want the first ultrasound visit -- take the earliest date for each participant -- USE TYPE_VISIT = 1 FOR NOW
  # group_by(SITE, MOMID, PREGID) %>%
  # arrange(M01_US_OHOSTDAT) %>%
  # slice(1) %>%
  filter(M01_TYPE_VISIT == 1) %>% 
  # select a subset of variables
  select(SITE, MOMID,PREGID,M01_TYPE_VISIT,M01_US_OHOSTDAT,contains("M09_BIRTH_DSTERM_INF"), contains("M01_US_GA_WKS_AGE_FTS"),
         contains("M01_US_GA_DAYS_AGE_FTS"),contains("M01_CAL_GA_WKS_AGE_FTS"),contains("M01_CAL_GA_DAYS_AGE_FTS"), M01_GA_LMP_WEEKS_SCORRES, M01_GA_LMP_DAYS_SCORRES) %>% 
  # filter out any ultrasound visit dates that are 07-07-1907
  filter(M01_US_OHOSTDAT != ymd("1907-07-07")) %>%   
  # calculate us ga in days with reported ga in wks + days. if ga is -7 or -5, replace with NA
  ## combine ga weeks and days variables to get a single gestational age variable
  mutate(GA_US_DAYS_FTS1 =  ifelse(SITE!="India-CMC" & M01_US_GA_WKS_AGE_FTS1!= -7 & M01_US_GA_DAYS_AGE_FTS1 != -7,  (M01_US_GA_WKS_AGE_FTS1 * 7 + M01_US_GA_DAYS_AGE_FTS1), NA), 
         GA_US_DAYS_FTS2 =  ifelse(SITE!="India-CMC" & M01_US_GA_WKS_AGE_FTS2!= -7 & M01_US_GA_DAYS_AGE_FTS2 != -7,  (M01_US_GA_WKS_AGE_FTS2 * 7 + M01_US_GA_DAYS_AGE_FTS2), NA),
         GA_US_DAYS_FTS3 =  ifelse(SITE!="India-CMC" & M01_US_GA_WKS_AGE_FTS3!= -7 & M01_US_GA_DAYS_AGE_FTS3 != -7,  (M01_US_GA_WKS_AGE_FTS3 * 7 + M01_US_GA_DAYS_AGE_FTS3), NA),
         GA_US_DAYS_FTS4 =  ifelse(SITE!="India-CMC" & M01_US_GA_WKS_AGE_FTS4!= -7 & M01_US_GA_DAYS_AGE_FTS4 != -7,  (M01_US_GA_WKS_AGE_FTS4 * 7 + M01_US_GA_DAYS_AGE_FTS4), NA)) %>% 
  ## combine ga weeks and days variables to get a single gestational age variable - CMC is using acog - use this here 
  mutate(GA_US_DAYS_FTS1 =  ifelse(SITE=="India-CMC" & M01_CAL_GA_WKS_AGE_FTS1!= -7 & M01_CAL_GA_DAYS_AGE_FTS1 != -7,  (M01_CAL_GA_WKS_AGE_FTS1 * 7 + M01_CAL_GA_DAYS_AGE_FTS1), GA_US_DAYS_FTS1), 
         GA_US_DAYS_FTS2 =  ifelse(SITE=="India-CMC" & M01_CAL_GA_WKS_AGE_FTS2!= -7 & M01_CAL_GA_DAYS_AGE_FTS2 != -7,  (M01_CAL_GA_WKS_AGE_FTS2 * 7 + M01_CAL_GA_DAYS_AGE_FTS2), GA_US_DAYS_FTS2),
         GA_US_DAYS_FTS3 =  ifelse(SITE=="India-CMC" & M01_CAL_GA_WKS_AGE_FTS3!= -7 & M01_CAL_GA_DAYS_AGE_FTS3 != -7,  (M01_CAL_GA_WKS_AGE_FTS3 * 7 + M01_CAL_GA_DAYS_AGE_FTS3), GA_US_DAYS_FTS3),
         GA_US_DAYS_FTS4 =  ifelse(SITE=="India-CMC" & M01_CAL_GA_WKS_AGE_FTS4!= -7 & M01_CAL_GA_DAYS_AGE_FTS4 != -7,  (M01_CAL_GA_WKS_AGE_FTS4 * 7 + M01_CAL_GA_DAYS_AGE_FTS4), GA_US_DAYS_FTS4)) %>% 
  #  pull the largest GA for multiple fetuses + convert to weeks
  mutate(US_GA_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% ## where GA_US_DAYS_FTSx is the reported GA by ultrasound (added together M01_US_GA_WKS_AGE_FTSx and M01_US_GA_DAYS_AGE_FTSx to get a single estimate in days)
  mutate(US_GA_WKS = US_GA_DAYS %/% 7) %>% 
  #  convert ga by LMP to days and wks
  mutate(LMP_GA_DAYS =  ifelse(M01_GA_LMP_WEEKS_SCORRES != -7 & M01_GA_LMP_DAYS_SCORRES != -7,  (M01_GA_LMP_WEEKS_SCORRES * 7 + M01_GA_LMP_DAYS_SCORRES), NA)) %>% 
  mutate(LMP_GA_WKS = LMP_GA_DAYS %/% 7) %>%
  ## generate indicator variable for missing US 
  mutate(MISSING_BOTH_US_LMP = ifelse((US_GA_WKS < 0 & LMP_GA_WKS < 0) | 
                                        (is.na(US_GA_WKS) & is.na(LMP_GA_WKS)), 1, 0)) %>% 
  #  calculate the difference in days between reported LMP and reported US
  mutate(GA_DIFF_DAYS = LMP_GA_DAYS-US_GA_DAYS) %>%
  #  obtain best obstetric estimate in weeks
  mutate(BOE_GA_DAYS = case_when(LMP_GA_DAYS %/% 7 < 9 ~
                                   if_else(abs(GA_DIFF_DAYS) <= 5,
                                           LMP_GA_DAYS,
                                           US_GA_DAYS),
                                 LMP_GA_DAYS %/% 7 < 16 ~
                                   if_else(abs(GA_DIFF_DAYS) <=7,
                                           LMP_GA_DAYS, US_GA_DAYS),
                                 LMP_GA_DAYS %/% 7 >= 16 ~
                                   if_else(abs(GA_DIFF_DAYS) <=10,
                                           LMP_GA_DAYS, US_GA_DAYS),
                                 TRUE ~ US_GA_DAYS)) %>%
  mutate(BOE_GA_WKS = BOE_GA_DAYS %/% 7) %>% 
  # generate indicator variable if LMP or US was used (where 1 = US and 2 = LMP)
  mutate(BOE_METHOD = ifelse(BOE_GA_WKS == US_GA_WKS, 1, 
                             ifelse(BOE_GA_WKS == LMP_GA_WKS, 2, 55))) %>% 
  # generate EDD based on BOE 
  # "zero out" GA and obtain the estimated "date of conception" 
  mutate(EST_CONCEP_DATE = M01_US_OHOSTDAT - BOE_GA_DAYS) %>% 
  # add 280 days to EST_CONCEP_DATE to generate EDD based on BOE 
  mutate(EDD_BOE = EST_CONCEP_DATE + 280) 

# save data set
write.csv(mnh01_constructed, paste0(path_to_save, "mnh01_constructed" ,".csv"), row.names=FALSE)

### MNH04 ###
# this form will be used to pull any fetal losses reported in MNH04 
## add constructed vars for: 
# Gestational age at fetal loss [varname: GESTAGE_FETAL_LOSS_WKS]

mnh04_constructed <- mnh04 %>% 
  select(SITE, MOMID, PREGID,M04_MAT_VISIT_MNH04, M04_PRG_DSDECOD, M04_FETAL_LOSS_DSSTDAT, M04_FETAL_LOSS_DSDECOD) %>% ## select only fetal loss variables
  group_by(SITE, MOMID, PREGID) %>% 
  distinct() %>% 
  ## calculate the gestational age at fetal loss 
  # first join in EST_CONCEP_DATE from mnh01_constructed 
  left_join(mnh01_constructed[c("SITE", "MOMID", "PREGID", "EST_CONCEP_DATE")], by = c("SITE", "MOMID", "PREGID")) %>% 
  # replace default value date with NA 
  mutate(M04_FETAL_LOSS_DSSTDAT = replace(M04_FETAL_LOSS_DSSTDAT, M04_FETAL_LOSS_DSSTDAT==ymd("1907-07-07"), NA)) %>% 
  # calculate gestational age at fetal loss
  mutate(GESTAGE_FETAL_LOSS_WKS = floor(as.numeric(M04_FETAL_LOSS_DSSTDAT-EST_CONCEP_DATE)/7))

### MNH09 ###
## add constructed vars to mnh09 for:
# GA at Birth [varname: GESTAGEBIRTH_BOE_DAYS, GESTAGEBIRTH_BOE]
# DOB (earliest DOB in the event of multiple fetuses) [varname: DOB] 

mnh09_constructed <- mnh09 %>%
  # only want participatns who are enrolled
  filter(PREGID %in% enrolled_ids_vec) %>% 
  # select(SITE, MOMID, PREGID, contains("M09_DELIV_DSSTDAT_INF"), contains("M09_BIRTH_DSTERM_INF")) %>% 
  ## 1. Calculating GA at birth ## 
  # merge in MNH01 info
  left_join(mnh01_constructed, by = c("SITE", "MOMID", "PREGID")) %>% 
  # pull earliest date of birth 
  # first replace default value date with NA 
  mutate(M09_DELIV_DSSTDAT_INF1 = replace(M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTDAT_INF1==ymd("1907-07-07"), NA),
         M09_DELIV_DSSTDAT_INF2 = replace(M09_DELIV_DSSTDAT_INF2, M09_DELIV_DSSTDAT_INF2%in% c(ymd("1907-07-07"), ymd("1905-05-05")), NA),
         M09_DELIV_DSSTDAT_INF3 = replace(M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTDAT_INF3==ymd("1907-07-07"), NA),
         M09_DELIV_DSSTDAT_INF4 = replace(M09_DELIV_DSSTDAT_INF4, M09_DELIV_DSSTDAT_INF4==ymd("1907-07-07"), NA)) %>% 
  mutate(DOB = 
           pmin(M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTDAT_INF2, 
                M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTDAT_INF4, na.rm = TRUE)) %>% 
  # generate indicator variable for having a birth outcome
  mutate(BIRTH_OUTCOME = ifelse(M09_BIRTH_DSTERM_INF1 == 1 | M09_BIRTH_DSTERM_INF1 == 2 | 
                                  M09_BIRTH_DSTERM_INF2 == 1 | M09_BIRTH_DSTERM_INF2 == 2 | 
                                  M09_BIRTH_DSTERM_INF3 == 1 | M09_BIRTH_DSTERM_INF3 == 3 |
                                  M09_BIRTH_DSTERM_INF4 == 1 | M09_BIRTH_DSTERM_INF4 == 2, 1, 0)) %>% 
  # only want those who have had a birth outcome 
  filter(BIRTH_OUTCOME == 1) %>% 
  # calculate the number of days between DOB and estimated conception date
  mutate(GESTAGEBIRTH_BOE_DAYS = as.numeric(DOB - EST_CONCEP_DATE), 
         GESTAGEBIRTH_BOE = floor(GESTAGEBIRTH_BOE_DAYS/7)) %>% 
  select(-names(mnh01_constructed[,-c(1:3)]))

### MNH09 - long ###
# make data long for infant required outcomes -- pull out each infant's data and merge back together in long format
m09_INF1 <- mnh09_constructed %>% 
  select(-contains("_INF2"), -contains("_INF3"), -contains("_INF4")) %>% 
  rename_with(~str_remove(., '_INF1')) %>% 
  rename("INFANTID" = "M09_INFANTID") %>% 
  filter(INFANTID != "n/a") %>% 
  mutate(M09_DELIV_DSSTDAT = replace(M09_DELIV_DSSTDAT, M09_DELIV_DSSTDAT==ymd("1907-07-07"), NA), # replace default value date with NA 
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="77:77", NA), # replace default value time with NA 
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="07:07", NA), # replace default value time with NA 
         DELIVERY_DATETIME = paste(M09_DELIV_DSSTDAT, M09_DELIV_DSSTTIM), # concatenate date and time of birth 
         DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")  # assign time field type for time of birth
  )  



m09_INF2 <- mnh09_constructed %>% 
  select(-contains("_INF1"), -contains("_INF3"), -contains("_INF4")) %>% 
  rename_with(~str_remove(., '_INF2')) %>% 
  rename("INFANTID" = "M09_INFANTID") %>% 
  filter(INFANTID != "n/a") %>% 
  mutate(M09_DELIV_DSSTDAT = replace(M09_DELIV_DSSTDAT, M09_DELIV_DSSTDAT==ymd("1907-07-07"), NA), # replace default value date with NA 
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="77:77", NA), # replace default value time with NA 
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="07:07", NA), # replace default value time with NA 
         DELIVERY_DATETIME = paste(M09_DELIV_DSSTDAT, M09_DELIV_DSSTTIM), # concatenate date and time of birth 
         DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")  # assign time field type for time of birth
  )  

m09_INF3 <- mnh09_constructed %>% 
  select(-contains("_INF1"), -contains("_INF2"), -contains("_INF4")) %>% 
  rename_with(~str_remove(., '_INF3')) %>% 
  rename("INFANTID" = "M09_INFANTID") %>% 
  filter(INFANTID != "n/a") %>% 
  mutate(M09_DELIV_DSSTDAT = replace(M09_DELIV_DSSTDAT, M09_DELIV_DSSTDAT==ymd("1907-07-07"), NA), # replace default value date with NA 
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="77:77", NA), # replace default value time with NA 
         M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="07:07", NA), # replace default value time with NA 
         DELIVERY_DATETIME = paste(M09_DELIV_DSSTDAT, M09_DELIV_DSSTTIM), # concatenate date and time of birth 
         DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")  # assign time field type for time of birth
  )  

# m09_INF4 <- mnh09_constructed %>% 
#   select(-contains("_INF1"), -contains("_INF2"), -contains("_INF3")) %>% 
#   rename_with(~str_remove(., '_INF4')) %>% 
#   rename("INFANTID" = "M09_INFANTID") %>% 
#   filter(INFANTID != "n/a") %>% 
#   mutate(M09_DELIV_DSSTDAT = replace(M09_DELIV_DSSTDAT, M09_DELIV_DSSTDAT==ymd("1907-07-07"), NA), # replace default value date with NA 
#          M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="77:77", NA), # replace default value time with NA 
#          M09_DELIV_DSSTTIM = replace(M09_DELIV_DSSTTIM, M09_DELIV_DSSTTIM=="07:07", NA), # replace default value time with NA 
#          DELIVERY_DATETIME = paste(M09_DELIV_DSSTDAT, M09_DELIV_DSSTTIM), # concatenate date and time of birth 
#          DELIVERY_DATETIME = as.POSIXct(DELIVERY_DATETIME, format= "%Y-%m-%d %H:%M")  # assign time field type for time of birth
#   )  

## bind all infants together 
#mnh09_long <- bind_rows(m09_INF1, m09_INF2, m09_INF3, m09_INF4) 
m09_INF1$M09_DELIV_DSSTTIM = as.character(m09_INF1$M09_DELIV_DSSTTIM) ## data housekeeping here for time issues in data
m09_INF2$M09_DELIV_DSSTTIM = as.character(m09_INF2$M09_DELIV_DSSTTIM) ## data housekeeping here for time issues in data
m09_INF3$M09_DELIV_DSSTTIM = as.character(m09_INF3$M09_DELIV_DSSTTIM) ## data housekeeping here for time issues in data

mnh09_long <- bind_rows(m09_INF1, m09_INF2, m09_INF3)

### MNH11 ###
## add constructed vars to mnh11 for:
# birthweight: PRISMA [varname: BWEIGHT_PRISMA] and PRISMA + Facility [varname: BWEIGHT_ANY]
mnh11_constructed <- mnh11 %>% 
  # only want participatns who are enrolled
  filter(PREGID %in% enrolled_ids_vec) %>% 
  mutate(BWEIGHT_PRISMA = ifelse(M11_BW_EST_FAORRES >=0 & M11_BW_EST_FAORRES < 72 & M11_BW_FAORRES > 0 & !is.na(M11_BW_FAORRES), M11_BW_FAORRES, 
                                 ifelse(is.na(M11_BW_FAORRES), -5, -5)),
         BWEIGHT_ANY = ifelse((BWEIGHT_PRISMA <= 0  & M11_BW_FAORRES_REPORT > 0) | ## if PRISMA is missing and facility is not 
                                (BWEIGHT_PRISMA < 0 & M11_BW_EST_FAORRES > 168), ## OR if prisma is not missing but time is >7days, select facility
                              M11_BW_FAORRES_REPORT, ## if facility is NOT missing
                              ifelse(BWEIGHT_PRISMA < 0 &  M11_BW_FAORRES_REPORT < 0 , -5, M11_BW_FAORRES)))  # if prisma is available but the time is invalid, use facility

### PULL LATEST VISIT ### 
## MNH11 + MNH13/14/15 -- pull the latest visit date for each infant - we will use this to calculate the "age infant was last seen"
mnh11_latest <- mnh11 %>% filter(M11_INF_VITAL_MNH11 ==1) %>%  select(SITE, INFANTID, M11_VISIT_OBSSTDAT)  %>% rename("VISITDATE" = M11_VISIT_OBSSTDAT) 
mnh13_latest <- mnh13%>% filter(M13_INF_VITAL_MNH13 ==1) %>%select(SITE, INFANTID, M13_VISIT_OBSSTDAT)  %>% rename("VISITDATE" = M13_VISIT_OBSSTDAT) %>% 
  filter(VISITDATE != 0) %>% mutate(VISITDATE = ymd(VISITDATE))
mnh14_latest <- mnh14%>% filter(M14_INF_VITAL_MNH14 ==1) %>%select(SITE, INFANTID, M14_VISIT_OBSSTDAT)  %>% rename("VISITDATE" = M14_VISIT_OBSSTDAT)
mnh15_latest <- mnh15%>% filter(M15_INF_VITAL_MNH15 ==1) %>%select(SITE, INFANTID, M15_OBSSTDAT)  %>% rename("VISITDATE" = M15_OBSSTDAT)

latest_visit <- bind_rows(mnh11_latest, mnh13_latest, mnh14_latest, mnh15_latest) %>% group_by(SITE, INFANTID) %>% summarise(LATESTDATE = max(VISITDATE))

### MNH24 ###
## add constructed vars to mnh24 for:
# Indicator if an infant dies [varname: DTH_INDICATOR]
# Age at death in days [varname: AGEDEATH]
# Age at death in hours [varname: AGEDEATH_HRS]

mnh24_constructed <- mnh24 %>% 
  # generate indicator if an infant died
  mutate(DTH_INDICATOR = ifelse(M24_CLOSE_DSDECOD == 3, 1, 0)) %>% 
  # merge in MNH13 to get visit dates 
  left_join(latest_visit, by = c("SITE", "INFANTID")) %>% 
  # merge in DOB information 
  left_join(mnh09_long, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  # concatenate death date time 
  mutate(M24_DTHDAT = replace(M24_DTHDAT, M24_DTHDAT==ymd("1907-07-07"), NA), # replace default value date with NA 
         M24_DTHTIM = replace(M24_DTHTIM, is.na(M24_DTHTIM), "12:30:00"), # replace default value time with NA 
         # M24_DTHTIM = replace(M24_DTHTIM, M24_DTHTIM=="07:07", NA), # replace default value time with NA 
         DEATH_DATETIME = paste(M24_DTHDAT, M24_DTHTIM), # concatenate date and time of birth 
         DEATH_DATETIME = as.POSIXct(DEATH_DATETIME, format= "%Y-%m-%d %H:%M")) %>% # assign time field type for time of birth
  mutate(DTHDAT = M24_DTHDAT) %>% 
  # calculate age at death
  mutate(AGEDEATH = ifelse(DTH_INDICATOR == 0, NA, 
                           ifelse(DTH_INDICATOR == 1, as.numeric(DTHDAT - DOB), NA))) %>% 
  mutate(AGEDEATH_HRS = floor(as.numeric(difftime(DEATH_DATETIME, DELIVERY_DATETIME, units = c("hours")))),1) %>% 
  select(names(mnh24),DOB,DEATH_DATETIME,DELIVERY_DATETIME, DTH_INDICATOR, DTHDAT,AGEDEATH, AGEDEATH_HRS) %>% 
  ## generate indicator variables for where things could go wrong
  mutate(MISSING_MNH09 = ifelse(is.na(DOB), 1, 0), # MISSING MNH09
         DOB_BEFORE_BIRTH = ifelse(is.na(DOB), 55,
                                   ifelse(DOB > DTHDAT, 1, 0)), # DEATH DATE BEOFRE DOB
         CLOSEOUTID_MISSING_MNH02 = ifelse(PREGID %in% enrolled_ids_vec, 0, 1))  # NOT ENROLLED

## TIME VARYING DATASET 
# generate constructed variables that will be used for time-varyign outcomes 
# Indicator if an infant dies [varname: DTH_INDICATOR]
# Date the infant was last seen [varname: DATE_LAST_SEEN]
# Age the infant was last seen [varname: AGE_LAST_SEEN]

# Inlude variables for death indicator, agedeath, age last seen 
timevarying_constructed <- mnh09_long %>% 
  # merge in mnh11 
  full_join(mnh11_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  select(SITE, MOMID, PREGID, INFANTID,M09_MAT_VISIT_MNH09, DOB, DELIVERY_DATETIME,M11_INF_VISIT_MNH11, M11_INF_DSTERM) %>% 
  # merge in closeout form
  full_join(mnh24_constructed[c("SITE","MOMID", "PREGID",  "INFANTID", "DTH_INDICATOR","M24_CLOSE_DSDECOD", "DTHDAT", "DEATH_DATETIME",
                                "AGEDEATH", "AGEDEATH_HRS","CLOSEOUTID_MISSING_MNH02")], by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% # , "MISSING_MNH09", "DOB_BEFORE_BIRTH", "CLOSEOUTID_MISSING_MNH02"
  # generate closeout indicator 
  mutate(CLOSEOUT = ifelse(!is.na(M24_CLOSE_DSDECOD), 1, 0)) %>% 
  mutate(DTH_INDICATOR = ifelse(is.na(M24_CLOSE_DSDECOD), 0,
                                ifelse(M24_CLOSE_DSDECOD == 3, 1, 0))) %>% #  & CLOSEOUTID_MISSING_MNH02 == 0
  # merge in latest visit data
  left_join(latest_visit, by = c("SITE", "INFANTID")) %>% 
  # if infant has died, the date of last visit is the death date
  mutate(DATE_LAST_SEEN = case_when(DTH_INDICATOR == 1 ~ DTHDAT, 
                                    TRUE ~ LATESTDATE)) %>% 
  select(-LATESTDATE) %>% 
  # calculate age at last seen for live births (age at death for dead)
  mutate(AGE_LAST_SEEN = case_when(DTH_INDICATOR == 0 ~ as.numeric(DATE_LAST_SEEN - DOB), 
                                   DTH_INDICATOR == 1 ~ AGEDEATH,
                                   TRUE ~ NA)) 

#*****************************************************************************
#* 1. Low birth-weight 
# a. PRISMA staff weight (missing if no weight taken): [varname: LBW2500_PRISMA, LBW1500_PRISMA]
# b. PRISMA (+facility weight if PRISMA is missing): [varname:LBW2500_ANY, LBW1500_ANY]
# c. HOLD: PRISMA staff weight adjusted for time at weighing (+facility weight if PRISMA is missing) 

# Forms and variables needed: 
# M11_INF_DSTERM [MNH11]
# M11_BW_FAORRES [MNH11]
# M11_BW_FAORRES_REPORT [MNH11]
# M11_BW_EST_FAORRES [MNH11]
# BWEIGHT_PRISMA [mnh11_constructed]
# BWEIGHT_ANY [mnh11_constructed]
#*****************************************************************************

lowbirthweight <- mnh11_constructed %>%
  ## pull key variables 
  select(SITE, MOMID, INFANTID, M11_INF_DSTERM, BWEIGHT_PRISMA, BWEIGHT_ANY, M11_BW_FAORRES, M11_BW_FAORRES_REPORT, M11_BW_EST_FAORRES) %>% 
  ## filter livebirths 
  filter(M11_INF_DSTERM == 1) %>% 
  ## LBW PRISMA measured (bw <2500g)
  mutate(LBW2500_PRISMA = ifelse(BWEIGHT_PRISMA >= 0 & BWEIGHT_PRISMA < 2500, 1,
                                 ifelse(BWEIGHT_PRISMA <= 0 , 55, 0))) %>% 
  ## LBW PRISMA measured (bw <1500g)
  mutate(LBW1500_PRISMA = ifelse(BWEIGHT_PRISMA >= 0 & BWEIGHT_PRISMA < 1500, 1,
                                 ifelse(BWEIGHT_PRISMA <= 0 , 55, 0))) %>% 
  ## LBW PRISMA measured (bw <2500g); varname: LBW2500_ANY
  mutate(LBW2500_ANY = ifelse(BWEIGHT_ANY >= 0 & BWEIGHT_ANY < 2500, 1,
                              ifelse(BWEIGHT_ANY <= 0 | is.na(BWEIGHT_ANY), 55, 0))) %>% 
  ## LBW PRISMA measured (bw <1500g); varname: LBW1500_ANY 
  mutate(LBW1500_ANY = ifelse(BWEIGHT_ANY >= 0 & BWEIGHT_ANY < 1500, 1,
                              ifelse(BWEIGHT_ANY <= 0 | is.na(BWEIGHT_ANY), 55, 0))) %>% 
  # ## PRISMA LBW categorical variable: (PRISMA bw <2500g)=11, (PRISMA bw <1500g)=12, (PRISMA bw >= 2500g)
  # mutate(LBW_CAT_PRISMA = ifelse(LBW1500_PRISMA == 1, 12, 
  #                         ifelse(LBW2500_PRISMA == 1, 11, 
  #                         ifelse(BWEIGHT_PRISMA >= 2500, 13, 
  #                         ifelse(BWEIGHT_PRISMA < 0 | M11_BW_EST_FAORRES > 150, 55, NA))))) %>% 
  # ## ANY LBW categorical variable: (any bw <2500g)=11, (any bw <1500g)=12, (any bw >= 2500g)
  # mutate(LBW_CAT_ANY = ifelse(LBW1500_ANY == 1, 12,
  #                                ifelse(LBW2500_ANY== 1, 11,
  #                                       ifelse(BWEIGHT_ANY >= 2500, 13, 
  #                                              ifelse(BWEIGHT_ANY < 0, 55, NA))))) %>% 
  ## PRISMA LBW categorical variable: (PRISMA bw <2500g)=11, (PRISMA bw <1500g)=12, (PRISMA bw >= 2500g)
mutate(LBW_CAT_PRISMA = ifelse(BWEIGHT_PRISMA >= 0 & BWEIGHT_PRISMA < 1500, 12, 
                               ifelse(BWEIGHT_PRISMA >= 1500 & BWEIGHT_PRISMA < 2500, 11, 
                                      ifelse(BWEIGHT_PRISMA >= 2500, 13, 
                                             ifelse(BWEIGHT_PRISMA < 0 | M11_BW_EST_FAORRES > 150, 55, NA))))) %>% 
  ## ANY LBW categorical variable: (any bw <2500g)=11, (any bw <1500g)=12, (any bw >= 2500g)
  mutate(LBW_CAT_ANY = ifelse(BWEIGHT_ANY >= 0 & BWEIGHT_ANY < 1500, 12,
                              ifelse( BWEIGHT_ANY >= 1500 & BWEIGHT_ANY < 2500, 11,
                                      ifelse(BWEIGHT_ANY >= 2500, 13, 
                                             ifelse(BWEIGHT_ANY < 0, 55, NA))))) %>% 
  ## generate indicator for missing weights 
  mutate(MISSING_PRISMA = ifelse(M11_BW_FAORRES < 0 & M11_BW_FAORRES_REPORT > 0, 1, 0), #  
         MISSING_FACILITY = ifelse(M11_BW_FAORRES_REPORT < 0 & M11_BW_FAORRES > 0, 1, 0), #  
         MISSING_BOTH = ifelse(M11_BW_FAORRES < 0  & M11_BW_FAORRES_REPORT < 0, 1, 0),
         MISSING_TIME = ifelse(M11_BW_EST_FAORRES < 0 | M11_BW_EST_FAORRES >= 168 | is.na(M11_BW_EST_FAORRES), 1, 0)) %>% # 
  mutate(BW_TIME = ifelse(M11_BW_EST_FAORRES < 0 | M11_BW_EST_FAORRES >= 97, NA, M11_BW_EST_FAORRES))

write.csv(lowbirthweight, paste0(path_to_save, "lowbirthweight" ,".csv"), row.names=FALSE)
#*****************************************************************************
#* 2. Pre-term delivery 
# a. Preterm delivery (<37 weeks): Delivery prior to 37 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT37]
# b. Preterm delivery (<34 weeks): Delivery prior to 34 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT34]
# c. Preterm delivery (<32 weeks): Delivery prior to 32 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT32]
# d. Preterm delivery (<28 weeks): Delivery prior to 28 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT28]
# e. Preterm delivery severity (categorical): Late preterm (34 to <37 wks), early preterm (32 to <34 wks), very preterm (28 to <32 wks), extermely preterm (<28 weeks) [varname: PRETERMBIRTH_CAT]

# Forms and variables needed: 
# M09_BIRTH_DSTERM_INF1-4 [MNH09]
# GESTAGEBIRTH_BOE [mnh01_constructed]

#*****************************************************************************
## Forms required: MNH01 constructed, MNH09_constructed

preterm_birth <- mnh01_constructed %>% 
  ## 1. Generate indicator variable for those who have had a birth outcome ## 
  # merge in MNH09 labor and delivery 
  left_join(mnh09_constructed, by = c("SITE", "MOMID", "PREGID")) %>% 
  # generate indicator variable for having a birth outcome
  mutate(BIRTH_OUTCOME = ifelse(M09_BIRTH_DSTERM_INF1 == 1 | M09_BIRTH_DSTERM_INF1 == 2 | 
                                  M09_BIRTH_DSTERM_INF2 == 1 | M09_BIRTH_DSTERM_INF2 == 2 | 
                                  M09_BIRTH_DSTERM_INF3 == 1 | M09_BIRTH_DSTERM_INF3 == 2 |
                                  M09_BIRTH_DSTERM_INF4 == 1 | M09_BIRTH_DSTERM_INF4 == 2, 1, 0)) %>% 
  #e only want those who have had a birth outcome 
  filter(BIRTH_OUTCOME == 1) %>% 
  # generate variable for fetal loss - if you have had at least one fetal loss (for multiples) then you get a "fetal loss" indicator
  mutate(LIVEBIRTH = ifelse(M09_BIRTH_DSTERM_INF1 == 2 | M09_BIRTH_DSTERM_INF2 == 2 | M09_BIRTH_DSTERM_INF3== 2 | M09_BIRTH_DSTERM_INF4 == 2, 0,
                            ifelse(M09_BIRTH_DSTERM_INF1 == 1 | M09_BIRTH_DSTERM_INF2 == 1 | M09_BIRTH_DSTERM_INF3== 1 | M09_BIRTH_DSTERM_INF4 == 1, 1, 55))) %>% 
  ## 2. Generate Outcomes ## 
  # a. Preterm birth (<37 weeks): Delivery prior to 37 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT37]
  mutate(PRETERMBIRTH_LT37 = ifelse(GESTAGEBIRTH_BOE >= 20 & GESTAGEBIRTH_BOE < 37, 1, 0)) %>% 
  
  # b. Preterm birth (<34 weeks): Delivery prior to 34 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT34]
  mutate(PRETERMBIRTH_LT34 = ifelse(GESTAGEBIRTH_BOE >= 20 & GESTAGEBIRTH_BOE < 34, 1, 0)) %>% 
  
  # c. Preterm birth (<32 weeks): Delivery prior to 32 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT32]
  mutate(PRETERMBIRTH_LT32 = ifelse(GESTAGEBIRTH_BOE >= 20 & GESTAGEBIRTH_BOE < 32, 1, 0)) %>% 
  
  # d. Preterm birth (<28 weeks): Delivery prior to 28 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT28]
  mutate(PRETERMBIRTH_LT28 = ifelse(GESTAGEBIRTH_BOE >= 20 & GESTAGEBIRTH_BOE < 28, 1, 0)) %>% 
  
  # e. Preterm birth severity (categorical): term (>37 wks), Late preterm (34 to <37 wks), early preterm (32 to <34 wks), very preterm (28 to <32 wks), extermely preterm (<28 weeks) [varname: PRETERMBIRTH_CAT]
  mutate(PRETERMBIRTH_CAT = ifelse(GESTAGEBIRTH_BOE >= 37, 11,  
                                   ifelse(GESTAGEBIRTH_BOE >= 34 & GESTAGEBIRTH_BOE < 37, 12, 
                                          ifelse(GESTAGEBIRTH_BOE >= 32 & GESTAGEBIRTH_BOE < 34, 13,
                                                 ifelse(GESTAGEBIRTH_BOE >= 28 & GESTAGEBIRTH_BOE < 32, 14,
                                                        ifelse(GESTAGEBIRTH_BOE >= 20 & GESTAGEBIRTH_BOE <28, 15, 55)))))) %>% 
  # only need a subset of vars
  select(SITE, MOMID, PREGID, M01_US_OHOSTDAT,GA_DIFF_DAYS, BIRTH_OUTCOME,LMP_GA_WKS,US_GA_WKS, BOE_GA_WKS, contains("PRETERMBIRTH_"), LIVEBIRTH,GESTAGEBIRTH_BOE)

write.csv(preterm_birth, paste0(path_to_save, "preterm_birth" ,".csv"), row.names=FALSE)

#*****************************************************************************
#* 3. Size for Gestational Age (SGA)
# a. Size for gestational age - categorical. [varname: SGA_CAT]
# b. Preterm small for gestational age: Preterm < 37 weeks AND SGA (<10th). [varname: INF_SGA_PRETERM]
# c. Preterm appropriate for gestational age: Preterm < 37 weeks AND not SGA (<10th). [varname: INF_AGA_PRETERM]
# d. Term small for gestational age: Term >=37 weeks AND SGA (<10th). [varname: INF_SGA_TERM]
# e. Term appropriate for gestational age: Term >=37 weeks AND not SGA (<10th). [varname: INF_AGA_TERM]

# Forms and variables needed: 
# EDD_BOE [mnh09_long]
# GESTAGEBIRTH_BOE [mnh09_long]
# BIRTH_DSTERM_INF1-4 [mnh09]
# SEX_INF1-4 [mnh09]
# BWEIGHT_PRISMA [mnh11_constructed]
# BWEIGHT_ANY [mnh11_constructed]
# PRETERMBIRTH_LT37 [preterm_birth; generated in section above]
# PRETERMBIRTH_CAT [preterm_birth; generated in section above]

#*****************************************************************************
sga <- mnh09_long %>% 
  select(SITE, MOMID, PREGID, GESTAGEBIRTH_BOE_DAYS, GESTAGEBIRTH_BOE,M09_INFANTS_FAORRES, INFANTID, M09_BIRTH_DSTERM, M09_SEX)  %>% 
  ## only want live births 
  filter(M09_BIRTH_DSTERM == 1) %>% 
  ## merge with mnh11 
  left_join(mnh11_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  ## convert weight from grams to kg 
  mutate(BWEIGHT_PRISMA_KG = ifelse(BWEIGHT_PRISMA > 0, BWEIGHT_PRISMA/1000, -5), 
         BWEIGHT_ANY_KG =ifelse(BWEIGHT_ANY < 0 | is.na(BWEIGHT_ANY), -5, BWEIGHT_ANY/1000)) %>% 
  ## calculate percentile - GA at birth must be between 232 and 300 days, bweight must be greater than 0 (remove default value)
  mutate(SGA = ifelse(is.na(GESTAGEBIRTH_BOE_DAYS), -5,  
                      ifelse(M09_SEX == 1  & BWEIGHT_ANY_KG > 0 & (GESTAGEBIRTH_BOE_DAYS >= 232 & GESTAGEBIRTH_BOE_DAYS <=300), floor(igb_wtkg2centile(GESTAGEBIRTH_BOE_DAYS, BWEIGHT_ANY_KG, sex = "Male")),
                             ifelse(M09_SEX == 2 & BWEIGHT_ANY_KG > 0 & (GESTAGEBIRTH_BOE_DAYS >= 232 & GESTAGEBIRTH_BOE_DAYS <=300), floor(igb_wtkg2centile(GESTAGEBIRTH_BOE_DAYS, BWEIGHT_ANY_KG, sex = "Female")),
                                    ifelse((GESTAGEBIRTH_BOE_DAYS > 0 & GESTAGEBIRTH_BOE_DAYS <232) | GESTAGEBIRTH_BOE_DAYS > 300, -5,
                                           ifelse(BWEIGHT_ANY_KG <= 0, -5, -5)))))) %>% 
  
  ## START CONSTRUCTING OUTCOMES ##
  # a. Size for gestational age - categorical. [varname: SGA_CAT]
  mutate(SGA_CAT = ifelse(SGA >= 0 & SGA < 3, 11, # SGA < 3rd
                          ifelse(SGA >= 3 & SGA < 10, 12, # SGA < 10rd
                                 ifelse(SGA >= 10 & SGA < 90, 13, # AGA 10to <90th 
                                        ifelse(SGA >= 90, 14, 55)))))  %>%  # LGA >= 90; 55 for missing
  select(-GESTAGEBIRTH_BOE) %>% 
  ## merge with preterm births dataset to get preterm vars 
  left_join(preterm_birth, by = c("SITE", "MOMID", "PREGID")) %>%
  # b. Preterm small for gestational age: Preterm < 37 weeks AND SGA (<10th). [varname: INF_SGA_PRETERM]
  mutate(INF_SGA_PRETERM = ifelse(PRETERMBIRTH_LT37 == 1 & SGA_CAT == 12, 1, 
                                  ifelse(SGA_CAT == 55, 55, 0))) %>% 
  # c. Preterm appropriate for gestational age: Preterm < 37 weeks AND not SGA (<10th). [varname: INF_AGA_PRETERM]
  mutate(INF_AGA_PRETERM = ifelse(PRETERMBIRTH_LT37 == 1 & (SGA_CAT == 13 | SGA_CAT == 14), 1, 0)) %>% 
  # d. Term small for gestational age: Term >=37 weeks AND SGA (<10th). [varname: INF_SGA_TERM]
  mutate(INF_SGA_TERM = ifelse(PRETERMBIRTH_CAT == 11 & SGA_CAT == 12, 1, 0)) %>% 
  # e. Term appropriate for gestational age: Term >=37 weeks AND not SGA (<10th). [varname: INF_AGA_TERM]
  mutate(INF_AGA_TERM = ifelse(PRETERMBIRTH_CAT == 11 & (SGA_CAT == 13 | SGA_CAT == 14), 1, 0))  %>% 
  select(SITE, MOMID, PREGID, INFANTID, LMP_GA_WKS, US_GA_WKS, BOE_GA_WKS, GESTAGEBIRTH_BOE_DAYS, GESTAGEBIRTH_BOE, M11_BW_EST_FAORRES, M11_BW_FAORRES, M11_BW_FAORRES_REPORT, BWEIGHT_ANY, 
         SGA,SGA_CAT) %>% 
  # generate reasons missing
  mutate(missingboe = ifelse(is.na(LMP_GA_WKS) & is.na(US_GA_WKS) & is.na(BOE_GA_WKS), 1, 0), 
         gestagebirth_under33 = ifelse(GESTAGEBIRTH_BOE_DAYS < 232, 1, 0), 
         gestagebirth_over42 = ifelse(GESTAGEBIRTH_BOE_DAYS > 400, 1, 0),
         missingmnh11 = ifelse(is.na(M11_BW_EST_FAORRES) & is.na(M11_BW_FAORRES) & is.na(M11_BW_FAORRES_REPORT), 1, 0),
         timegreater7d = ifelse(M11_BW_EST_FAORRES > 168, 1, 0), 
         missing_weight = ifelse(BWEIGHT_ANY < 0, 1, 0)) 

# export
write.csv(sga, paste0(path_to_save, "sga" ,".csv"), row.names=FALSE)

#*****************************************************************************
#* Mortality
#  4. Neonatal mortality 
# a. <24 hours 
# b. Early neontal mortality: first  7 days 
# c. Late neonatal mortality: between 7 & 28 days

# 5. Infant mortality 

# Forms and variables needed: 
# M11_INF_DSTERM [mnh11]
# AGEDEATH [timevarying_constructed]
# AGEDEATH_HRS [timevarying_constructed]
# DTH_INDICATOR [timevarying_constructed]
# DOB [MNH09]
# AGEDEATH_HRS [AGE_LAST_SEEN]

#*****************************************************************************
## DENOMINATOR is anyone with: 
# Date of birth (mnh09)
# live birth (mnh11)
# have to be able to link mnh09 with mnh11

mortality <- timevarying_constructed %>% 
  # generate new variable for birth outcome (if they are missing mnh11, then this should be 55)
  mutate(BIRTH_OUTCOME = ifelse(is.na(M11_INF_DSTERM) | M11_INF_DSTERM == 77, 55, M11_INF_DSTERM)) %>% # where livebirth=1, fetal loss=2, missing=55
  
  ## generate variable if death is reported among live birth but is missing time of death 
  mutate(DTH_TIME_MISSING = ifelse((BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & is.na(AGEDEATH)) | 
                                     (BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & AGEDEATH < 0), 1, 0)) %>% 
  
  ## generate outcome for neonatal death if infant dies <28 days of life
  mutate(NEO_DTH = ifelse((BIRTH_OUTCOME==1 & DTH_INDICATOR==1 & is.na(AGEDEATH)) | (BIRTH_OUTCOME==1 & DTH_INDICATOR==1 & AGEDEATH < 0), 66,  ## if missing the three criteria, they are missing
                          ifelse(DTH_INDICATOR == 0, 0, 
                                 ifelse(M11_INF_DSTERM == 1 & AGEDEATH >= 0 & AGEDEATH < 28, 1, ## if live birth AND age of death is <= 28, they get a 1
                                        ifelse(is.na(M11_INF_DSTERM) & is.na(DOB), 55, 0))))) %>%  ## 66 - THESE ARE PEOPLE WHO ARE REPORTING A DEATH BUT ARE MISSING OR HAVE INVALID AGE AT DEATH
  
  ## generate outcome for infant death if infant dies <365 days of life
  mutate(INF_DTH = ifelse((BIRTH_OUTCOME==1 & DTH_INDICATOR==1 & is.na(AGEDEATH)) | (BIRTH_OUTCOME == 1 & DTH_INDICATOR==1 & AGEDEATH < 0), 66,  ## if missing the three criteria, they are missing
                          ifelse(DTH_INDICATOR == 0, 0, 
                                 ifelse(M11_INF_DSTERM == 1 & AGEDEATH < 365, 1, ## if live birth AND age of death is < 365, they get a 1
                                        ifelse(is.na(M11_INF_DSTERM) & is.na(DOB), 55, 0))))) %>%  ## 66 - THESE ARE PEOPLE WHO ARE REPORTING A DEATH BUT ARE MISSING OR HAVE INVALID AGE AT DEATH
  
  ## timing of neonatal mortality
  mutate(DTH_0D = ifelse(DTH_INDICATOR ==1 & is.na(AGEDEATH_HRS), 0,
                         ifelse(DTH_INDICATOR ==1 & AGEDEATH_HRS < 24, 1, 0)),
         DTH_7D = ifelse(DTH_INDICATOR ==1 & AGEDEATH >=0 & AGEDEATH < 7, 1, 0), 
         DTH_28D = ifelse(DTH_INDICATOR ==1 & AGEDEATH >=7 & AGEDEATH < 28, 1, 0),
         DTH_365D = ifelse(DTH_INDICATOR ==1 & AGEDEATH >=28 & AGEDEATH < 365, 1, 0)) %>% 
  
  ## calculate denominators based on age last seen 
  mutate(DENOM_28d = ifelse(AGE_LAST_SEEN >= 28 | DTH_28D == 1, 1, 0),
         DENOM_365d = ifelse(AGE_LAST_SEEN >= 365 | DTH_365D == 1, 1, 0)) %>% 
  
  # generate indicator variables where things could go wrong with this outcome
  mutate(ID_MISSING_ENROLLMENT = ifelse(PREGID %in% enrolled_ids_vec, 0, 1),
         DOB_AFTER_DEATH = ifelse(DTHDAT < DOB, 1, 0), ## if death comes before dob
         MISSING_MNH09 = ifelse(is.na(M09_MAT_VISIT_MNH09), 1, 0), ## if mnh09 is missing (we need this for DOB)
         MISSING_MNH11 = ifelse(is.na(M11_INF_VISIT_MNH11), 1, 0), ## if mnh11 is missing (we need this form for birth outcome)
         INVALID_DTH_REPORT = ifelse(BIRTH_OUTCOME ==2  & DTH_INDICATOR == 1, 1, 0) # in order to be included as a neonatal or infant death, the infant had to have been born alive
  ) %>% 
  select(SITE, MOMID, PREGID, INFANTID, CLOSEOUT, DOB, DELIVERY_DATETIME, BIRTH_OUTCOME, DATE_LAST_SEEN, 
         AGE_LAST_SEEN, DTH_INDICATOR, DTHDAT, DEATH_DATETIME,DTH_TIME_MISSING, AGEDEATH, AGEDEATH_HRS,NEO_DTH,INF_DTH, 
         ID_MISSING_ENROLLMENT,DOB_AFTER_DEATH, contains("MISSING"),contains("DTH"), contains("DENOM"), INVALID_DTH_REPORT, contains("has")) 

# export
write.csv(mortality, paste0(path_to_save, "mortality" ,".csv"), row.names=FALSE)

#  4. Neonatal mortality: Denominator is all live births reported in MNH11 with mnh09 filled out 
# a. <24 hours 
# b. Early neontal mortality: first  7 days 
# c. Late neonatal mortality: between 7 & 28 days
neonatal_mortality <- mortality %>% 
  ## filter for live births
  filter(BIRTH_OUTCOME == 1) %>%
  # generate categorical outcome
  mutate(NEO_DTH_CAT = ifelse(DTH_INDICATOR == 0, 10, 
                              ifelse(DTH_0D == 1, 11, 
                                     ifelse(DTH_7D == 1, 12, 
                                            ifelse(DTH_28D==1, 13, 
                                                   ifelse((BIRTH_OUTCOME ==1 & DTH_INDICATOR == 1 & 
                                                             is.na(AGEDEATH)) | 
                                                            (BIRTH_OUTCOME == 1 &DTH_INDICATOR == 1 & AGEDEATH < 0), 66, 55)))))) %>% 
  # generate total neonatal deaths 
  mutate(TOTAL_NEO_DEATHS = ifelse(DTH_INDICATOR ==1 & AGEDEATH < 28, 1, 0))

# export
write.csv(neonatal_mortality, paste0(path_to_save, "neonatal_mortality" ,".csv"), row.names=FALSE)

#  5. Infant mortality 
# a.<365 days
infant_mortality <- mortality %>% 
  ## filter for live births
  filter(BIRTH_OUTCOME == 1) %>% 
  mutate(INF_DTH_CAT = ifelse(DTH_INDICATOR == 0, 10, 
                              ifelse(DTH_0D == 1, 11, 
                                     ifelse(DTH_7D == 1, 12, 
                                            ifelse(DTH_28D==1, 13, 
                                                   ifelse(DTH_365D==1,14,
                                                          ifelse((BIRTH_OUTCOME == 1 & DTH_INDICATOR == 1 & is.na(AGEDEATH)) |
                                                                   (BIRTH_OUTCOME == 1 &DTH_INDICATOR == 1 & AGEDEATH < 0), 66, 55))))))) %>% 
  # generate total infant deaths 
  mutate(TOTAL_INF_DEATHS = ifelse(DTH_INDICATOR ==1 & (AGEDEATH >= 28 & AGEDEATH < 365), 1, 0))



# export
write.csv(infant_mortality, paste0(path_to_save, "infant_mortality" ,".csv"), row.names=FALSE)

#*****************************************************************************
#* 6. Stillbirth
# a. STILLBIRTH_SIGNS_LIFE 
# b. STILLBIRTH_20WK
# c. STILLBIRTH_22WK
# d. STILLBIRTH_28WK
# e. STILLBIRTH_GESTAGE_CAT
# f. STILLBIRTH_TIMING

# Forms and variables needed: 
# M04_PRG_DSDECOD [mnh04]
# GESTAGEBIRTH_BOE [mnh09_constructed]
# GESTAGE_FETAL_LOSS_WKS [mnh04_constructed]
# CRY_CEOCCUR_INF1-4 [MNH09]
# FHR_VSTAT_INF1-4 [MNH09]
# MACER_CEOCCUR_INF1-4 [MNH09]
# CORD_PULS_CEOCCUR_INF1-4 [MNH09]
#*****************************************************************************

# filter mnh04_constructed to include only fetal losses reported (M04_PRG_DSDECOD == 2)
mnh04_constructed_fetal_loss <- mnh04_constructed %>% filter(M04_PRG_DSDECOD == 2)

stillbirth <- mnh09_long %>% 
  select(SITE, MOMID, PREGID, INFANTID, DOB, M09_MAT_VISIT_MNH09, GESTAGEBIRTH_BOE, M09_CRY_CEOCCUR, M09_FHR_VSTAT, 
         M09_MACER_CEOCCUR, M09_CORD_PULS_CEOCCUR) %>%   
  # merge in mnh01 information to obtain boe 
  left_join(mnh01_constructed[c("SITE", "MOMID", "PREGID", "EDD_BOE", "BOE_GA_WKS")],  # "M01_US_GA_WKS_AGE_FTS1", "M01_US_GA_WKS_AGE_FTS2", "M01_US_GA_DAYS_AGE_FTS1", "M01_US_GA_DAYS_AGE_FTS2","M01_GA_LMP_WEEKS_SCORRES", "M01_GA_LMP_DAYS_SCORRES"
            by = c("SITE", "MOMID", "PREGID")) %>% 
  # merge in fetal loss information from mnh04 
  left_join(mnh04_constructed_fetal_loss, by = c("SITE", "MOMID", "PREGID")) %>%  # dim = 1236
  # merge in mnh11 information to get birth outcome and signs of live
  full_join(mnh11_constructed[c("SITE", "MOMID", "PREGID", "INFANTID", "M11_INF_DSTERM", "M11_BREATH_FAIL_CEOCCUR")], 
            by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% # dim = 1246 (n=10 are weird ghana ids in mnh11)
  # re-classify fetal loss code from mnh04 to be 0 for those without a fetal loss 
  mutate(M04_PRG_DSDECOD = ifelse(is.na(M04_PRG_DSDECOD), 0 , M04_PRG_DSDECOD)) %>% 
  ## add new var with a single ga at birth --use mnh09, if missing, use mnh04 
  mutate(GA_AT_BIRTH_ANY = ifelse(is.na(GESTAGEBIRTH_BOE), GESTAGE_FETAL_LOSS_WKS, GESTAGEBIRTH_BOE)) %>% 
  ## add new var with a single fetal loss --use mnh11, if missing, use mnh04 
  mutate(BIRTH_OUTCOME = ifelse(M11_INF_DSTERM==2 | M04_PRG_DSDECOD!=0, 0, 
                                ifelse(M11_INF_DSTERM ==1, 1, NA))) %>%  ## where 1 = live birth and 2 = fetal loss/fetal death
  ## add new var if the ga at birth is <20wks 
  mutate(GESTAGE_UNDER20 = ifelse(GESTAGEBIRTH_BOE<20, 1,0)) %>% 
  ## START CONSTRUCTING OUTCOMES ## Death prior to delivery of a fetus at â¥20 weeks of gestation (or >350 g weight, if gestational age is unavailable).
  # a. STILLBIRTH_SIGNS_LIFE: Delivery of a fetus showing no signs of life, as indicated by absence of breathing, heartbeat, pulsation of the umbilical cord, or definite movements of voluntary muscles.
  # 1, Yes = Definitely live birth: cried, pulsate, initiated and sustained breathing
  # 0, No = Definitely dead: no heart rate, macerated
  ## QUESTION: how to handle NAs/77 here 
  mutate(STILLBIRTH_SIGNS_LIFE = ifelse(M09_CRY_CEOCCUR ==1 | M09_CORD_PULS_CEOCCUR ==1 | M11_BREATH_FAIL_CEOCCUR==0, 1, 
                                        ifelse(M09_FHR_VSTAT ==0 | M09_MACER_CEOCCUR ==1, 0, 55))) %>%  ## come back to -- why aren't the NAs replacing correct?
  # b. STILLBIRTH_20WK
  mutate(STILLBIRTH_20WK = ifelse(is.na(GA_AT_BIRTH_ANY), 55,
                                  ifelse(GA_AT_BIRTH_ANY < 20, 66,
                                         ifelse(GA_AT_BIRTH_ANY >= 20 & BIRTH_OUTCOME ==0, 1, # if birth outcome is fetal loss and gestage birth is >= 20wks
                                                ifelse(BIRTH_OUTCOME == 0 & STILLBIRTH_SIGNS_LIFE == 1, 66,  # if fetal loss reported but there are signs of life reported -- 66 
                                                       ifelse(BIRTH_OUTCOME == 1 & STILLBIRTH_SIGNS_LIFE == 0, 99, 0)))))) %>% # if fetal loss not reported but there are no signs of life  -- 66 
  ## if missing ga at birth & ga at fetal loss--55
  # c. STILLBIRTH_22WK
  mutate(STILLBIRTH_22WK = ifelse(STILLBIRTH_20WK ==1 & GESTAGEBIRTH_BOE >= 22, 1, 0)) %>% # if birth outcome is fetal loss and gestage birth is >= 22wks
  
  # d. STILLBIRTH_28WK
  mutate(STILLBIRTH_28WK = ifelse(STILLBIRTH_20WK ==1 & GESTAGEBIRTH_BOE >= 28, 1, 0)) %>% # if birth outcome is fetal loss and gestage birth is >= 28wks
  
  # e. STILLBIRTH_GESTAGE_CAT - 
  mutate(STILLBIRTH_GESTAGE_CAT = ifelse(BIRTH_OUTCOME==1, 14, ##live birth
                                         ifelse(STILLBIRTH_20WK == 1 & GESTAGEBIRTH_BOE>=20 & GESTAGEBIRTH_BOE <=27, 11, # "Early: Death prior to delivery of a fetus at 20 to 27 weeks of gestation.  
                                                ifelse(STILLBIRTH_20WK == 1 & GESTAGEBIRTH_BOE>=28 & GESTAGEBIRTH_BOE <=36, 12, # Late: Death prior to delivery of a fetus at 28 to 36 weeks of gestation.
                                                       ifelse(STILLBIRTH_20WK == 1, 13, # Term: Death prior to delivery of a fetus at â¥37 weeks of gestation.    
                                                              ifelse(GESTAGEBIRTH_BOE<20, 66, 55) # if GA at bith is <20, exclude from these categories
                                                       ))))) %>% 
  # f. STILLBIRTH_TIMING
  mutate(STILLBIRTH_TIMING = ifelse(STILLBIRTH_20WK==1 & (is.na(M09_FHR_VSTAT) & is.na(M09_MACER_CEOCCUR)), 55, 
                                    ifelse(BIRTH_OUTCOME==1, 13, ## live birth
                                           ifelse(STILLBIRTH_20WK == 1 & (M09_FHR_VSTAT==0 | M09_MACER_CEOCCUR ==1), 11, # Antepartum: heart rate not detected â¥12 hrs prior to delivery AND/OR signs of maceration.
                                                  ifelse(STILLBIRTH_20WK == 1 & (M09_FHR_VSTAT ==1 & M09_MACER_CEOCCUR == 0), 12, 99))))) %>% 
  
  mutate(STILLBIRTH_TIMING =  ifelse(is.na(STILLBIRTH_TIMING) & SITE == "Zambia", 99, STILLBIRTH_TIMING)) %>% ## COME BACK TO AND FIX
  # STILLBIRTH_DENOMINATOR 
  mutate(STILLBIRTH_DENOM = ifelse(BIRTH_OUTCOME==1 | BIRTH_OUTCOME==0, 1, 0)) %>% 
  ## EXTRA INFO FOR REPORT ## 
  # are there people with a fetal loss in mnh04 but are missing mnh09
  mutate(MISSING_MNH09 = ifelse(M04_PRG_DSDECOD == 2 & is.na(M09_MAT_VISIT_MNH09), 1, 0)) %>% 
  # Missing signs of life information -- denomintaor is anyone who had a fetal loss reported in mnh04 or mnh09 
  mutate(MISSING_SIGNS_OF_LIFE = ifelse(BIRTH_OUTCOME==0 & (M09_CRY_CEOCCUR == 77 | M09_CORD_PULS_CEOCCUR == 77 | 
                                                              M09_FHR_VSTAT==77 | M09_MACER_CEOCCUR == 77 | M11_BREATH_FAIL_CEOCCUR == 77), 1, 0))

# export data 
write.csv(stillbirth, paste0(path_to_save, "stillbirth" ,".csv"), row.names=FALSE)


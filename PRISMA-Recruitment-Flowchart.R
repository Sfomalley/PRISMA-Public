#*****************************************************************************
#* PRISMA Recruitment Flow Chart 
#* Drafted: 11 October 2023, Stacie Loisate
#* Last updated: 12 October 2023
 
 
#* This code will output quick cross tabs of the data that corresponds to the flowchart linked here: 
#* Due to site-specific processes, we have left areas for sites to use, if needed (with "FOR SITES TO FILL OUT" in the code below)

#* Steps to running the code: 
  # 1. import the forms listed below in the "Data Setup" section
  # 2. make sure the variable names are uppercase
  # 3. add any site specific processes to the codes in the "flow chart code" section
  # 4. run the "flow chart code" section
#*****************************************************************************
#* Data Setup 
#*****************************************************************************
library(tidyverse)
library(readr)
library(lubridate)

# set path to data
path_to_data <- paste("Z:/SynapseCSVs/",site, "/", UploadDate, sep = "")

# import forms 
mnh00 <- read_csv(paste0(path_to_data,"/", "mnh00.csv")) 
mnh02 <- read_csv(paste0(path_to_data,"/", "mnh02.csv")) 
mnh03 <- read_csv(paste0(path_to_data,"/", "mnh03.csv")) 
mnh04 <- read_csv(paste0(path_to_data,"/", "mnh04.csv")) 
mnh05 <- read_csv(paste0(path_to_data,"/", "mnh05.csv")) 
mnh06 <- read_csv(paste0(path_to_data,"/", "mnh06.csv")) 
mnh07 <- read_csv(paste0(path_to_data,"/", "mnh07.csv")) 
mnh08 <- read_csv(paste0(path_to_data,"/", "mnh08.csv"))
mnh25 <- read_csv(paste0(path_to_data,"/", "mnh25.csv")) 

#*****************************************************************************
#* Flow chart code
#*****************************************************************************
####################################################
# Prescreened 
  # N = MNH00s completed
####################################################
# TABLE OF PRE-SCREENED 
table("Pre-Screened" = nrow(mnh00))

# Tables of where people may have fallen out between prescreening and screening 
table("Has clinical signs of pregnancy" = mnh00$PREGNANT_IEORRES) 
table("GA <=25 weeks" = mnh00$EGA_LT25_IEORRES) 
table("Lives within catchment area" = mnh00$CATCHMENT_IEORRES) 
table("Meets age requirement" = mnh00$AGE_IEORRES) 
table("Willing and able to provide consent" = mnh00$CON_YN_DSDECOD) 
table("Willing and able to provide consent (legal representative/guardian)" = mnh00$CON_LAR_YN_DSDECOD) 
table("Willing and able to provide assent" = mnh00$ASSNT_YN_DSDECOD) 
table("Other reason for exclusion" = mnh00$OTHR_IEORRES) 
table("Other reason for exclusion - specify" = mnh00$OTHR_REASON_IEORRES)
#table("Other reason for exclusion - other, specify (text)" = mnh00$OTHR_SPFY_IEORRES) 
# FOR SITES TO FILL OUT, if applicable, based on screening flow) : 
  # table("Did not come in for ultrasound " = mnh00$[SITE INSERT VAR]) 

####################################################
# Screened 
# N= MNH02 completed 
####################################################
# TABLE OF SCREENED 
table("Screened" = nrow(mnh02))


# Tables of where people may have fallen out between screening and enrollment 
table("Viable intrauterine pregnancy less than 20 weeks gestation confirmed via ultrasound" = mnh02$PC_IEORRES) 
table("Meets age requirement" = mnh02$AGE_IEORRES) 
table("Lives within catchment area" = mnh02$CATCHMENT_IEORRES) 
table("Plans to remain within catchment area" = mnh02$CATCH_REMAIN_IEORRES) 
table("Willing and able to provide consent" = mnh02$CONSENT_IEORRES) 
# FOR SITES TO FILL OUT: 
# table("Other (For sites to use as needed)" = mnh02$[SITE INSERT VAR]) 

####################################################
# Enrolled 
# N= MNH02 where PC_IEORRES=1 & AGE_IEORRES=1 & CATCHMENT_IEORRES=1 & CATCH_REMAIN_IEORRES=1& CONSENT_IEORRES=1 
####################################################
# generate enrolled variable using the criteria on the line above 
mnh02$ENROLLED = ifelse(mnh02$PC_IEORRES==1 & 
                          mnh02$AGE_IEORRES==1 & 
                          mnh02$CATCHMENT_IEORRES==1 & 
                          mnh02$CATCH_REMAIN_IEORRES==1 & 
                          mnh02$CONSENT_IEORRES==1, 1, 0)

# TABLE OF ENROLLED  
table("Enrolled" = mnh02$ENROLLED)

# FOR SITES TO FILL OUT: 
# List any reason a mother was enrolled and then did not have an enrollment ANC visit (For sites to use as needed)
# table("Reason enrolled but missing enrollment ANC visit" = mnh02$[SITE INSERT VAR]) 

####################################################
# FORM COMPLETION (MNH03, MNH04, MNH05, MNH06, MNH07, MNH08, MNH25) 

  # N= MNHxx where TYPE_VISIT=1 and MAT_VISIT_MNHxx=1,2 
  # Note: MNH03 is only required at enrollment: 
      # count the number of rows in the dataset. should be the same as enrolled
####################################################
# MNH03 complete at enrollment 
table("MNH03 at enrollment completed" = nrow(mnh03))

# MNH04 complete at enrollment 
  # N = MNH04 where TYPE_VISIT=1 and MAT_VISIT_MNH04= 1,2
mnh04$VISIT_COMPLETE = ifelse(mnh04$TYPE_VISIT==1 & (mnh04$MAT_VISIT_MNH04 %in% c(1,0)), 1, 0) # generate visit complete variable with type visit and mat visit status
table("MNH04 at enrollment completed" = mnh04$VISIT_COMPLETE)

# MNH05 complete at enrollment 
  # N = MNH05 where TYPE_VISIT=1 and MAT_VISIT_MNH05= 1,2
mnh05$VISIT_COMPLETE = ifelse(mnh05$TYPE_VISIT==1 & (mnh05$MAT_VISIT_MNH05 %in% c(1,0)), 1, 0) # generate visit complete variable with type visit and mat visit status
table("MNH05 at enrollment completed" = mnh05$VISIT_COMPLETE)

# MNH06 complete at enrollment 
  # N = MNH06 where TYPE_VISIT=1 and MAT_VISIT_MNH05= 1,2
mnh06$VISIT_COMPLETE = ifelse(mnh06$TYPE_VISIT==1 & (mnh06$MAT_VISIT_MNH06 %in% c(1,0)), 1, 0) # generate visit complete variable with type visit and mat visit status
table("MNH06 at enrollment completed" = mnh06$VISIT_COMPLETE)

# MNH07 complete at enrollment 
  # N = MNH07 where TYPE_VISIT=1 and MAT_VISIT_MNH05= 1,2
mnh07$VISIT_COMPLETE = ifelse(mnh07$TYPE_VISIT==1 & (mnh07$MAT_VISIT_MNH07 %in% c(1,0)), 1, 0) # generate visit complete variable with type visit and mat visit status
table("MNH07 at enrollment completed" = mnh07$VISIT_COMPLETE)

# MNH08 complete at enrollment 
  # N = MNH08 where TYPE_VISIT=1 and MAT_VISIT_MNH05= 1,2
mnh08$VISIT_COMPLETE = ifelse(mnh08$TYPE_VISIT==1 & (mnh08$MAT_VISIT_MNH08 %in% c(1,0)), 1, 0) # generate visit complete variable with type visit and mat visit status
table("MNH08 at enrollment completed" = mnh08$VISIT_COMPLETE)

# MNH25 complete at enrollment 
  # N = MNH25 where TYPE_VISIT=1 and MAT_VISIT_MNH05= 1,2
mnh25$VISIT_COMPLETE = ifelse(mnh25$TYPE_VISIT==1 & (mnh25$MAT_VISIT_MNH25 %in% c(1,0)), 1, 0) # generate visit complete variable with type visit and mat visit status
table("MNH25 at enrollment completed" = mnh25$VISIT_COMPLETE)


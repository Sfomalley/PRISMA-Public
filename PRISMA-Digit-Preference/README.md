PRISMA Digit Preference
================

## Description

This folder contains an R and STATA file to run a digit preference analysis for MUAC, Maternal resipiratory rate, and SpO2. This code can be modified to check more variables by adding in different variable names. 

Digit preference occurs when a large frequency of values fall on the same number. For example, birth weights being reported as 3200g, when it should be reported as 3125g. 

#### :pushpin: Updated on 13 September 2023
#### :pushpin: Originally drafted by: 
  + R code: Xiaoyan Hu (xyh@gwu.edu) 
  + STATA code: Xiaoyan Hu (xyh@gwu.edu)  & Stacie Loisate (stacie.loisate@gwu.edu) 

## What data is required:
* MNH05: Maternal Anthropometry
  + Digit Preference Variables: `MUAC_PERES`
* MNH06: Maternal Point of Care Diagnostics
  + Digit Preference Variables: `PULSEOX_VSORRES`, `RR_VSORRES`

## Codes included:

**`STATA_PRISMA_Digit_Preference.log`** outputs .png histograms of digit preference. 

**`R_PRISMA_Digit_Preference.R`** outputs .pdf histograms of digit preference. 



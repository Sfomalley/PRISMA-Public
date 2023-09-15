#*****************************************************************************
#*Skip logic check
#*--check if the skip logic applied correctly
#*****************************************************************************
rm(list = ls())
library(tidyverse)
library(readxl)
library(xlsx)
library(data.table)
site = "Pakistan"
UploadDate = "2023-03-17"

#*****************************************************************************
#* read data (dataForm)
#*****************************************************************************
setwd(paste0("Z:/SynapseCSVs/",site,"/",UploadDate)) 

##import raw .CSVs in wide format 
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

# make sure all column names are uppercase 
myfiles <- lapply(myfiles, function (x){
  upper <- toupper(names(x))
  setnames(x, upper)
})

# convert to individual dataframes 
names(myfiles) <- gsub(".csv", "", temp)
list2env(myfiles, globalenv())

# derive date variable "VisitDate" for each form (can remove if we don't output date)
if (exists("mnh00") == TRUE){
  mnh00 <- mnh00 %>% 
    mutate(VisitDate = SCRN_OBSSTDAT)
}

if (exists("mnh01") == TRUE){
  mnh01 <- mnh01 %>% 
    mutate(VisitDate = US_OHOSTDAT)
}

if (exists("mnh02") == TRUE){
  mnh02 <- mnh02 %>% 
    mutate(VisitDate = SCRN_OBSSTDAT)
}

if (exists("mnh03") == TRUE){
  mnh03 <- mnh03 %>% 
    mutate(VisitDate = SD_OBSSTDAT)
}

if (exists("mnh04") == TRUE){
  mnh04 <- mnh04 %>% 
    mutate(VisitDate = ANC_OBSSTDAT)
}

if (exists("mnh05") == TRUE){
  mnh05 <- mnh05 %>% 
    mutate(VisitDate = ANT_PEDAT)
}

if (exists("mnh06") == TRUE){
  mnh06 <- mnh06 %>% 
    mutate(VisitDate = DIAG_VSDAT)
}

if (exists("mnh07") == TRUE){
  mnh07 <- mnh07 %>% 
    mutate(VisitDate = MAT_SPEC_COLLECT_DAT)
}

if (exists("mnh08") == TRUE){
  mnh08 <- mnh08 %>% 
    mutate(VisitDate = LBSTDAT)
}

if (exists("mnh09") == TRUE){
  mnh09 <- mnh09 %>% 
    mutate(VisitDate = MAT_LD_OHOSTDAT)
}

if (exists("mnh10") == TRUE){
  mnh10 <- mnh10 %>% 
    mutate(VisitDate = FORMCOMPLDAT_MNH10) #revise date var for new updates
}

if (exists("mnh11") == TRUE){
  mnh11 <- mnh11 %>% 
    mutate(VisitDate = VISIT_OBSSTDAT)
}

if (exists("mnh12") == TRUE){
  mnh12 <- mnh12 %>% 
    mutate(VisitDate = VISIT_OBSSTDAT)
}

if (exists("mnh13") == TRUE){
  mnh13 <- mnh13 %>% 
    mutate(VisitDate = VISIT_OBSSTDAT)
}

if (exists("mnh14") == TRUE){
  mnh14 <- mnh14 %>% 
    mutate(VisitDate = VISIT_OBSSTDAT)
}

if (exists("mnh15") == TRUE){
  mnh15 <- mnh15 %>% 
    mutate(VisitDate = OBSSTDAT)
}

if (exists("mnh16") == TRUE){
  mnh16 <- mnh16 %>% 
    mutate(VisitDate = VISDAT)
}

if (exists("mnh17") == TRUE){
  mnh17 <- mnh17 %>% 
    mutate(VisitDate = VISDAT)
}

if (exists("mnh18") == TRUE){
  mnh18 <- mnh18 %>% 
    mutate(VisitDate = VISDAT)
}

if (exists("mnh19") == TRUE){
  mnh19 <- mnh19 %>% 
    mutate(VisitDate = OBSSTDAT)
}

if (exists("mnh20") == TRUE){
  mnh20 <- mnh20 %>% 
    mutate(VisitDate = OBSSTDAT)
}

if (exists("mnh21") == TRUE){
  mnh21 <- mnh21 %>% 
    mutate(VisitDate = AESTDAT)
}

if (exists("mnh22") == TRUE){
  mnh22 <- mnh22 %>% 
    mutate(VisitDate = DVSTDAT)
}

if (exists("mnh23") == TRUE){
  mnh23 <- mnh23 %>% 
    mutate(VisitDate = CLOSE_DSSTDAT)
}

if (exists("mnh24") == TRUE){
  mnh24 <- mnh24 %>% 
    mutate(VisitDate = CLOSE_DSSTDAT)
}

if (exists("mnh25") == TRUE){
  mnh25 <- mnh25 %>% 
    mutate(VisitDate = OBSSTDAT)
}

if (exists("mnh26") == TRUE){
  mnh26 <- mnh26 %>% 
    mutate(VisitDate = FTGE_OBSTDAT)
}

#*****************************************************************************
#*read filter (filterForm)
#*****************************************************************************
#read dictionary and keep the columns needed
dict <- read_excel("~/github/Dictionary/PRiSMA-MNH-Data-Dictionary-Repository_V.2.1-OCT182022_filter.xlsx") %>% 
  select(Form, `Variable Name`, `Field Label`, `Response Options`, `Values`, `Filter`, `Field Type (Date, Time, Number, Text)`) %>% 
  rename("FieldType" = `Field Type (Date, Time, Number, Text)`) %>% 
  mutate(`Variable Name` = toupper(`Variable Name`))

#list each forms as in data dictionary
filter <- lapply(c(paste0("MNH0",c(0:9)),
        paste0("MNH",c(10:24)), 
        paste0("MNH25_", site), 
        "MNH26"),
       function(n) {
         filter <- dict %>% filter(Form == n)
       }
)
names(filter) <- c(paste0("filter0", c(0:9)),paste0("filter",c(10:26)))
#split filter list
list2env(filter, globalenv())

#*************************************** Remove this part once we have all filter for mnh04
#MNH04-only include section A, B, C as sample
filter04 <- filter04 %>% 
  slice(1:152) #this number shown how many variables are currently checked (secton A, B, C)
#***************************************

#*****************************************************************************
#*logic check function
#*****************************************************************************
LogicCheck <- function(dataForm, filterForm){
  
  #create variables for checking logic
  varName = colnames(dataForm)
  filterForm$expCount = NA
  filterForm$datCount = NA
  
  #check logic
  for (var in varName) {
    #check if ask all variables has missing value
    for (i in 1:nrow(filterForm)) { 
      if (filterForm$`Variable Name`[i] == var & filterForm$Filter[i] == "ALL") {
        filterForm$expCount[i] = nrow(dataForm)
        filterForm$datCount[i] = sum(!is.na(dataForm[[var]]))
      }
      else if (filterForm$`Variable Name`[i] == var & filterForm$Filter[i] == "OPTIONAL") {
        filterForm$expCount[i] = 0
        filterForm$datCount[i] = 0 #assume datcount == 0 to skip check
      }
      #check if variable has a filter
      #check text, date, time data by filter
      else if (filterForm$`Variable Name`[i] == var & filterForm$FieldType[i] %in% c("Text", "Date", "Time")) {
        filterForm$expCount[i] = sum(eval(parse(text=paste0(deparse(substitute(dataForm)),"$", filterForm$Filter[i]))))
        filterForm$datCount[i] = sum(!(is.na(mnh04[[var]]) | dataForm[[var]] %in% c("n/a","07/07/1907", "77:77","missing","05/05/1905", "55:55")))
      }
      #check categorical data has a filter (if has correct value as listed)
      else if (filterForm$`Variable Name`[i] == var & !is.na(filterForm$Values[i])) {
        filterForm$expCount[i] = sum(eval(parse(text=paste0(deparse(substitute(dataForm)),"$", filterForm$Filter[i]))))
        filterForm$datCount[i] = sum(dataForm[[var]] %in% eval(parse(text = paste0("c(",filterForm$Values[i],")"))))
      }
      #check continuous data has a filter (if value >=0) 
      else if (filterForm$`Variable Name`[i] == var) { #this part can be combined with condition above if we include valid value
        filterForm$expCount[i] = sum(eval(parse(text=paste0(deparse(substitute(dataForm)),"$", filterForm$Filter[i]))))
        filterForm$datCount[i] = sum(dataForm[[var]] >= 0) 
      }
    }
  }
  
  #skip errors overview
  errorForm <- filterForm %>% 
    mutate(
      skipError = case_when(
        expCount == datCount ~ "no error", 
        expCount > datCount ~ "missing", #missing data
        expCount < datCount ~ "extra") #extra data
    ) %>% 
    filter(skipError == "missing" | skipError == "extra") %>% 
    mutate(Comments = "")
  
#skip error cases
  #add id var if not exist
  ID <- c("SCRNID", "MOMID", "PREGID", "INFANTID", "VisitDate")
  for (x in ID) {
    if (!x %in% colnames(dataForm)) {
      dataForm[, x] = NA
    }
  }
  
  #list all vars with error
  checkCaseList <- list()
  for (l in 1:nrow(errorForm)) {
    if (errorForm$Filter[l] == "ALL") {
      checkCase <- dataForm %>% 
        select(all_of(ID), errorForm$`Variable Name`[l]) %>% 
        mutate(skipError = ifelse(
          is.na(eval(parse(text = errorForm$`Variable Name`[l]))) |
            eval(parse(text = errorForm$`Variable Name`[l])) %in% 
            c(-7, 77, "n/a","07/07/1907", "77:77", -5, 55,"missing", "05/05/1905", "55:55"),
          "missing", NA)
        ) %>% 
        add_column(Form = errorForm$Form[l],
                   ParentVar = "", 
                   "ParentVar Value" = "",
                   Variable = errorForm$`Variable Name`[l], 
                   Filter = errorForm$Filter[l]) %>% 
        rename("Variable Value" = errorForm$`Variable Name`[l]) %>% 
        select(Form, all_of(ID), Variable, "Variable Value", ParentVar, "ParentVar Value", Filter, skipError) %>% 
        filter(!is.na(skipError)) %>% 
        mutate(across(everything(), as.character))
      checkCaseList[[l]] = checkCase
    }
    else if(errorForm$Filter[l] != "ALL") {
    checkCase <- dataForm %>% 
    select(all_of(ID), str_replace_all(errorForm$Filter[l], c(" > .*" = "", " == .*" = "")), 
           errorForm$`Variable Name`[l]) %>% mutate(skipError = NA) %>% 
    mutate(
      skipError = ifelse(eval(parse(text = errorForm$Filter[l])) & 
                       (is.na(eval(parse(text = errorForm$`Variable Name`[l]))) |
                          eval(parse(text = errorForm$`Variable Name`[l])) %in% c(-7, 77, "n/a","07/07/1907", "77:77", -5, 55,"missing", "05/05/1905", "55:55")), 
                     "missing", NA) ###!!! add extra
    ) %>% 
    add_column(Form = errorForm$Form[l],
               ParentVar = sub(" == .*", "", errorForm$Filter[l]), 
               Variable = errorForm$`Variable Name`[l], 
               Filter = errorForm$Filter[l]) %>% 
    rename("ParentVar Value" = sub(" == .*", "", errorForm$Filter[l]),
           "Variable Value" = errorForm$`Variable Name`[l]) %>% 
    select(Form, all_of(ID), Variable, "Variable Value", ParentVar, "ParentVar Value", Filter, skipError) %>% 
    filter(!is.na(skipError)) %>% 
    mutate(across(everything(), as.character))
  checkCaseList[[l]] = checkCase
    }
}
  #save all error case in one data frame
  errorCase <- checkCaseList %>% bind_rows()

 #list outputs
 errorList <- list()
 errorList$errorForm <- errorForm
 errorList$errorCase <- errorCase
 
 return(errorList)
}

#*****************************************************************************
#*skip logic check for each form
#*****************************************************************************
error00  <- LogicCheck(mnh00, filter00)
error04 <- LogicCheck(mnh04, filter04)
errorOverview = bind_rows(
  as.data.frame(error00["errorForm"]),
  as.data.frame(error04["errorForm"])
  ) #add more errorForm here
errorCases = bind_rows(
  as.data.frame(error00["errorCase"]),
  as.data.frame(error04["errorCase"])
  )

#*****************************************************************************
#*export excel for skip logic errors
#*****************************************************************************
setwd(paste0("~/github/Skip-Logic-query-2023/", site, "/", UploadDate))

write.xlsx(as.data.frame(errorOverview), 
           file = paste0(site, " - Skip Logic Error - ", Sys.Date(), ".xlsx"), 
           sheetName = "Error Overview", col.names=TRUE,  row.names = FALSE, append = TRUE, showNA = FALSE)

write.xlsx(as.data.frame(errorCases), 
           file = paste0(site, " - Skip Logic Error - ", Sys.Date(), ".xlsx"), 
           sheetName = "Error Cases", col.names=TRUE,  row.names = FALSE, append = TRUE, showNA = FALSE)



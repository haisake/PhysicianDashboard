# Purpose: To Generate Heat Maps of client volumes for Richmond
# Author: Hans Aisake
# Date Created: May 15, 2019
# Date Modified: see .git
# Comments:
# Use ACE open date as   2018-10-19  or 2019-09

#extra code bits 
#source("./SourceFiles/sourceHeader_v4.R", local = TRUE)

#set working directory
wd <- "//vch.ca/departments/VCHDecisionSupport/Patient Flow/Richmond SSRS Reports/PhysicianDashboard/PhysicianDashboard/queries and scripts"
setwd(wd)

#load in source files
source("./R Source Files/intialization.R", local = TRUE)
source("./R Source Files/dataLoading.R", local = TRUE)
intialize() #intialize local variables and libraries

#set parameters
dsdw_dsn <- "AISAKE-DSSI"
capPlanConfig <- "H:/Hans/DSN Configs/CAPPLAN.yml"
queryFileList <- list.files()[   which( sapply(list.files(), function(x) grep(".sql",x) ) == 1 )  ] #all files with .sql in the folder

ID1_censusLimits <<- data.frame(DoctorService = c("ACE-Hospitalist","ACE-InternalMedicine", "ACE-Other", "Hospitalist","InternalMedicine", "Other"), Targets = c(18,18,0,90,31,0)) #from Clair
ID1_censusLimits_test <<- data.frame(DoctorService = c("ACE-Hospitalist","ACE-InternalMedicine", "ACE-Other", "Hospitalist","InternalMedicine", "Other"), Targets = c(1,1,1,1,1,1)) #for testing


#load in data
#dataList  <- loadData(queryFileList, dsdw_dsn, capPlanConfig) # Pull in datasets\
dataList <- readFromExcel()
dataList2 <- addFieldsAndAggregate( dataList ) #add things like service and aggregate the data when appropriate






# Purpose: To Generate Heat Maps of client volumes for Richmond
# Author: Hans Aisake
# Date Created: May 15, 2019
# Date Modified: see .git
# Comments:

#extra code bits 
#source("./SourceFiles/sourceHeader_v4.R", local = TRUE)

#set working directory
wd <- "//vch.ca/departments/VCHDecisionSupport/Patient Flow/Richmond SSRS Reports/PhysicianDashboard/PhysicianDashboard/queries and scripts"
setwd(wd)

#set parameters
dsdw_dsn <- "AISAKE-DSSI"
capPlanConfig <- "H:/Hans/DSN Configs/CAPPLAN.yml"
queryFileList <- list.files()[   which( sapply(list.files(), function(x) grep(".sql",x) ) == 1 )  ] #all files with .sql in the folder

#load in source files
source("./R Source Files/intialization.R", local = TRUE)
source("./R Source Files/dataLoading.R", local = TRUE)

intialize() #intialize local variables and libraries

dataList <- loadData(queryFileList, dsdw_dsn, capPlanConfig) # Pull in datasets
dataList2 <- dataList #temporary development var

dataList <- addFieldsAndAggregate( dataList )



####

#at this point all the relevant data sets are made and now I need to compute the indicators
#the start and end dates aren't utilized yet and would need to be changed in the join conditions above to be used properly
census_df <- select(census_df,-c(lu_SpecialtyID, DrName, DS_StartDate, DS_EndDate )) 
transfers_df <- select(census_df,-c(lu_SpecialtyID, DrName, DS_StartDate, DS_EndDate )) 



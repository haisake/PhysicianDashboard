# Purpose: To Generate Heat Maps of client volumes for Richmond
# Author: Hans Aisake
# Date Created: May 15, 2019
# Date Modified: May 16, 2019
# Comments:

# Initialization ####
#clear workspace
rm(list=ls())

#set library and working directories
libPath <-"H:/Hans/R Libraries"
.libPaths(libPath) #set the library path
wd <- "//vch.ca/departments/VCHDecisionSupport/Patient Flow/Richmond SSRS Reports/PhysicianDashboard/PhysicianDashboard/queries and scripts"
setwd(wd)

#load libraries
library(DBI) #for database connections
library(odbc) #for database connections
library(dplyr) #for data manipulation

#DSDW DSN
dsdw_dsn <- "AISAKE-DSSI"
#capplan details are more hardcoded

# Data loading functions ####

#get query from a .sql file
#requires saving as UTF-8 without signature.
getSQL <- function(filepath)
{
  con = file(filepath, "r")
  sql.string <- ""
  
  while (TRUE){
    line <- readLines(con, n = 1, encoding="UTF-8")
    
    if ( length(line) == 0 ){
      break
    }
    
    line <- gsub("\\t", " ", line)
    
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    
    sql.string <- paste(sql.string, line)
  }
  
  close(con)
  return(sql.string)
}

#Purpose: load data from CapPlan for a specified query
loadFromCapPlan <-function(queryFileName)
{
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "SPDBSCAP001",
                        Database ="CapPlan_rhs",
                        UID = "CapPlanHC_Reader",
                        PWD    = rstudioapi::askForPassword("Database password (case sensitive):")
  )
  # replace this line if you want to ask for the User ID to be entered as well.
  # UID    = rstudioapi::askForPassword("Database user"),
  
  query <- getSQL(queryFileName)        #get the query string from the external query file
  x <- dbGetQuery(con, query)  
  
  dbDisconnect(con) #close data base connection
  return(x) #return the results
}

#Purpose: load data from CapPlan for a specified query
loadFromDSDW <-function(queryFileName, dsdw_dsn)
{
  con <- DBI::dbConnect(odbc::odbc(),
                        dsn = dsdw_dsn
  )
  # replace this line if you want to ask for the User ID to be entered as well.
  # UID    = rstudioapi::askForPassword("Database user"),
  
  query <- getSQL(queryFileName)        #get the query string from the external query file
  x <- dbGetQuery(con, query)  
  
  dbDisconnect(con) #close data base connection
  return(x) #return the results
}
#####

# Pull in data from the different environments ####
  #test cases for cap plan
  queryFileName <- "transferQuery.sql"
  transfers_df <- loadFromCapPlan(queryFileName)
  
  queryFileName <- "censusQuery.sql"
  census_df <- loadFromCapPlan(queryFileName)
  
  #test cases for DSDW
  queryFileName <- "adtcQuery.sql"
  adtc_df <- loadFromDSDW(queryFileName,dsdw_dsn)
  
  queryFileName <- "reportDateQuery.sql"
  repDate_df <- loadFromDSDW(queryFileName,dsdw_dsn)
  
  queryFileName <- "edQuery.sql"
  ed_df <- loadFromDSDW(queryFileName,dsdw_dsn)
  
  queryFileName <- "dadQuery.sql"
  testData <- loadFromDSDW(queryFileName,dsdw_dsn)
  
  queryFileName <- "doctorServicesQuery.sql"
  service_df <- loadFromDSDW(queryFileName,dsdw_dsn)
#####

names(adtc_df)
names(census_df)
names(ed_df)
names(repDate_df)
names(service_df)
names(transfers_df)
  

#add doctor service categories to data sets and filter to the target services <changed description> ####
  target_services <- c( "Internal Medicine", "Hospitalist" )
  
  census_df <-census_df %>% 
    semi_join(service_df, by="DrCode" )  %>%
    semi_join(repDate_df, by = c("Date" = "ShortDate") ) %>%
    filter(DoctorService %in% target_services )
  
  transfers_df <-transfers_df %>%
    semi_join(service_df, by = c("OrigPhys" = "DrCode") ) %>%
    semi_join(service_df, by = c("TransPhys" = "DrCode") ) %>%
    semi_join(repDate_df, by = c("TransferDate" = "ShortDate") ) %>%
    filter(DoctorService.x %in% target_services | DoctorService.y %in% target_services ) %>%

  adtc_df <- adtc_df %>% 
    semi_join(service_df, by=c("DischargeAttendingDrcode" = "DrCode" ) )  %>%
    semi_join(repDate_df, by = c("Date" = "ShortDate") ) %>%
    filter( DoctorService %in% target_services )
  
  dad_df <- dad_df %>% 
    semi_join(service_df, by="DrCode" )  %>%
    filter( DoctorService %in% target_services )

####

#at this point all the relevant data sets are made and now I need to compute the indicators
#the start and end dates aren't utilized yet and would need to be changed in the join conditions above to be used properly
census_df <- select(census_df,-c(lu_SpecialtyID, DrName, DS_StartDate, DS_EndDate )) 
transfers_df <- select(census_df,-c(lu_SpecialtyID, DrName, DS_StartDate, DS_EndDate )) 



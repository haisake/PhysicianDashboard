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
#targeted services data we want after joining
target_services <- c( "Internal Medicine", "Hospitalist" )

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
                        PWD    = rstudioapi::askForPassword("CapPlan password (case sensitive):")
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
  queryFileName <- "adtcAdmitsQuery.sql"
  adtcAdmits_df <- loadFromDSDW(queryFileName,dsdw_dsn)

  queryFileName <- "adtcDischargesQuery.sql"
  adtcDischarges_df <- loadFromDSDW(queryFileName,dsdw_dsn)
  
  queryFileName <- "adtcReadmitsQuery.sql"
  adtcReadmits_df <- loadFromDSDW(queryFileName,dsdw_dsn)
  
  queryFileName <- "reportDateQuery.sql"
  repDate_df <- loadFromDSDW(queryFileName,dsdw_dsn)
  
  queryFileName <- "edQuery.sql"
  ed_df <- loadFromDSDW(queryFileName,dsdw_dsn)
  
  queryFileName <- "dadQuery.sql"
  testData <- loadFromDSDW(queryFileName,dsdw_dsn)
  
  queryFileName <- "doctorServicesQuery.sql"
  service_df <- loadFromDSDW(queryFileName,dsdw_dsn)
#####


#add doctor service categories to data sets and filter to the target services <changed description> ####
  #  backup df's for dev purposes
  # census_df2 <- census_df
  # adtc_df2 <- adct_df
  # ed_df2 <- ed_df
  # transfers_df2 <- trnasfers_df
  # service_df2 <-service_df

  #deal with data types
  census_df$Date <- as.POSIXct(census_df$Date, format="%Y-%m-%d", tz="UTC") 

  #add service description and fiscal period
  census_df <-census_df %>% 
    left_join(service_df, by="DrCode" )  %>%
    inner_join(repDate_df, by = c("Date" = "ShortDate") )%>% 
    filter( DoctorService %in% target_services )

  #add service description and fiscal period
  transfers_df <-transfers_df %>%
    left_join(service_df, by = c("OrigPhys" = "DrCode") ) %>%
    left_join(service_df, by = c("TransPhys" = "DrCode") ) %>%
    inner_join(repDate_df, by = c("TransferDate" = "ShortDate") ) %>% 
    filter(DoctorService.x %in% target_services | DoctorService.y %in% target_services )
  
  #add service description and fiscal period
  adtcAdmits_df <- adtcAdmits_df %>% 
    left_join(service_df, by= "DrCode" )  %>%
    inner_join(repDate_df, by = c("Date" = "ShortDate") ) %>%  
    filter( DoctorService %in% target_services )
  
  #add service description and fiscal period
  adtcDischarges_df <- adtcDischarges_df %>% 
    left_join(service_df, by= "DrCode" )  %>%
    inner_join(repDate_df, by = c("Date" = "ShortDate") ) %>%  
    filter( DoctorService %in% target_services )
  
  #add service description and fiscal period
  adtcReadmits_df <- adtcReadmits_df %>% 
    left_join(service_df, by= "DrCode" )  %>%
    inner_join(repDate_df, by = c("Date" = "ShortDate") ) %>%  
    filter( DoctorService %in% target_services )
  
  #add service description and fiscal period
  dad_df <- dad_df %>% 
    left_join(service_df, by="DrCode" ) %>% 
    filter( DoctorService %in% target_services )

####

#at this point all the relevant data sets are made and now I need to compute the indicators
#the start and end dates aren't utilized yet and would need to be changed in the join conditions above to be used properly
census_df <- select(census_df,-c(lu_SpecialtyID, DrName, DS_StartDate, DS_EndDate )) 
transfers_df <- select(census_df,-c(lu_SpecialtyID, DrName, DS_StartDate, DS_EndDate )) 



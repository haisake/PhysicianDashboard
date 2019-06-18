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
library(DBI)
library(odbc)

#get query from a .sql file
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
loadFromDSDW <-function(queryFileName)
{
  con <- DBI::dbConnect(odbc::odbc(),
                        dsn = rstudioapi::askForPassword("Database connection name (case sensitive):")
  )
  # replace this line if you want to ask for the User ID to be entered as well.
  # UID    = rstudioapi::askForPassword("Database user"),
  
  query <- getSQL(queryFileName)        #get the query string from the external query file
  x <- dbGetQuery(con, query)  
  
  dbDisconnect(con) #close data base connection
  return(x) #return the results
}

#test cases for cap plan
queryFileName <- "transferQuery.sql"
testData <- loadFromCapPlan(queryFileName)

#test cases for DSDW
queryFileName <- "adtcQuery.sql"
testData <- loadFromDSDW(queryFileName)

queryFileName <- "reportDateQuery.sql"
testData <- loadFromDSDW(queryFileName)

queryFileName <- "edQuery.sql"
testData <- loadFromDSDW(queryFileName)



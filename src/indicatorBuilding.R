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
wd <- "//vch.ca/departments/VCHDecisionSupport/Patient Flow/Richmond SSRS Reports/PhysicianDashboard/PhysicianDashboard/src"
setwd(wd)

#load libraries
library(RODBC)

#get query from a .sql file
getQuery <-function(fileName){
  rawQueryTxt <-readChar(fileName, file.info(fileName)$size)
  finalQueryTxt <-gsub( "[\r\n\t]", " ", rawQueryTxt) #remove spaces, newlines, and tabs
}

#Purpose: load other data sets given a ODBC connection anme and queryfile name
loadCensus <-function(connectionName, queryFileName)
{
  uid <- readline("User id: ")
  pwd <- readline("Password: ")
  channel <- odbcConnect(connectionName, uid, pwd)  #set the connection details
  query <- getQuery(queryFileName)        #get the query string from the external query file
  x <-sqlQuery(channel, query)          #query the database and bring the data in as a table
  
  odbcCloseAll() #close data base connection
  return(x) #return the results
}

#censusQuery
censusData          <- loadDataGeneric("CAPPLAN","censuQuery.sql")
doctorServicesTable <- loadDataGeneric("AISAKE-DSSI","doctorServicesQuery.sql")


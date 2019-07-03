#to house source code for libraries 

intialize <- function(){
  
  #clear workspace
  rm(list=ls())
  setHansPersonalRLibrary()

  #load libraries
  library(DBI) #for database connections
  library(odbc) #for database connections
  library(dplyr) #for data manipulation
  library(config) #for config files for the DSNs
  library(reshape2) # for pivoting data
  library(ggplot2) #for plotting
}

setHansPersonalRLibrary <- function(){
  #set library and working directories
  libPath <-"H:/Hans/R Libraries"
  .libPaths(libPath) #set the library path
}

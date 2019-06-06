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
library(ggplot2)

# !diagnostics off
#to house source code for data loading and tra


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

#Purpose: run queries on CapPlan for a specified query
loadFromCapPlan <-function(queryFileName, capPlanConfig)
{
  dw <- config::get("capplan",file = capPlanConfig) #get dsn details from a config
  #set up connection
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = dw$driver,
                        Server = dw$server,
                        Database =dw$database,
                        UID = dw$uid,
                        PWD    = dw$pwd
  )

  query <- getSQL(queryFileName)        #get the query string from the external query file
  x <- dbGetQuery(con, query)   #run the queyr and get result
  
  dbDisconnect(con) #close data base connection
  return(x) #return the results
}

#Purpose: run queries on the DSDW for a specified query
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

#Purpose: load in data
loadData <- function(queryFileList, dsdw_dsn,capPlanConfig){

  loadedDataList <- list() #result holding variable
  count <- 1 #count the itteration of the loop

  for (ii in queryFileList){ #for each query
    if ( grepl("capplan",ii)  ){ #if query has capplan in the name
      loadedDataList[[count]] <- loadFromCapPlan(ii,capPlanConfig)
      if( "Date" %in% names(loadedDataList[[count]]) ){
        #if date is as a character change it to to a date type
        if ( "character" %in% is(loadedDataList[[count]]$Date) ) {
          loadedDataList[[count]]$Date <- as.POSIXct(loadedDataList[[count]]$Date, format="%Y-%m-%d", tz="UTC")
        }
      }
    } else{
      loadedDataList[[count]] <- loadFromDSDW(ii, dsdw_dsn)
    }
    count <- count + 1 #itterate count
    
  } #end for

  df_names <- sapply(queryFileList, function(x) paste0(gsub("Query.sql","", x),"_df"), USE.NAMES = FALSE ) #derive name for loaded df from query name
  names(loadedDataList) <- df_names  

  return(loadedDataList)  

}

#Purpose: convert the loaded data to their desired state for indicator computation. Also filter out unnecessary data
addFieldsAndAggregate <- function(dataList){
  
  dataList <- transformCensus(dataList)
  dataList <- transformTransfers(dataList)
  dataList <- transformPtVolumes(dataList)
  dataList <- transformAdmits(dataList)
  dataList <- transformDischarges(dataList)
  dataList <- transformReadmits(dataList)
  dataList <- transformDAD(dataList)
  dataList <- transformLLOS(dataList)
  return(dataList)
}

#to transform the transfers data set to what is needed for computations
transformTransfers <- function(dataList){
  
  x <- dataList$capplanTransfer_df #shorter var name
  
  #add service description and fiscal period
  x <- x %>%
    left_join( dataList$doctorServices_df, by = c("OrigPhys" = "DrCode") ) %>%
    left_join( dataList$doctorServices_df, by = c("TransPhys" = "DrCode") ) %>%
    inner_join( dataList$reportDate_df, by = c("TransferDate" = "ShortDate") ) %>% 
    rename(OrigService = DoctorService.x, TransService = DoctorService.y, transFP =fiscalperiodlong, transFY = fiscalyear)
  
  #note who has ace service
  index  <- which(x$Orig_ACE_Flag =="ACE") #ace patients
  index2 <- which(x$Trans_ACE_Flag =="ACE") #ace patients
  x$OrigService[index] <- paste0("ACE-", x$OrigService[index])
  x$TransService[index2] <- paste0("ACE-", x$TransService[index2])
  
  #remove FP's without sufficiently compelte data
  y <- x %>% group_by( transFP,TransferDate ) %>% summarize( c = n(), fpStart = min(fiscalperiodstartdate), fpEnd = max(fiscalperiodenddate) )
  y <- y %>% ungroup()
  y <- y %>% group_by( transFP ) %>% summarize( c =n(), fpStart = min(fpStart), fpEnd = max(fpEnd) )
  y$p_length = as.numeric(y$fpEnd-y$fpStart+1)
  m_l <- min(y$transFP[which(y$c/y$p_length >=0.8)])
  m_m <- max(y$transFP[which(y$c/y$p_length >=0.8)])
  index3 <-  y$transFP[y$transFP >= m_l & y$transFP <= m_m]
  index4 <- which(x$transFP %in% index3)
  x <- x[index4,]

  #aggregate up to service and drop columns and rename
  x <- x %>%
    group_by(transFY, transFP, TransferDoW, TransferHour, OrigService, TransService) %>%
    summarize( NumTransfers = n())
  
  #create an all combos data set to facilitate identification of 0 transfers in combo
  #list of dimensions for the combos
  p = data.frame( transFP = unique( x$transFP), foo=1)
  q = data.frame( TransferDoW = unique( x$TransferDoW), foo=1)
  r = data.frame( TransferHour = unique( x$TransferHour), foo=1)
  s = data.frame( OrigService = as.character(unique( x$OrigService)), foo=1)
  t = data.frame( TransService = as.character(unique( x$TransService)), foo=1)
  placeholder <- p %>% left_join(q, by="foo") %>% left_join(r, by="foo") %>% left_join(s, by="foo") %>% left_join(t, by="foo") %>% select(-foo)

  #need a column to join on so we have to make a unique dummy column for DPLYR
  placeholder$key <-apply( placeholder , 1 , paste , collapse = "ZZ" )
  x$key <- apply( x[, c("transFP","TransferDoW","TransferHour","OrigService","TransService")] , 1 , paste , collapse = "ZZ" )
  
  #combine the all combos palceholder with the observed values
  placeholder <- placeholder %>%  
    left_join( x, by="key", suffix=c("",".y")) %>% 
    select(names(placeholder),"NumTransfers", -"key")
  placeholder$NumTransfers[is.na(placeholder$NumTransfers)] <- 0  #set 0s
  dataList$capplanTransfer_df <- placeholder #put the result into the data list
  
  return(dataList) #return the result
}

#PURPOSE: to transform the census data set to what is needed for computations
transformCensus <- function(dataList){
  
  x<- dataList$capplanCensus_df #shorten the var name

  #add service description and fiscal period
  x <- x %>% 
    left_join( dataList$doctorServices_df, by="DrCode", suffix=c("",".y") )  %>%
    inner_join( dataList$reportDate_df, by = c("Date" = "ShortDate"), suffix=c("",".z") ) %>% 
    select( names(x), "DoctorService","fiscalperiodlong","fiscalperiodenddate" )
  x<- x %>% rename( censusFP = fiscalperiodlong, fpEnd = fiscalperiodenddate) 

  #note who has ace service
  index <- which(x$ACE_Flag =="ACE") #ace patients
  x$DoctorService[index] <- paste0("ACE-", x$DoctorService[index]) #update doctor service with ACE tag
  x$DoctorService[ is.na(x$DoctorService) ] <- "Other"
  
  #aggregate to service
  x <- x %>%
    group_by(Date, censusFP, fpEnd, CensusHour, ALCFlag, DoctorService) %>%
    summarize( Census = sum( Census, na.rm = TRUE) )
  x$key <- paste(x$Date, x$censusFP, x$CensusHour, x$ALCFlag, x$DoctorService, sep="-")
  x <- x %>% ungroup()
  
  #create an all combos data set to facilitate identification of 0 census
  #list of dimensions for the combos
  p = data.frame( unique(  x[, c("Date", "censusFP","fpEnd", "CensusHour")]), foo=1)
  q = data.frame( ALCFlag = unique(x$ALCFlag), foo=1)
  r = data.frame( DoctorService = unique( x$DoctorService ), foo=1)
  placeholder <- p %>% left_join(q, by="foo") %>% left_join(r, by="foo") %>% select(-foo)
  placeholder$key <- paste(placeholder$Date, placeholder$censusFP, placeholder$CensusHour, placeholder$ALCFlag, placeholder$DoctorService, sep="-")
  
  #create the final data set
  placeholder <- placeholder %>% left_join(x,by="key", suffix=c("", "y"))  %>% select(names(placeholder), Census, -key)
  placeholder[is.na(placeholder)]<-0 #set NA to 0
  
  dataList$capplanCensus_df <- placeholder
  return(dataList) #return the result
}

#Purpose: transform the admits data set. Add doctor service and consolidate
transformAdmits <- function(dataList){
  
  x <- dataList$adtcAdmits_df #shorter variable name
  
  #create an all combos data set to facilitate identification of 0 transfers in combo
  #list of dimensions for the combos
  p = data.frame( unique( x[, c("Admit_FP","Admit_FY")] ), foo=1)
  q = data.frame( Admit_DoW = unique( x$Admit_DoW), foo=1)
  r = data.frame( Admit_Hour = unique( x$Admit_Hour), foo=1) #could be 0:23
  s = data.frame( DoctorService =  c("ACE-Hospitalist","Hospitalist","ACE-InternalMedicine","InternalMedicine","Other" ), foo=1)
  placeholder <- p %>% left_join(q, by="foo") %>% left_join(r, by="foo") %>% left_join(s, by="foo") %>% select(-foo)
  
  #actuals
  x <- x %>% left_join( dataList$doctorServices_df, by= "DrCode", suffix=c("",".y") ) %>% select(names(x),"DoctorService")
  index  <- which( x$ACE_Flag =="ACE") #ace patients
  x$DoctorService[index] <- paste0("ACE-", x$DoctorService[index]) #update doctor service with ACE tag
  x <- x %>% group_by(Admit_FP, Admit_FY, Admit_DoW, Admit_Hour, DoctorService) %>% summarize( NumAdmits = sum(NumAdmissions, na.rm = TRUE))
  
  #need a column to join on so we have to make a unique dummy column for DPLYR
  placeholder$key <-apply( placeholder , 1 , paste , collapse = "ZZ" )
  x$key <- apply( x[, c("Admit_FP","Admit_FY", "Admit_DoW","Admit_Hour","DoctorService")] , 1 , paste , collapse = "ZZ" )
  
  #combine the all combos palceholder with the observed values
  placeholder <- placeholder %>%  
    left_join( x, by="key", suffix=c("",".y")) %>% 
    select(names(placeholder),"NumAdmits", -"key")
  placeholder$NumAdmits[is.na(placeholder$NumAdmits)] <- 0  #set 0s
  
  dataList$adtcAdmits_df <- placeholder #put the result into the data list
  return(dataList)
}

#Purpose: transform the discharge data set. Add doctor service and consolidate
transformDischarges <- function(dataList){
  
  x <- dataList$adtcDischarges_df #store data by a shorter name
  
  #create an all combos data set to facilitate identification of 0 transfers in combo
  #list of dimensions for the combos
  p = data.frame( unique( x[, c("Discharge_FP","Discharge_FY")] ), foo=1)
  q = data.frame( Disch_DoW = unique( x$Disch_DoW), foo=1)
  r = data.frame( Disch_Hour = unique( x$Disch_Hour), foo=1) #could be 0:23
  s = data.frame( DoctorService =  c("ACE-Hospitalist","Hospitalist","ACE-InternalMedicine","InternalMedicine","Other" ), foo=1)
  placeholder <- p %>% left_join(q, by="foo") %>% left_join(r, by="foo") %>% left_join(s, by="foo") %>% select(-foo)
  
  #actuals
  x <- x %>% left_join( dataList$doctorServices_df, by= "DrCode", suffix=c("",".y") ) %>% select(names(x),"DoctorService")
  index  <- which( x$ACE_Flag =="ACE" ) #ace patients
  x$DoctorService[index] <- paste0("ACE-", x$DoctorService[index]) #update doctor service with ACE tag
  x <- x %>% group_by(Discharge_FP, Discharge_FY, Disch_DoW, Disch_Hour, DoctorService) %>%
    summarize( NumDisch = sum(NumDischarges, na.rm = TRUE))

  #need a column to join on so we have to make a unique dummy column for DPLYR
  placeholder$key <-apply( placeholder , 1 , paste , collapse = "ZZ" )
  x$key <- apply( x[, c("Discharge_FP","Discharge_FY", "Disch_DoW","Disch_Hour","DoctorService")] , 1 , paste , collapse = "ZZ" )
  
  #combine the all combos palceholder with the observed values
  placeholder <- placeholder %>%  
    left_join( x, by="key", suffix=c("",".y")) %>% 
    select(names(placeholder),"NumDisch", -"key")
  placeholder$NumDisch[is.na(placeholder$NumDisch)] <- 0  #set 0s
  
  dataList$adtcDischarges_df <- placeholder #put the result into the data list
  return(dataList)
}

#Purpose: transform the discharge data set. Add doctor service and consolidate
transformReadmits <- function(dataList){
  
  x <- dataList$adtcReadmits_df #stored to a shorter name
  
  #create an all combos data set to facilitate identification of 0 transfers in combo
  #list of dimensions for the combos
  p = data.frame( unique( x[, c("Disch_FP","Disch_FY")] ), foo=1)
  q = data.frame( DoctorService = unique( dataList$doctorServices_df$DoctorService ), foo=1)
  placeholder <- p %>% left_join(q, by="foo") %>% select(-foo)
  
  #figure out doctor service
  x<- x %>% left_join( dataList$doctorServices_df, by="DrCode") 
  index  <- which( x$ACE_Flag =="ACE" ) #ace patients
  x$DoctorService[index] <- paste0("ACE-", x$DoctorService[index]) #update doctor service with ACE tag
  
  #compute readmits and rates
  x <- x %>% group_by(Disch_FP, Disch_FY, DoctorService) %>%
    summarize( Seven_Day_Readmits = sum( Seven_Day_Readmits, na.rm = TRUE)
               , TwentyEight_Day_Readmits = sum( TwentyEight_Day_Readmits, na.rm = TRUE)
               , TotalDischarges = sum(TotalDischarges, na.rm = TRUE) )
  
  x$Seven_Day_Readmit_Rates <- x$Seven_Day_Readmits / x$TotalDischarges
  x$TwentyEigth_Day_Readmit_Rates <- x$TwentyEight_Day_Readmits / x$TotalDischarges
  
  #need a column to join on so we have to make a unique dummy column for DPLYR
  placeholder$key <-apply( placeholder , 1 , paste , collapse = "ZZ" )
  x$key <- apply( x[, c("Disch_FP","Disch_FY","DoctorService")] , 1 , paste , collapse = "ZZ" )
  
  #combine the all combos palceholder with the observed values
  placeholder <- placeholder %>%  
    left_join( x, by="key", suffix=c("",".y")) %>% 
    select(names(placeholder),"Seven_Day_Readmits","TwentyEight_Day_Readmits", "TotalDischarges"
           , "Seven_Day_Readmit_Rates","TwentyEigth_Day_Readmit_Rates", -"key")
  
  placeholder$Seven_Day_Readmits[is.na(placeholder$Seven_Day_Readmits)]             <- 0  #set 0s
  placeholder$TwentyEight_Day_Readmits[is.na(placeholder$TwentyEight_Day_Readmits)] <- 0  #set 0s
  placeholder$TotalDischarges[is.na(placeholder$TotalDischarges)]              <- 0  #set 0s
  placeholder$Seven_Day_Redamit_Rates[is.na(placeholder$TotalDischarges)]       <- 0  #set 0s
  placeholder$TwentyEigth_Day_Readmit_Rates[is.na(placeholder$TotalDischarges)] <- 0  #set 0s
  
  dataList$adtcReadmits_df <- placeholder #put the result into the data list
  return(dataList)
}

#Purpose: transform the DAD data set. Add doctor service and consolidate. Doesn't need to be done in R but w/e. 
transformDAD <- function(dataList) {
  
  x <- dataList$dad_df #shorter variable name
  
  #add service description and fiscal period
  x <- x %>% 
    left_join(dataList$doctorServices_df, by=c("MostRespProviderCode" = "DrCode" ) )
  index  <- which( x$ACE_Flag =="ACE" ) #ace patients
  x$DoctorService[index] <- paste0("ACE-", x$DoctorService[index]) #update doctor service with ACE tag
  
  #aggregate by service
  x <- x %>%
    group_by(FiscalPeriodLong, FiscalYear, CMG, DoctorService) %>%
    summarize( Sum_ELOS = sum( Sum_ELOS, na.rm = TRUE)
               , Sum_LOS = sum( Sum_LOS, na.rm = TRUE)
               , NumCases = sum(NumCases, na.rm = TRUE) )

  dataList$dad_df <- x
  
  return(dataList) #return result
}

#Purpose: transform the PtVolumes data set. Add doctor service and consolidate. Doesn't need to be done in R but w/e. 
transformPtVolumes <- function(dataList){

  x <- dataList$capplanPtVolumes_df #shorter variable name
  
  #add doctor service
  x <- x %>% 
    left_join( dataList$doctorServices_df, by= "DrCode", suffix=c("",".y") )  %>%
    select(names(x), DoctorService, -DrCode)
  x$DoctorService[is.na(x$DoctorService)] <-"Other" #data quality cleaning set na as other
  
  #note who has ace service
  index <- which(x$ACE_Flag =="ACE") #ace patients
  x$DoctorService[index] <- paste0("ACE-", x$DoctorService[index]) #update doctor service with ACE tag
 
  #actuals; could be wrong if people bounce between units
  x <- x %>%
    group_by(FiscalPeriodLong, DoctorService) %>%
    summarize( NumUniquePTs = sum(NumUniqueEncounters, na.rm = TRUE))
  x <- x %>% ungroup()
  x <- droplevels(x)
  x$key <- apply( x[, c("FiscalPeriodLong", "DoctorService")] , 1 , paste , collapse = "ZZ" )
  
  #create an all combos data set to facilitate identification of 0 volumes
  #list of dimensions for the combos
  p = data.frame( FiscalPeriodLong = unique( x$FiscalPeriodLong ), foo=1)
  q = data.frame( DoctorService = unique( x$DoctorService ), foo=1)
  placeholder <- p %>% left_join(q, by="foo") %>% select(-foo)
  placeholder$key <-apply( placeholder , 1 , paste , collapse = "ZZ" )
  
  #combine the all combos palceholder with the observed values
  placeholder <- placeholder %>%  
    left_join( x, by="key", suffix=c("",".y")) %>% 
    select(names(placeholder),NumUniquePTs, -key)
  placeholder$NumUniquePTs[is.na(placeholder$NumUniquePTs)] <- 0  #set 0s
  
  dataList$capplanPtVolumes_df <- placeholder #put the result into the data list
  return(dataList)

}

#Purpose: transform the PtVolumes data set. Add doctor service and consolidate. Doesn't need to be done in R but w/e.
transformLLOS <- function(dataList){
  
  x <- dataList$adtcLLOS_df #shorter variable name
  
  #add doctor service
  x <- x %>% 
    left_join( dataList$doctorServices_df, by= "DrCode", suffix=c("",".y") )  %>%
    select(names(x), DoctorService, -DrCode)
  x$DoctorService[is.na(x$DoctorService)] <- "Other" #data quality cleaning set na as other
  
  #note who has ACE service
  index <- which(x$ACE_Flag =="ACE") #ace patients
  x$DoctorService[index] <- paste0("ACE-", x$DoctorService[index]) #update doctor service with ACE tag
 
  #aggregate to service
  x <- x %>% group_by(FiscalPeriodLong, DoctorService) %>%
    summarize( NumLLOSPatients = sum(NumLLOSPatients), LLOSDays_Tot = sum(LLOSDays) )
  
  #
  dataList$adtcLLOS_df <- x #put the result into the data list
  return(dataList)

}

#load from Excel
readFromExcel <- function(){
  require(readxl)
  file <- "G:/VCHDecisionSupport/Patient Flow/Richmond SSRS Reports/PhysicianDashboard/PhysicianDashboard/Manaul Data/Data.xlsx"
  queryFileList <- list.files()[   which( sapply(list.files(), function(x) grep(".sql",x) ) == 1 )  ] #all files with .sql in the folder

  dataList <- list()
  for (ii in 1:length(queryFileList)){
    dataList[[ii]] <- read_excel(file, sheet = ii)
  }
  
  n <- paste0(gsub("Query.sql", "", queryFileList),"_df")
  names(dataList) <- n
  
  return(dataList)
}





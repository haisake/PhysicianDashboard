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
  
  for (ii in queryFileList){
    if (ii %in% c("capplanTransferQuery.sql", "capplanCensusQuery.sql") ){
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
addFieldsAndAggregate <- function(dataList, target_Services){
  
  dataList$census_df         <- transformCensus(dataList$census_df) #census_df add service description and fiscal period
  dataList$transfer_df       <- transformTransfers(dataList) #transfers_df add service description and fiscal period
  dataList$adtcAdmits_df     <- transformAdmits(dataList$adtcAdmits_df)  #adctAdmits_df add service description and fiscal period
  dataList$adtcDischarges_df <- transformDischarges(dataList$adtcDischarges_df) #
  dataList$adtcReadmits_df   <- transformReadmits(dataList$adtcReadmits_df) #
  dataList$dad_df            <- transformDAD(dataList$dad_df)
  return(dataList)
}

#to transform the transfers data set to what is needed for computations
transformTransfers <- function(dataList){
  
  #transfers_df
  #add service description and fiscal period
  dataList$transfer_df <- dataList$transfer_df %>%
    left_join( dataList$doctorServices_df, by = c("OrigPhys" = "DrCode") ) %>%
    left_join( dataList$doctorServices_df, by = c("TransPhys" = "DrCode") ) %>%
    inner_join( dataList$reportDate_df, by = c("TransferDate" = "ShortDate") ) %>% 
    rename(OrigService = DoctorService.x, TransService = DoctorService.y, transFP =fiscalperiodlong, transFY = fiscalyear)
  
  #note who ahs ace service
  index  <- which(dataList$transfer_df$OrigUnit =="R4N") #ace patients
  index2 <- which(dataList$transfer_df$TransUnit =="R4N") #ace patients
  index3 <- dataList$transfer_df$OrigService %in% target_services
  dataList$transfer_df$OrigService[!index3] <-"Other"
  index4 <- dataList$transfer_df$TransService %in% target_services
  dataList$transfer_df$TransService[!index4] <-"Other"
  dataList$transfer_df$OrigService[index] <- paste0("ACE-", dataList$transfer_df$OrigService[index])
  dataList$transfer_df$TransService[index] <- paste0("ACE-", dataList$transfer_df$TransService[index])
  
  #aggregate up to service and drop columns and rename
  dataList$transfer_df <- dataList$transfer_df %>%
    group_by(transFY, transFP, TransferDoW, TransferHour, OrigService, TransService) %>%
    summarize( N = n())
  
  #create an all combos data set to facilitate identification of 0 transfers in combo
  #list of dimensions for the combos
  p = data.frame( transFP = unique(dataList$transfer_df$transFP), foo=1)
  q = data.frame( TransferDoW = unique(dataList$transfer_df$TransferDoW), foo=1)
  r = data.frame( TransferHour = unique(dataList$transfer_df$TransferHour), foo=1)
  s = data.frame( OrigService = as.character(unique(dataList$transfer_df$OrigService)), foo=1)
  t = data.frame( TransService = as.character(unique(dataList$transfer_df$TransService)), foo=1)
  placeholder <- p %>% left_join(q, by="foo") %>% left_join(r, by="foo") %>% left_join(s, by="foo") %>% left_join(t, by="foo") %>% select(-foo)

  #need a column to join on so we have to make a unique dummy column for DPLYR
  placeholder$key <-apply( placeholder , 1 , paste , collapse = "ZZ" )
  dataList$transfer_df$key <- apply( dataList$transfer_df[, c("transFP","TransferDoW","TransferHour","OrigService","TransService")] , 1 , paste , collapse = "ZZ" )
  
  #combine the all combos palceholder with the observed values
  placeholder <- placeholder %>%  
    left_join(dataList$transfer_df, by="key", suffix=c("",".y")) %>% 
    select(names(placeholder),"N", -"key")
  placeholder$N[is.na(placeholder$N)] <- 0  #set 0s
  dataList$transfer_df <- placeholder #put the result into the data list
  
  return(dataList) #return the result
}

#PURPOSE: to transform the census data set to what is needed for computations
transformCensus <- function(dataList){
  
  hour <- unique(dataList$CensusHour) #record census hour
  
  #add service description and fiscal period
  dataList$census_df <- dataList$census_df %>% 
    left_join( dataList$doctorServices_df, by="DrCode" )  %>%
    inner_join( dataList$reportDate_df, by = c("Date" = "ShortDate") )%>% 
    rename( censusFP = fiscalperiodlong)
  
  #note who has ace service
  index <- which(dataList$census_df$NursingUnitCode =="R4N") #ace patients
  dataList$census_df$DoctorService[index] <- paste0("ACE-", dataList$census_df$DoctorService[index])
  
  #aggregate up to service
  dataList$census_df <- dataList$census_df %>%
    group_by(Date, censusFP, DoctorService, ALCFlag) %>%
    summarize( census = sum(Census, na.rm = TRUE), )
  dataList$census_df$Defintion <-paste0("Census was computed from CapPlan at ", hour, " of 24hrs")
  
  return(dataList) #return the result
}

#Purpose: transform the admits data set. Add doctor service and consolidate
transformAdmits <- function(dataList){
  
  #create an all combos data set to facilitate identification of 0 transfers in combo
  #list of dimensions for the combos
  p = data.frame( unique( dataList$adtcAdmits_df[, c("Admit_FP","Admit_FY")] ), foo=1)
  q = data.frame( Admit_DoW = unique( dataList$adtcAdmits_df$Admit_DoW), foo=1)
  r = data.frame( Admit_Hour = unique( dataList$adtcAdmits_df$Admit_Hour), foo=1) #could be 0:23
  s = data.frame( DoctorService = unique( dataList$doctorServices_df$DoctorService ), foo=1)
  placeholder <- p %>% left_join(q, by="foo") %>% left_join(r, by="foo") %>% left_join(s, by="foo") %>% select(-foo)
  
  #actuals
  dataList$adtcAdmits_df <- dataList$adtcAdmits_df %>% 
    left_join( dataList$doctorServices_df, by= "DrCode" )  %>%
    group_by(Admit_FP, Admit_FY, Admit_DoW, Admit_Hour, DoctorService) %>%
    summarize( NumAdmits = sum(NumAdmissions, na.rm = TRUE))

  #need a column to join on so we have to make a unique dummy column for DPLYR
  placeholder$key <-apply( placeholder , 1 , paste , collapse = "ZZ" )
  dataList$adtcAdmits_df$key <- apply( dataList$adtcAdmits_df[, c("Admit_FP","Admit_FY", "Admit_DoW","Admit_Hour","DoctorService")] , 1 , paste , collapse = "ZZ" )
  
  #combine the all combos palceholder with the observed values
  placeholder <- placeholder %>%  
    left_join( dataList$adtcAdmits_df, by="key", suffix=c("",".y")) %>% 
    select(names(placeholder),"NumAdmits", -"key")
  placeholder$NumAdmits[is.na(placeholder$NumAdmits)] <- 0  #set 0s
  
  dataList$adtcAdmits_df <- placeholder #put the result into the data list
  return(dataList)
}

#Purpose: transform the discharge data set. Add doctor service and consolidate
transformDischarges <- function(dataList){
  
  #create an all combos data set to facilitate identification of 0 transfers in combo
  #list of dimensions for the combos
  p = data.frame( unique( dataList$adtcDischarges_df[, c("Discharge_FP","Discharge_FY")] ), foo=1)
  q = data.frame( Disch_DoW = unique( dataList$adtcDischarges_df$Disch_DoW), foo=1)
  r = data.frame( Disch_Hour = unique( dataList$adtcDischarges_df$Disch_Hour), foo=1) #could be 0:23
  s = data.frame( DoctorService = unique( dataList$doctorServices_df$DoctorService ), foo=1)
  placeholder <- p %>% left_join(q, by="foo") %>% left_join(r, by="foo") %>% left_join(s, by="foo") %>% select(-foo)
  
  #actuals
  dataList$adtcDischarges_df <- dataList$adtcDischarges_df %>% 
    left_join( dataList$doctorServices_df, by= "DrCode" )  %>%
    group_by(Discharge_FP, Discharge_FY, Disch_DoW, Disch_Hour, DoctorService) %>%
    summarize( NumDisch = sum(NumDischarges, na.rm = TRUE))

  #need a column to join on so we have to make a unique dummy column for DPLYR
  placeholder$key <-apply( placeholder , 1 , paste , collapse = "ZZ" )
  dataList$adtcDischarges_df$key <- apply( dataList$adtcDischarges_df[, c("Discharge_FP","Discharge_FY", "Disch_DoW","Disch_Hour","DoctorService")] , 1 , paste , collapse = "ZZ" )
  
  #combine the all combos palceholder with the observed values
  placeholder <- placeholder %>%  
    left_join( dataList$adtcDischarges_df, by="key", suffix=c("",".y")) %>% 
    select(names(placeholder),"NumDisch", -"key")
  placeholder$NumDisch[is.na(placeholder$NumDisch)] <- 0  #set 0s
  
  dataList$adtcDischarges_df <- placeholder #put the result into the data list
  return(dataList)
}

#Purpose: transform the discharge data set. Add doctor service and consolidate
transformReadmits <- function(dataList){
  
  #create an all combos data set to facilitate identification of 0 transfers in combo
  #list of dimensions for the combos
  p = data.frame( unique( dataList$adtcReadmits_df[, c("Discharge_FP","Discharge_FY")] ), foo=1)
  q = data.frame( DoctorService = unique( dataList$doctorServices_df$DoctorService ), foo=1)
  placeholder <- p %>% left_join(q, by="foo") %>% select(-foo)
  
  #actuals
  dataList$adtcReadmits_df <- dataList$adtcReadmits_df %>% 
    left_join( dataList$doctorServices_df, by=c("DischargeAttendingDrCode" = "DrCode") )  %>%
    group_by(Discharge_FP, Discharge_FY, DoctorService) %>%
    summarize( Seven_Day_Readmits = sum( Seven_Day_Readmits, na.rm = TRUE)
               , TwentyEight_Day_Readmits = sum( TwentyEight_Day_Readmits, na.rm = TRUE)
               , TotalDischarges = sum(TotalDischarges, na.rm = TRUE) )
  
  #need a column to join on so we have to make a unique dummy column for DPLYR
  placeholder$key <-apply( placeholder , 1 , paste , collapse = "ZZ" )
  dataList$adtcReadmits_df$key <- apply( dataList$adtcReadmits_df[, c("Discharge_FP","Discharge_FY","DoctorService")] , 1 , paste , collapse = "ZZ" )
  
  #combine the all combos palceholder with the observed values
  placeholder <- placeholder %>%  
    left_join( dataList$adtcReadmits_df, by="key", suffix=c("",".y")) %>% 
    select(names(placeholder),"Seven_Day_Readmits","TwentyEight_Day_Readmits", "TotalDischarges", -"key")
  
  placeholder$Seven_Day_Readmits[is.na(placeholder$Seven_Day_Readmits)] <- 0  #set 0s
  placeholder$TwentyEight_Day_Readmits[is.na(placeholder$TwentyEight_Day_Readmits)] <- 0  #set 0s
  placeholder$TotalDischarges[is.na(placeholder$TotalDischarges)] <- 0  #set 0s
  
  dataList$adtcReadmits_df <- placeholder #put the result into the data list
  return(dataList)
}

#Purpose: transform the DAD data set. Add doctor service and consolidate. Doesn't need to be done in R but w/e. 
transformDAD <- function(dataList) {
  #add service description and fiscal period
  dataList$dad_df <- dataList$dad_df %>% 
    left_join(dataList$doctorServices_df, by=c("MostRespProviderCode" = "DrCode" ) ) %>%
    group_by(FiscalPeriodLong, FiscalYear, CMG, DoctorService) %>%
    summarize( Sum_ELOS = sum( Sum_ELOS, na.rm = TRUE)
               , Sum_LOS = sum( Sum_LOS, na.rm = TRUE)
               , NumCases = sum(NumCases, na.rm = TRUE) )
  dataList$dad_df$ELOS_ALOS <- dataList$dad_df$Sum_ELOS/dataList$dad_df$Sum_LOS #add ELOS/ALOS by CMG
  
  return(dataList) #return result
}





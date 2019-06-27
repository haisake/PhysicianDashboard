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
    if (ii %in% c("transferQuery.sql", "censusQuery.sql") ){
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
addFieldsAndFilter <- function(dataList, target_Services){
  
  #census_df
    #add service description and fiscal period
    dataList$census_df <- transformCensus(dataList$census_df, target_services)
    
    #note who has ace service
    index <- which(dataList$census_df$NursingUnitCode =="R4N") #ace patients
    dataList$census_df$DoctorService[index] <- paste0("ACE-", dataList$census_df$DoctorService[index])
    
    #aggregate up to service
    dataList$census_df <- dataList$census_df %>%
      group_by(Date, censusFP, DoctorService, ALCFlag) %>%
      summarize( census = sum(Census, na.rm = TRUE))

  #transfers_df
    #add service description and fiscal period
    dataList$transfer_df <- transformTransfers(dataList, target_services)

  #add service description and fiscal period
  dataList$adtcAdmits_df <- dataList$adtcAdmits_df %>% 
    left_join( dataList$doctorServices_df, by= "DrCode" )  %>%
    inner_join( dataList$reportDate_df, by = c("Date" = "ShortDate") ) %>%  
    filter( DoctorService %in% target_services )
  
  #add service description and fiscal period
  dataList$adtcDischarges_df <- dataList$adtcDischarges_df %>% 
    left_join( dataList$doctorServices_df, by= "DrCode" )  %>%
    inner_join( dataList$reportDate_df, by = c("Date" = "ShortDate") ) %>%  
    filter( DoctorService %in% target_services )
  
  #add service description and fiscal period
  dataList$adtcReadmits_df <- dataList$adtcReadmits_df %>% 
    left_join( dataList$doctorServices_df, by= "DrCode" )  %>%
    inner_join( dataList$reportDate_df, by = c("Date" = "ShortDate") ) %>%  
    filter( DoctorService %in% target_services )
  
  #add service description and fiscal period
  dataList$dad_df <- dataList$dad_df %>% 
    left_join(dataList$doctorServices_df, by="DrCode" ) %>% 
    filter( DoctorService %in% target_services )
  
  return(dataList)
}

#to transform the transfers data set to what is needed for computations
transformTransfers <- function(dataList, target_services){
  
  #transfers_df
  #add service description and fiscal period
  dataList$transfer_df <- dataList$transfer_df %>%
    left_join( dataList$doctorServices_df, by = c("OrigPhys" = "DrCode") ) %>%
    left_join( dataList$doctorServices_df, by = c("TransPhys" = "DrCode") ) %>%
    inner_join( dataList$reportDate_df, by = c("TransferDate" = "ShortDate") ) %>% 
    filter(DoctorService.x %in% target_services | DoctorService.y %in% target_services ) %>%
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
transformCensus <- function(dataList, target_services){
  
  #add service description and fiscal period
  dataList$census_df <- dataList$census_df %>% 
    left_join( dataList$doctorServices_df, by="DrCode" )  %>%
    inner_join( dataList$reportDate_df, by = c("Date" = "ShortDate") )%>% 
    filter( DoctorService %in% target_services ) %>%
    rename( censusFP = fiscalperiodlong)
  
  #note who has ace service
  index <- which(dataList$census_df$NursingUnitCode =="R4N") #ace patients
  dataList$census_df$DoctorService[index] <- paste0("ACE-", dataList$census_df$DoctorService[index])
  
  #aggregate up to service
  dataList$census_df <- dataList$census_df %>%
    group_by(Date, censusFP, DoctorService, ALCFlag) %>%
    summarize( census = sum(Census, na.rm = TRUE))
  
  return(dataList) #return the result
}
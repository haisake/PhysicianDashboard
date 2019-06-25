#to house source code for indicatorBuild.R

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
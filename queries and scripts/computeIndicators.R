# !diagnostics off
#to compute the indicators

#Purpose: to compute percentage of days where census <= target or funded level by service
#Returns a single data frame with the rates
compID1 <- function( capplanCensus_df ){
  
  #troubleshooting
  capplanCensus_df <- dataList$capplanCensus_df
  
  #targets for indicators
  #specified targets
  targets <- data.frame( DoctorService = c("ACE-Hospitalist", "ACE-InternalMedicine", "ACE-Other", "Hospitalist", "InternalMedicine", "Other"), targets = c(NA,NA,NA,NA,36,NA))
  
  #identify days over target
  capplanCensus_df <- capplanCensus_df %>% left_join( targets, by = "DoctorService" ) #combine targets to DF
  index <- which(capplanCensus_df$census <= capplanCensus_df$target) #which records are over target
  capplanCensus_df$overTargetFlag <- 0  #0 if under or equal to
  capplanCensus_df$overTargetFlag[index] <-1  #if over
  
  #count number of days over and under and compute the rate
  result_df <- capplanCensus_df %>%
              group_by(censusFP, DoctorService, Definition) %>%
              summarize( daysOver = sum(overTargetFlag), totalDays = n() )
  result_df$rate <- 1.0*result_df$daysOver/result_df$totalDays #compute the rate
  
  return(result_df) #return the result
}

#Purpose: to compute distribution of census by services is typical or atypical
#Returns a plot and a final flag variable for atypical and typical in a list of 2
compID2- function( capplanCensus_df ){
  
  #troubleshooting
  #capplanCensus_df  <- dataList$capplanCensus_df
  
  #find the distribution of census over the disciplines of interest
  capplanCensus_df <- capplanCensus_df %>% filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-Internal Medicine","InternalMedicine" ) ) #remove other services
  capplanCensus_df <- capplanCensus_df %>% group_by(Date, DoctorService) %>% summarize( census = sum(census) ) #add ALC and non ALC together
  capplanCensus_df <- capplanCensus_df %>% ungroup() #remove groupers
  total   <- capplanCensus_df %>% group_by(Date) %>% summarize( totalCensus = sum(census) )
  capplanCensus_df <- capplanCensus_df %>% ungroup() %>% left_join(total, by="Date")
  capplanCensus_df$distCensus <- capplanCensus_df$census/capplanCensus_df$totalCensus

  #compute the distance between the days
  x <- capplanCensus_df %>% select(Date, DoctorService, distCensus)
  x <- dcast(data=x,  formula = Date ~ DoctorService, value.var = "distCensus")
  x <- x[order(x$Date, decreasing = TRUE),] #order so the most recent is row 1
  x[is.na(x)]<-0 #replace NA with 0; it's actually a 0
  d <- suppressWarnings(dist(x, method = "manhattan"))
  d <- as.matrix(d)
  s <- data.frame( dissimilarity = rowSums(d), group=1, CurrentDayValue = rowSums(d)[1] , labels=c("Current Day",rep("History",nrow(d)-1)), color=c("Blue",rep("Black",nrow(d)-1)) )
  msg <- gsub(" UTC","",max(x$Date))
  
  #compute a plot summarizing similarity
  p <- ggplot(s, aes(x=group, y=dissimilarity)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8) + 
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3) +
    geom_hline( aes(yintercept=CurrentDayValue, color=paste0("Red-Current Day-",msg), fill=labels, guide=FALSE ) )+
    labs(color = "") + xlab(" ")  + ylab("Dissimilarity") + ggtitle("Dissimilarity of Census by Service") +
    theme(legend.position="bottom",  axis.ticks.x=element_blank(),axis.text.x=element_blank())

  #typical or atypical flag
  flag <- ( quantile(s$dissimilarity,c(0.15)) <= s$dissimilarity[1] & s$dissimilarity[1] <= quantile(s$dissimilarity,c(0.85)) )
  
  if(flag){
    flag <-"Typical"
  }else{
    flag <-"Atypical"
  }
  
  l <- list(flag,p)
  return(l)
}

#Purpose: to compute distribution of census by TOD is typical or atypical
compID3- function( capplanCensus_df ){
  #troubleshooting
  #capplanCensus_df  <- dataList$capplanCensus_df
  
  #find the distribution of census over the disciplines of interest
  capplanCensus_df <- capplanCensus_df %>% filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-Internal Medicine","InternalMedicine" ) ) #remove other services
  capplanCensus_df <- capplanCensus_df %>% group_by(Date, DoctorService) %>% summarize( census = sum(census) ) #add ALC and non ALC together
  capplanCensus_df <- capplanCensus_df %>% ungroup() #remove groupers
  total   <- capplanCensus_df %>% group_by(Date) %>% summarize( totalCensus = sum(census) )
  capplanCensus_df <- capplanCensus_df %>% ungroup() %>% left_join(total, by="Date")
  capplanCensus_df$distCensus <- capplanCensus_df$census/capplanCensus_df$totalCensus
  
  #compute the distance between the days
  x <- capplanCensus_df %>% select(Date, DoctorService, distCensus)
  x <- dcast(data=x,  formula = Date ~ DoctorService, value.var = "distCensus")
  x <- x[order(x$Date, decreasing = TRUE),] #order so the most recent is row 1
  x[is.na(x)]<-0 #replace NA with 0; it's actually a 0
  d <- suppressWarnings(dist(x, method = "manhattan"))
  d <- as.matrix(d)
  s <- data.frame( dissimilarity = rowSums(d), group=1, CurrentDayValue = rowSums(d)[1] , labels=c("Current Day",rep("History",nrow(d)-1)), color=c("Blue",rep("Black",nrow(d)-1)) )
  msg <- gsub(" UTC","",max(x$Date))
  
  #compute a plot summarizing similarity
  p <- ggplot(s, aes(x=group, y=dissimilarity)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8) + 
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3) +
    geom_hline( aes(yintercept=CurrentDayValue, color=paste0("Red-Current Day-",msg), fill=labels, guide=FALSE ) )+
    labs(color = "") + xlab(" ")  + ylab("Dissimilarity") + ggtitle("Dissimilarity of Census by Service") +
    theme(legend.position="bottom",  axis.ticks.x=element_blank(),axis.text.x=element_blank())
  
  #typical or atypical flag
  flag <- ( quantile(s$dissimilarity,c(0.15)) <= s$dissimilarity[1] & s$dissimilarity[1] <= quantile(s$dissimilarity,c(0.85)) )
  
  if(flag){
    flag <-"Typical"
  }else{
    flag <-"Atypical"
  }
  
  l <- list(flag,p)
  return(l)
}

#Purpose: to compute distribution # of inpatient days by service
compID4- function( capplanCensus_df ){
  
  #troubleshooting
  #capplanCensus_df  <- dataList$capplanCensus_df
  
  capplanCensus_df <- capplanCensus_df %>% filter( !DoctorService %in% c("Other","ACE-Other") ) #remove other services
  capplanCensus_df <- capplanCensus_df %>% group_by(censusFP, DoctorService, Definition) %>% summarize( drCensus = sum(census) )
  totalCensus_df   <- capplanCensus_df %>% group_by(censusFP) %>% summarize( totalCensus = sum(drCensus)) #total census in FP
  capplanCensus_df <- capplanCensus_df %>% left_join(totalCensus_df, by="censusFP")
  capplanCensus_df$distCensus <- capplanCensus_df$drCensus/capplanCensus_df$totalCensus
  
  temp <- capplanCensus_df %>% select(censusFP, DoctorService, distCensus)
  
  
  dcast(data=temp,  formula = censusFP ~ DoctorService)
  

  return(-1)
}

#Purpose: to compute # of ALC days by service
compID5a- function( capplanCensus_df ){
  return(-1)
}

#Purpose: to compute # of ALC rate by service
compID5b- function( capplanCensus_df ){
  return(-1)
}

#Purpose: Inpatient days per patient days by service
compID6- function( capplanCensus_df, capplanPtVolumes_df ){
  return(-1)
}

#Purpose: Inpatient days per patient by service distribution typical or atypical
compID7- function( capplanCensus_df, capplanPtVolumes_df ){
  return(-1)
}

#Purpose: ALOS/ELOS
compID8- function( dad_df ){
  return(-1)
}

#Purpose: ALOS/ELOS top 5 most significant
compID9- function( dad_df ){
  return(-1)
}

#Purpose: Admission rate by service
compID10- function( missing ){
  return(-1)
}

#Purpose: Admission volumes by TOd and Service distribution typical or atypical
compID11- function( adtcAdmits_df ){
  return(-1)
}

#Purpose: Discharges volumes by TOd and Service distribution typical or atypical
compID12- function( adtcDischarges_df ){
  return(-1)
}

#Purpose: ED consults by service
compID13- function( ed_df ){
  return(-1)
}






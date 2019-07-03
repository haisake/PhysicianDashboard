# !diagnostics off
#to compute the indicators

#Purpose: to compute percentage of days where census <= target or funded level by service
#Returns a single data frame with the rates
compID1 <- function( dataList2 ){
  
  #troubleshooting
  x <- dataList2$x
  
  #targets for indicators
  #specified targets
  targets <- data.frame( DoctorService = c("ACE-Hospitalist", "ACE-InternalMedicine", "ACE-Other", "Hospitalist", "InternalMedicine", "Other"), targets = c(NA,NA,NA,NA,36,NA))
  
  #identify days over target
  x <- x %>% left_join( targets, by = "DoctorService" ) #combine targets to DF
  index <- which(x$Census >= x$targets) #which records are over target
  x$overTargetFlag <- 0  #0 if under or equal to
  x$overTargetFlag[index] <-1  #if over
  
  #count number of days over and under and compute the rate
  result_df <- x %>%
              group_by(censusFP, DoctorService) %>%
              summarize( daysOver = sum(overTargetFlag), totalDays = n() )
  result_df$rate <- 1.0*result_df$daysOver/result_df$totalDays #compute the rate
  
  return(result_df) #return the result
}

#Purpose: to compute distribution of census by services is typical or atypical
#Returns a plot and a final flag variable for atypical and typical in a list of 2
compID2- function( dataList2 ){
  
  #troubleshooting
  x  <- dataList2$x

  #find the distribution of census over the disciplines of interest
  x <- x %>% filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-Internal Medicine","InternalMedicine" ) ) #remove other services
  x <- x %>% group_by(Date, DoctorService) %>% summarize( Census = sum(Census) ) #add ALC and non ALC together
  x <- x %>% ungroup() #remove groupers
  total   <- x %>% group_by(Date) %>% summarize( totalCensus = sum(Census) )
  x <- x %>% ungroup() %>% left_join(total, by="Date")
  x$distCensus <- x$Census/x$totalCensus

  #compute the distance between the days
  x <- x %>% select(Date, DoctorService, distCensus)
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
    geom_hline( aes(yintercept=CurrentDayValue, color=paste0("Red-Current Day-",msg) ) )+
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
compID3- function( dataList2 ){
  #removed
}

#Purpose: to compute distribution # of inpatient days by service
compID4- function( dataList2 ){
  
  #troubleshooting
  x <- dataList2$capplanCensus_df
  
  x <- x %>% filter( !DoctorService %in% c("Other","ACE-Other") ) #remove other services
  x <- x %>% group_by(censusFP, DoctorService) %>% summarize( drCensus = sum(Census) )
  totalCensus_df   <- x %>% group_by(censusFP) %>% summarize( totalCensus = sum(drCensus)) #total census in FP
  x <- x %>% left_join(totalCensus_df, by="censusFP")
  x$distCensus <- x$drCensus/x$totalCensus

  #compute the distance between the days
  x <- x %>% select(censusFP, DoctorService, distCensus)
  x <- dcast(data=x,  formula = censusFP ~ DoctorService, value.var = "distCensus")
  x <- x[order(x$censusFP, decreasing = TRUE),] #order so the most recent is row 1
  x[is.na(x)]<-0 #replace NA with 0; it's actually a 0
  d <- suppressWarnings(dist(x, method = "manhattan"))
  d <- as.matrix(d)
  s <- data.frame( dissimilarity = rowSums(d), group=1, CurrentDayValue = rowSums(d)[1] , labels=c("Current Period",rep("History",nrow(d)-1)), color=c("Blue",rep("Black",nrow(d)-1)) )
  msg <- max(x$censusFP)
  
  #compute a plot summarizing similarity
  p <- ggplot(s) + 
    geom_dotplot( aes(x=dissimilarity, fill=color) ) +      
    scale_fill_discrete(name = "Legend", labels = c("History", "CurrentValue")) +
    ggtitle("Placeholder") + xlab("Dissimilarity Score") + ylab("Number of Elements in Bucket") +
    theme(axis.title.y = element_blank() , axis.ticks.y = element_blank(), axis.text.y = element_blank())
  
  #ggplot(s)+
  #  geom_histogram(aes(x=dissimilarity, fill=labels)) +
  #  scale_fill_discrete(name = "Legend", labels = c("Current", "History"))
 
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

#Purpose: to compute # of ALC days by service
compID5a- function( x ){
  return(-1)
}

#Purpose: to compute # of ALC rate by service
compID5b- function( x ){
  return(-1)
}

#Purpose: Inpatient days per patient days by service
compID6- function( x, capplanPtVolumes_df ){
  return(-1)
}

#Purpose: Inpatient days per patient by service distribution typical or atypical
compID7- function( x, capplanPtVolumes_df ){
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






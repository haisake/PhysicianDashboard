# !diagnostics off
#to compute the indicators

#Purpose: compute all indicators
computeIndicators <- function( dataList2){
  
  results <- list()
  results[[1]] <- compID1(dataList2) 
  results[[2]] <- compID2(dataList2) 
  results[[3]] <- compID4a(dataList2) 
  results[[4]] <- compID4b(dataList2) 
  results[[5]] <- compID5a(dataList2) 
  results[[6]] <- compID5b(dataList2) 
  results[[7]] <- compID6(dataList2) 
  results[[8]] <- NA #compID7(dataList2) 
  results[[9]] <- compID8(dataList2) 
  results[[10]] <- compID9(dataList2) 
  results[[11]] <- NA #compID10(dataList2) 
  results[[12]] <- compID11a(dataList2) 
  results[[13]] <- compID11b(dataList2) 
  results[[14]] <- compID12a(dataList2) 
  results[[15]] <- compID12b(dataList2) 
  results[[16]] <- compID13(dataList2) 
  results[[17]] <- compID14(dataList2) 
  results[[18]] <- NA #compID15(dataList2) 
  results[[19]] <- compID16a(dataList2)
  results[[20]] <- compID16b(dataList2) 
  results[[21]] <- compID17a(dataList2) 
  results[[21]] <- compID17b(dataList2) 
  
  names(results) <- c("ID1","ID2","ID4a","ID4b", "ID5a","ID5b","ID6","ID7","ID8", "ID9", "ID10", "ID11a","ID11b","ID12a","ID12b","ID13", "ID14","ID15","ID16","ID17")
  
  return(results)
  
}


#Purpose: to compute percentage of days where census <= target or funded level by service
#Returns a single data frame with the rates
compID1 <- function( dataList2 ){
  
  #troubleshooting
  x <- dataList2$capplanCensus_df

  #targets for indicators
  #specified targets
  targets <- ID1_censusLimits #official
  #targets <- ID1_censusLimits_test #testing
 
  #identify days over target
  x <- x %>% inner_join( targets, by = "DoctorService")  #combine targets to DF
  index <- which(x$Census >= x$Targets) #which records are over target
  x$overTargetFlag <- 0  #0 if under or equal to
  x$overTargetFlag[index] <-1  #if over
  
  #count number of days over and under and compute the rate
  y <- x %>%
    group_by(Date, DoctorService) %>%
    summarize( totCensus = sum(Census) )
  
  #count number of days over and under and compute the rate
  result_df <- x %>%
              group_by(censusFP, DoctorService) %>%
              summarize( daysOver = sum(overTargetFlag), totalDays = n() )
  result_df$rate <- 1.0*result_df$daysOver/result_df$totalDays #compute the rate
  
  #plot like the ED P4P
  p <- ggplot( result_df, aes(x=censusFP, y=rate, group=1)) +
    facet_wrap( ~ DoctorService, ncol=2) +
    geom_line(aes(colour=DoctorService)) +
    xlab("FiscalPeriod")  + 
    ylab("Rate%") + 
    ggtitle("Days Census <= Target Level") +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
    scale_y_continuous(labels = scales::percent, limits = c(0,0.5))
  
  #box plot
  p2<- ggplot(y, aes( y=totCensus, fill=DoctorService)) +
    geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=FALSE) +
    ggtitle("Days Census <= Target Level") +
    theme(axis.text.x = element_blank())
  
  #cumulative denisty plot
  p3 <- ggplot(y, aes(totCensus, colour=DoctorService)) + stat_ecdf(geom = "step")
   
  
  l <-list(result_df,p,p2,p3)
  names(l) <- c("Data","Plot","Boxpolt","CumulativePlot")
  
  return(l) #return the result
}

#Purpose: to compute distribution of census by services is typical or atypical
#Returns a plot and a final flag variable for atypical and typical in a list of 2
compID2 <- function( dataList2 ){
  
  #troubleshooting
  x  <- dataList2$capplanCensus_df

  #find the distribution of census over the disciplines of interest
  x <- x %>% filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-Internal Medicine","InternalMedicine" ) ) #remove other services
  x <- x %>% group_by(Date, DoctorService) %>% summarize( Census = sum(Census) ) #add ALC and non ALC together
  x <- x %>% ungroup() #remove groupers
  total   <- x %>% group_by(Date) %>% summarize( totalCensus = sum(Census) )
  x <- x %>% ungroup() %>% left_join(total, by="Date")
  x$distCensus <- x$Census/x$totalCensus

  #compute the distance between the days
  x <- x %>% select(Date, DoctorService, distCensus)
  x <- dcast(data=x,  formula = Date ~ DoctorService, value.var = "distCensus", sum)
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

#Purpose: to compute distribution of census by TOD is typical or atypical; removed takes too much data and effort to extract
#compID3- function( dataList2 ){
  #removed
#}

#Purpose: to compute distribution # of inpatient days by service
compID4a <- function( dataList2 ){
  
  #troubleshooting
  x <- dataList2$capplanCensus_df
  
  x <- x %>% filter( !DoctorService %in% c("Other","ACE-Other") ) #remove other services
  x <- x %>% group_by(censusFP, DoctorService) %>% summarize( drCensus = sum(Census) )
  x <- x %>% ungroup() #ungroup
  totalCensus_df   <- x %>% group_by(censusFP) %>% summarize( totalCensus = sum(drCensus)) #total census in FP
  totalCensus_df <- totalCensus_df %>% ungroup()
  x <- x %>% left_join(totalCensus_df, by="censusFP")
  x$distCensus <- x$drCensus/x$totalCensus
  
  #create a plot
  #q <- x %>% select("censusFP", "DoctorService", "drCensus")
  p <- ggplot(data=x, aes(x=censusFP, y=drCensus, fill=DoctorService)) + 
    geom_bar(stat="identity") +
    xlab("FiscalPeriod")  + 
    ylab("Total Inpatient Days") + 
    ggtitle("RHS - Inpatient Days by Service - Hosp & IM") +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5))

  l <- list(x, p) #list with return values
  names(l) <- c("data","plot")
  
  return(l)
}

#Purpose: to compute distribution # of inpatient days by service typical or atypical
compID4b <- function( dataList2 ){
  
  #troubleshooting
  x <- dataList2$capplanCensus_df
  
  x <- x %>% filter( !DoctorService %in% c("Other","ACE-Other") ) #remove other services
  x <- x %>% group_by(censusFP, DoctorService) %>% summarize( drCensus = sum(Census) )
  x <- x %>% ungroup() #ungroup
  totalCensus_df   <- x %>% group_by(censusFP) %>% summarize( totalCensus = sum(drCensus)) #total census in FP
  totalCensus_df <- totalCensus_df %>% ungroup()
  x <- x %>% left_join(totalCensus_df, by="censusFP")
  x$distCensus <- x$drCensus/x$totalCensus
  
  #compute the distance between the days
  x <- x %>% select(censusFP, DoctorService, distCensus)
  x <- dcast(data=x,  formula = censusFP ~ DoctorService, value.var = "distCensus", sum)
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
    ggtitle("Placeholder - Inpatient Days dist") + 
    xlab("Dissimilarity Score (low = similar, high= different)") +
    ylab("Number of Elements in Bucket") +
    theme(axis.title.y = element_blank() , axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    scale_y_continuous(limits = c(0,0.25))
    
    annotate(""
             
              +
               
  
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
compID5a <- function( dataList2 ){
  
  #troubleshooting
  x <- dataList2$capplanCensus_df
  
  #compute the ALC days by service per fiscal period
  x <- x %>% filter( !DoctorService %in% c("Other","ACE-Other")  ) #remove other services
  x <- x %>% filter( ALCFlag =="ALC" &  Date == fpEnd)
  #x <- x %>% group_by(censusFP, DoctorService) %>% summarize( alcDays = sum(Census) )

  #clean up the data object structures
  x <- x %>% ungroup() #remove groups
  x <- droplevels(x)   #update levels

  #ggplot
  p <- ggplot(x, aes(x=censusFP, y=Census, group=DoctorService, color=DoctorService)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("RHS - ALC Days FP End Snapshot by Service")
  
  l <- list(x,p)
  return(l)
}

#Purpose: to compute # of ALC rate by service
compID5b <- function( dataList2 ){
  
  #troubleshooting
  x <- dataList2$capplanCensus_df
  
  #compute the ALC days by service per fiscal period
  x <- x %>% filter( !DoctorService %in% c("Other","ACE-Other")  ) #remove other services
  y <- x %>% filter( ALCFlag =="ALC" )
  y <- y %>% group_by(censusFP, DoctorService) %>% summarize( alcDays = sum(Census) )
  x <- x %>% group_by(censusFP, DoctorService) %>% summarize( totalDays = sum(Census) )
  
  #clean up the data object structures
  x <- x %>% ungroup() #remove groups
  y <- y %>% ungroup() #remove groups
  x <- droplevels(x)   #update levels
  y <- droplevels(y)   #update levels
  
  #join the data and compute the ALC rate
  x$key <- paste(x$censusFP, x$DoctorService, sep="-") #add a key for joining
  y$key <- paste(y$censusFP, y$DoctorService, sep="-") #add a key for joining
  y <- y %>% left_join(x, by="key", suffix=c("",".q") ) %>% select(names(y), totalDays ,-key)  #join the data
  y$alcRate <- y$alcDays/y$totalDays #ALC Rate
  
  #ggplot
  p <- ggplot(y, aes(x=censusFP, y=alcRate, group=DoctorService, color=DoctorService)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("ALC Rate by Service")  +
    scale_y_continuous(labels = scales::percent, limits = c(0,0.5))
  
  l <- list(y,p)
  return(l)
}

#Purpose: Inpatient days per patient by service
compID6 <- function( dataList2 ){
  
  x <- dataList2$capplanPtVolumes_df #shorter variable
  y <- dataList2$capplanCensus_df    #shorter variable
  
  #find the distribution of census over the disciplines of interest
  x <- x %>% filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-InternalMedicine","InternalMedicine" ) )  %>% rename(censusFP=FiscalPeriodLong)#remove other services
  x <- droplevels(x)
  x$key <-paste( x$censusFP, x$DoctorService, sep="-")

  y <- y %>% 
    filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-InternalMedicine","InternalMedicine" ) ) %>%
    group_by(censusFP,DoctorService) %>%
    summarize( ipDays = sum(Census))
  y <- y %>% ungroup()
  y <- droplevels(y)
  y$key <-paste( y$censusFP, y$DoctorService, sep="-")
  
  #aggregate inpatient days per period by service
  y <- y %>% left_join(x, by="key", suffix=c("",".x")) %>% select(names(y),NumUniquePTs, -key)
  y$alos <- y$ipDays/y$NumUniquePTs
  
  #compute a plot summarizing similarity
  p <- ggplot(y, aes(x=censusFP, y=alos, color=DoctorService, group=DoctorService)) +
    geom_line() +
    xlab("Fiscal Period")  + ylab("ALOS* (days)") + 
    ggtitle("Inpatient days per patient by Service") +
    theme(legend.position="bottom", axis.text.x = element_text(angle = 90, hjust = 1)) 
  
  #IP days per case box plot
  
  l <- list(y,p) #the internal medicine ace numbers aren't stable enough
  return(l)
}

# #Purpose: Inpatient days per patient by service distribution typical or atypical;
# compID7 <- function( dataList2 ){
#   
#   x <- dataList2$capplanPtVolumes_df #shorter variable
#   y <- dataList2$capplanCensus_df    #shorter variable
#   
#   #find the distribution of census over the disciplines of interest
#   x <- x %>% filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-InternalMedicine","InternalMedicine" ) )  %>% rename(censusFP=FiscalPeriodLong)#remove other services
#   x <- droplevels(x)
#   x$key <-paste( x$censusFP, x$DoctorService, sep="-")
#   
#   y <- y %>% 
#     filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-InternalMedicine","InternalMedicine" ) ) %>%
#     group_by(censusFP,DoctorService) %>%
#     summarize( ipDays = sum(Census))
#   y <- y %>% ungroup()
#   y <- droplevels(y)
#   y$key <-paste( y$censusFP, y$DoctorService, sep="-")
#   
#   #aggregate inpatient days per period by service
#   y <- y %>% left_join(x, by="key", suffix=c("",".x")) %>% select(names(y),NumUniquePTs, -key)
#   y$alos <- y$ipDays/y$NumUniquePTs
# 
#   #compute the distance between the days
#   x <- x %>% select(Date, DoctorService, distCensus)
#   x <- dcast(data=x,  formula = Date ~ DoctorService, value.var = "distCensus", sum)
#   x <- x[order(x$Date, decreasing = TRUE),] #order so the most recent is row 1
#   x[is.na(x)]<-0 #replace NA with 0; it's actually a 0
#   d <- suppressWarnings(dist(x, method = "manhattan"))
#   d <- as.matrix(d)
#   s <- data.frame( dissimilarity = rowSums(d), group=1, CurrentDayValue = rowSums(d)[1] , labels=c("Current Day",rep("History",nrow(d)-1)), color=c("Blue",rep("Black",nrow(d)-1)) )
#   msg <- gsub(" UTC","",max(x$Date))
#   
#   #compute a plot summarizing similarity
#   p <- ggplot(y, aes(x=factor(DoctorService), y=alos)) +
#     geom_boxplot() +
#     geom_point( z, mapping = aes(x=factor(DoctorService), y=alos), color="darkorchid1", size=4, shape=18 )    
#   
#   #same y axis
#   p <- ggplot(y, aes(x =factor(1), group=factor(DoctorService), y=alos)) +
#     facet_grid( col= vars(DoctorService), scales = "free_y") +
#     geom_boxplot() +
#     geom_point( mapping = aes(x=factor(1), y=alos, color=latestFlag), size=2, shape=18 ) +
#     scale_color_discrete(name = "Legend", labels = c("Historical", "Current Period")) +
#     theme( legend.position = "bottom")
#   
#   #y axis varies
#   p <- ggplot(y, aes(x =factor(1), group=factor(DoctorService), y=alos)) +
#     facet_wrap( ~DoctorService, scales = "free_y") +
#     geom_boxplot() +
#     geom_point( mapping = aes(x=factor(1), y=alos, color=latestFlag), size=2, shape=18 ) +
#     scale_color_discrete(name = "Legend", labels = c("Historical", "Current Period")) +
#     theme( legend.position = "bottom")                         
#                             
#                             
#                             geom_point(z, aes(x=1, y=alos), color="blue") +
#     xlab("")  + ylab("ALOS* (days)") + 
#     ggtitle("Inpatient days per patient by Service Boxplots") +
#     theme(legend.position="bottom", axis.text.x = element_blank())
#     
#     
#   
#     
#   
#   #typical or atypical flag
#   for(ii unique(y$DoctorService)){
#     quantile(y$alos,c(0.15)) <= y$alos[1] & y$alos[1] <= quantile(y$alos,c(0.85))
#   }
#   
#   flag <- ( quantile(y$alos,c(0.15)) <= s$dissimilarity[1] & s$dissimilarity[1] <= quantile(s$dissimilarity,c(0.85)) )
#   
#   if(flag){
#     flag <-"Typical"
#   }else{
#     flag <-"Atypical"
#   }
#   
#   l <- list(y,p) #the internal medicine ace numbers aren't stable enough
#   return(l)
#   
# 
# }

#Purpose: ALOS/ELOS
compID8 <- function( dataList2 ){
  
  #data
  x <- dataList2$dad_df
  x <- x %>% filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-InternalMedicine","InternalMedicine" ) ) %>%
    group_by(FiscalPeriodLong, DoctorService) %>%
    summarize( LOS = sum(Sum_LOS), ELOS = sum(Sum_ELOS), N=n() )
  x <- x %>% ungroup()
  x$LOS_ELOS <- x$LOS/x$ELOS
  
  #plot by service
  p <- ggplot(x, aes(x=FiscalPeriodLong, y=LOS_ELOS, group=DoctorService, colour=DoctorService) ) + 
    geom_line( ) + 
    geom_hline( yintercept=1, color="Black") +
    ggtitle("ALOS/ELOS for RHS") +
    theme(legend.position="bottom", axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("ALOS/ELOS")  + xlab("Discharge Fiscal Period")
  
  #return list
  l <- list(x,p)
  names(l) <-c("Data","Plot")
 
   # scale_y_continuous(labels = scales::percent, limits = c(0,0.5))
  
  return(l)
}

#Purpose: ALOS/ELOS top 5 most significant CMGs
compID9 <- function( dataList2 ){
  
  x <- dataList2$dad_df
  x <- x %>% filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-InternalMedicine","InternalMedicine" ) ) %>%
    group_by(CMG) %>%
    summarize( LOS = sum(Sum_LOS), ELOS = sum(Sum_ELOS), N=n() )
  x <- x %>% ungroup()
  x$LOS_ELOS <- x$LOS/x$ELOS
  
  #based on computations I made in 2016 ~ Control Limit Methodologyv5b
  #G:\Projects (Dept VC)\Patient Flow Project\VCHSummary\ALOS vs ELOS national study
  #sigma squared was computed as 0.45 see Master Data Extract FileV3 - 2013 version.
  #it has to use all the DAD, not just what we pulled here.
  sigmaSQ <- 0.45
  z95 <-qnorm(0.05)
  z99 <-qnorm(0.01)
  num =(exp(sigmaSQ)-1)
  
  fit95 <- data.frame(lwr95 = 1:max(x$N), upr95 = 1:max(x$N), N= 1:max(x$N))
  fit95$lwr95 <- 1-z95*sqrt(num/fit95$lwr95)
  fit95$upr95 <- 1+z95*sqrt(num/fit95$upr95)
  
  fit99 <- data.frame(lwr99 = 1:max(x$N), upr99 = 1:max(x$N), N= 1:max(x$N))
  fit99$lwr99 <- 1-z99*sqrt(num/fit99$lwr99)
  fit99$upr99 <- 1+z99*sqrt(num/fit99$upr99)
  
  x <- x %>% left_join( fit95, by = "N", suffix=c("","T") ) %>%
    left_join( fit99, by = "N", suffix=c("","G") ) 
  
  #find the 5 significant points; any point above 99 upr
  index <- which(x$LOS_ELOS >= x$upr99)
  sigBad <- x[index,]
  sigBad <- sigBad[order(sigBad$N, decreasing = TRUE),]
  sigBad <- sigBad[1:5,]
  
  #find the 5 best success points
  index <- which(x$LOS_ELOS <= x$lwr99)
  sigGood <- x[index,]
  sigGood <- sigGood[order(sigGood$N, decreasing = TRUE),]
  sigGood <- sigGood[1:5,]
  
  #create a funnel plot
  p <- ggplot(x, aes(x=N, y=LOS_ELOS)) +
    geom_point(shape=1) + 
    geom_hline(yintercept=1) +
    geom_line(aes(y = upr95), color="black", linetype=2) + 
    geom_line(aes(x=N, y = lwr95), color="black", linetype=2) +
    geom_line(aes(x=N, y = upr99), color="red", linetype=3) + 
    geom_line(aes(x=N, y = lwr99), color="red", linetype=3)  + 
    annotate("text", 15, 3.2, label="95% limit", colour="black", 
             size=3, hjust=0) +
    annotate("text", 15, 3.5, label="99.9% limit", colour="red", 
             size=3, hjust=0) +
    labs(x="No. Discharges", y="ALOS/ELOS", title="RHS Funnel Plot ALOS/ELOS") +    
    theme_bw()
  
  #results
  l <-list(sigBad, sigGood, p)
  names(l) <- c("sigBad","sigGood","FunnelPlot")
 
  return(l)
}

#Purpose: Admission rate by service; not possible we don't have denominators
# compID10 <- function( dataList2 ){
#   return(-1)
# }

#Purpose: Admission volumes by TOD and service
compID11a <- function( dataList2 ){
  
  x <-dataList2$adtcAdmits_df #shorter variable name
  x <- x %>% filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-InternalMedicine","InternalMedicine" ) )
  
  return(x)
}

#Purpose: Admission volumes by TOD typical or atypical current period vs. historical
#Comments: ACE-Hospitalist, and ACE -Internal Medicine don't get any admits. Is that true?
compID11b <- function( dataList2 ){
  
  x <-dataList2$adtcAdmits_df #shorter variable name
  x <- x %>% filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-InternalMedicine","InternalMedicine" ) )
  
  #unique services
  temp <-unique(x$DoctorService)
  l <- list() #result holder

  #for each service find out if the pattern is typical
  for (ii in 1:length(temp) ) {
    y <- x %>% filter( DoctorService ==temp[ii] ) #filter to service
    
    #compute the distance between the days
    y <- y %>% select(Admit_FP, Admit_DoW, Admit_Hour, NumAdmits)
    y <- dcast(data=y,  formula = Admit_FP ~ Admit_DoW + Admit_Hour, value.var = "NumAdmits", sum)
    y <- y[order(y$Admit_FP, decreasing = TRUE),] #order so the most recent is row 1
    y[is.na(y)]<-0 #replace NA with 0; it's actually a 0
    d <- suppressWarnings(dist(y, method = "manhattan"))
    d <- as.matrix(d)
    s <- data.frame( dissimilarity = rowSums(d), group=1, CurrentDayValue = rowSums(d)[1] , labels=c("Current Period",rep("History",nrow(d)-1)), color=c("Blue",rep("Black",nrow(d)-1)) )
    msg <- max(y$Admit_FP)
    
    #compute a plot summarizing similarity
    p <- ggplot(s, aes(x=group, y=dissimilarity)) + 
      geom_boxplot(outlier.colour="red", outlier.shape=8) + 
      geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3) +
      geom_hline( aes(yintercept=CurrentDayValue, color=paste0("Red-Current Day-",msg) ) ) +
      labs(color = "") + xlab(" ")  + ylab("Dissimilarity") + ggtitle(paste0("Dissimilarity of Admits by DoW, ToD Service ",temp[ii]) ) +
      theme(legend.position="bottom",  axis.ticks.x=element_blank(),axis.text.x=element_blank())
    
    #typical or atypical flag
    flag <- ( quantile(s$dissimilarity,c(0.15)) <= s$dissimilarity[1] & s$dissimilarity[1] <= quantile(s$dissimilarity,c(0.85)) )
    
    if(flag){
      flag <-"Typical"
    }else{
      flag <-"Atypical"
    }
    
    k <- list(flag,p)
    l[[ii]] <- k
  }
 
  return(l)
}

#Purpose: Discharges volumes by TOd and Service distribution
compID12a <- function( dataList2 ){
  
  x <- dataList2$adtcDischarge_df #shorter variable name
  x <- x %>% filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-InternalMedicine","InternalMedicine" ) )

  return(x)
}

#Purpose: Admission volumes by TOD typical or atypical current period vs. historical
#Comments: ACE-Hospitalist, and ACE -Internal Medicine don't get any admits. Is that true?
compID12b <- function( dataList2 ){
  
  x <- dataList2$adtcDischarges_df #shorter variable name
  x <- x %>% filter( DoctorService %in% c("ACE-Hospitalist","Hospitalist","ACE-InternalMedicine","InternalMedicine" ) )
  
  #unique services
  temp <-unique(x$DoctorService)
  l <- list() #result holder
  
  #for each service find out if the pattern is typical
  for (ii in 1:length(temp) ) {
    y <- x %>% filter( DoctorService ==temp[ii] ) #filter to service
    
    #compute the distance between the days
    y <- y %>% select(Discharge_FP, Disch_DoW, Disch_Hour, NumDisch)
    y <- dcast(data=y,  formula = Discharge_FP ~ Disch_DoW + Disch_Hour, value.var = "NumDisch", sum)
    y <- y[order(y$Admit_FP, decreasing = TRUE),] #order so the most recent is row 1
    y[is.na(y)]<-0 #replace NA with 0; it's actually a 0
    d <- suppressWarnings(dist(y, method = "manhattan"))
    d <- as.matrix(d)
    s <- data.frame( dissimilarity = rowSums(d), group=1, CurrentDayValue = rowSums(d)[1] , labels=c("Current Period",rep("History",nrow(d)-1)), color=c("Blue",rep("Black",nrow(d)-1)) )
    msg <- max(y$Admit_FP)
    
    #compute a plot summarizing similarity
    p <- ggplot(s, aes(x=group, y=dissimilarity)) + 
      geom_boxplot(outlier.colour="red", outlier.shape=8) + 
      geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3) +
      geom_hline( aes(yintercept=CurrentDayValue, color=paste0("Red-Current Day-",msg) ) ) +
      labs(color = "") + xlab(" ")  + ylab("Dissimilarity") + ggtitle(paste0("Dissimilarity of Admits by DoW, ToD Service ",temp[ii]) ) +
      theme(legend.position="bottom",  axis.ticks.x=element_blank(),axis.text.x=element_blank())
    
    #typical or atypical flag
    flag <- ( quantile(s$dissimilarity,c(0.15)) <= s$dissimilarity[1] & s$dissimilarity[1] <= quantile(s$dissimilarity,c(0.85)) )
    
    if(flag){
      flag <-"Typical"
    }else{
      flag <-"Atypical"
    }
    
    k <- list(flag,p)
    l[[ii]] <- k
  }
  
  return(l)
}

#Purpose: ED consults by service
compID13 <- function( dataList2 ){
  
  x <- dataList2$edConsults_df
  
  return(x)
}

#Purpose: ED consults by service typical or atypical
# compID13 <- function( dataList2 ){
#   
#   x <- dataList2$edConsults_df
#   
#   return(x)
# }

#Purpose: LLOS Snapshot
compID14 <- function( dataList2 ){
  
  x <- dataList2$adtcLLOS_df

  return(x)
}

#Purpose: LLOS ALC days snapshot; removed pending futher consideration
# compID15 <- function( dataList2 ){}

#Purpose: Service transfer volumes
compID16a <- function( dataList2 ){
  
  x <- dataList2$capplanTransfer_df #shorter variable dname
  
  #aggregate to number of transfers between service by FP
  x$t_type <- paste(x$OrigService, x$TransService, sep="-")  #assign category of event
  x <- x %>% group_by(transFP, t_type) %>% summarize( Transfers = sum(NumTransfers) )
  x <- x %>% ungroup()
  
  return(x)
}

#Purpose: Service transfer volumes and typical or atypical
compID16b <- function( dataList2 ){
  
  x <- dataList2$capplanTransfer_df #shorter variable dname
  
  #aggregate to number of transfers between service by FP
  x$t_type <- paste(x$OrigService, x$TransService, sep="-")  #assign category of event
  x <- x %>% group_by(transFP, t_type) %>% summarize( Transfers = sum(NumTransfers) )
  x <- x %>% ungroup()
  
  #compute the distance between the days
  x <- dcast(data=x,  formula = transFP ~ t_type, value.var = "Transfers", sum)
  x <- x[order(x$transFP, decreasing = TRUE),]    #order so the most recent is row 1
  x[is.na(x)]<-0  #replace NA with 0; it's actually a 0
  d <- suppressWarnings(dist(x, method = "manhattan"))
  d <- as.matrix(d)
  s <- data.frame( dissimilarity = rowSums(d), group=1, CurrentDayValue = rowSums(d)[1] , labels=c("Current Day",rep("History",nrow(d)-1)), color=c("Blue",rep("Black",nrow(d)-1)), fp = unique(as.character(x$transFP)) )
  msg <- max(as.character(x$transFP))
  
  #compute a plot summarizing similarity
  p <- ggplot(s, aes(x=group, y=dissimilarity)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8) + 
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3) +
    geom_hline( aes(yintercept=CurrentDayValue, color=paste0("Red-Current Day-",msg) ) )+
    labs(color = "") + xlab(" ")  + ylab("Dissimilarity") + ggtitle("Dissimilarity of Transfers Patterns") +
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

#Purpose: 7 readmission rates
compID17a <- function( dataList2 ){
  
  x <- dataList2$adtcReadmits_df %>% select(Disch_FP, Disch_FY, DoctorService, Seven_Day_Readmits, TotalDischarges, Seven_Day_Readmit_Rates) #shorter variable name
  return(x)
}

#Purpose: 28 day readmission rates
compID17b <- function( dataList2 ){
  
  x <- dataList2$adtcReadmits_df %>% select(Disch_FP, Disch_FY, DoctorService, TwentyEight_Day_Readmits, TotalDischarges, TwentyEight_Day_Readmit_Rates) #shorter variable name
  return(x)
}







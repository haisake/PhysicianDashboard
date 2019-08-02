#ED consults analysis

#Purpose: ED consults by service
compID13 <- function( dataList2 ){
  
  x <- dataList2$edConsults_df
  x$Consult_DoW <- as.factor(x$Consult_DoW)
  x$Consult_Hour <- as.factor(x$Consult_Hour)
  x$ConsultationServiceDesc <- as.factor(x$ConsultationServiceDesc)
  
  
  
  #split data into training and test
  # ii <- 1:nrow(x)
  # ind <- sample(ii, floor(nrow(x)*0.75), replace =FALSE)
  # train  <- x[ind,]
  # test <- x[-ind,]
  # #MSE 1.799 training, 1.834 test. Close enough.
  # 
  # #
  # m1 <- lm(data = train, NumConsults ~ ConsultationServiceDesc + Consult_Hour + Consult_DoW)
  # z <- predict(m1, test)
  # 
  # sqrt(sum((test$NumConsults-z)^2)/length(z))
  
  m1 <- lm(data = x, NumConsults ~ ConsultationServiceDesc + Consult_Hour + Consult_DoW)
  # m2 <- lm(data = x, NumConsults ~ ConsultationServiceDesc + Consult_Hour)
  # m3 <- lm(data = x, NumConsults ~ ConsultationServiceDesc + Consult_DoW)
  m4 <- lm(data = x, NumConsults ~ ConsultationServiceDesc + Consult_Hour + Consult_DoW + Consult_Hour:Consult_DoW)
  # summary(m1) # F190.9 pval <2.2e-16; 0.3774 Rsq
  # summary(m2) # F235.9 pval <2.2e-16; 0.3749 Rsq
  # summary(m3) # F355.5 pval <2.2e-16; 0.2089 Rsq
  # summary(m4) # F35.16 pval <2.2e-16; 0.3791 Rsq
  # 
  # AIC(m1,m2,m3) #supports M1 as the best model, but i barely edges out m2
  # anova(m2,m1) #supports that the adding DoW with hours is worth the extra variable
  
  df1.coeffs <- 
    tidy(m1) %>% 
    mutate(lower = estimate - 1.96 * std.error, 
           upper = estimate + 1.96 * std.error)
  
  #labels
  z <- data.frame(term=as.character(1:23), label =c("01AM", "02AM", "03AM", "04AM", "05AM", "06AM", "07AM", "08AM", "09AM", "10AM", "11AM", "12PM", "01PM", "02PM", "03PM", "04PM", "05PM", "06PM", "07PM", "08PM", "09PM", "10PM", "11PM") )
  z$label <- factor(z$label, levels = z$label)
  
  df1.coeffs %>% 
    filter(grepl("Hour", term)) %>% 
    mutate(term = substring(term, 13)) %>%
    left_join(z, by="term") %>%
    ggplot()  +
    geom_pointrange(aes(x = label, 
                        ymin = lower, 
                        ymax = upper, 
                        y = estimate)) + 
    geom_hline(yintercept = 0) + 
    
    scale_y_continuous(limits = c(-2, 2), 
                       breaks = seq(-10, 10, 4)) + 
    
    labs(x = "Time of Day", 
         y = "Difference in average daily ED consult requests" ,
         title = "RHS ED \nImpact of Time of Day on average daily ED consult requests", 
         subtitle = "These estimates control for day of week and consult service requested \n\nBaseline - 12AM") + 
    theme_light() +
    theme(panel.grid.minor = element_line(colour = "grey95"), 
          panel.grid.major = element_line(colour = "grey95"),
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  return(-1)
}
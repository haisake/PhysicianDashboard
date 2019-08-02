#'--- 
#' title: "Census: Does Day of Week Tell us anything?"
#' author: "Hans aisake
#' date: "2019-07-08"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#' ---

#+ libraries, message = FALSE 
#library(here)
#library(tidyverse)
library(DT)
library(ggbeeswarm)
library(broom)
library(lubridate) #might be the only one I need for computation

# 1) pull in the census data and convert it to the structure needed for analysis
  # two sets : census overall, ALC census
  df1.census <- dataList2$capplanCensus_df
  targetService <- "Hospitalist"
  
  df2.census <- df1.census %>% group_by(Date, DoctorService) %>% summarize( census = sum(Census))
  df2.census <- df2.census %>% filter(DoctorService ==targetService)
  df2.census <- df2.census %>% ungroup()

  #add month and day of week
  df2.census$DayOfWeek <- weekdays(df2.census$Date)
  df2.census$Month <- months(df2.census$Date, abbreviate=TRUE)

  df2.census <- 
    df2.census %>% 
    mutate(weekday = DayOfWeek %>% as.factor(), 
           lag_census = lag(census), 
           date = Date,
           month = Month %>% as.factor(),
           year = year(Date) %>% as.factor())
  #df2.census %>% datatable()
  #str(df2.census)

# 2) ?? --------

  m1.census <- lm(census ~ lag_census + weekday + month, data = df2.census)
  m2.census <- lm(census ~ weekday + month, data = df2.census)
  
  #plot with fitted and smoothed values
  df2.census %>%
    ggplot(aes(x = Date, y = census)) +
    geom_point() +
    geom_line( y= c(NA,m1.census$fitted.values), col="red", alpha=0.4) +
    geom_line( y= m2.census$fitted.values, col="green", alpha=0.4) +
    geom_smooth() +
    labs(title =paste0("Census-",targetService )) +
    theme_light() +
    theme(panel.grid.minor = element_line(colour = "grey95"), panel.grid.major = element_line(colour = "grey95"))
  
  


df2.census %>% filter(DoctorService==ds[3]) %>%
  pull(Census) %>% 
  hist

# 3) regression ------------
m1.census <- lm(Census ~ lag_census + weekday + month, 
                      data = df2.census %>% filter(DoctorService==ds[3]))

summary(m1.census )

par(mfrow = c(2,2))
plot(m1.census )
par(mfrow = c(1,1))

df3.coeffs <- 
  tidy(m1.census ) %>% 
  mutate(lower = estimate - 1.96 * std.error, 
         upper = estimate + 1.96 * std.error)


#'
#' ## Notes
#'  

#'  Model not significant. We cannot conclude that there is any weekday effect -
#'  too much variability, not enough data
#'  


m2.all_ed_visits <- lm(ed_visits ~ weekday + year + lag_ed_visits, 
                       data = df2.census)

summary(m2.all_ed_visits)

par(mfrow = c(2,2))
plot(m2.all_ed_visits)
par(mfrow = c(1,1))


df4.coeffs <- 
  tidy(m2.all_ed_visits) %>% 
  mutate(lower = estimate - 1.96 * std.error, 
         upper = estimate + 1.96 * std.error)

df4.coeffs %>% 
  select(term, 
         lower, 
         estimate, 
         upper, 
         everything()) %>% 
  datatable() %>% 
  formatRound(2:7, 2)


#'
#' ## Notes
#'

#'  In this case, there are significant weekday effects 
#'  



# 4) visualize effects: ----- 

df4.coeffs %>% 
  filter(grepl("weekday", term)) %>% 
  
  mutate(term = substring(term, 8)) %>% 
  mutate(term = factor(term, 
                       levels = c("Monday", 
                                  "Tuesday", 
                                  "Wednesday", 
                                  "Thursday", 
                                  "Friday",
                                  "Saturday", 
                                  "Sunday"))) %>%
  
  ggplot()  +
  geom_pointrange(aes(x = term, 
                      ymin = lower, 
                      ymax = upper, 
                      y = estimate)) + 
  geom_hline(yintercept = 0) + 
  
  scale_y_continuous(limits = c(-10, 10), 
                     breaks = seq(-10, 10, 4)) + 
  
  labs(x = "Day of week", 
       y = "Difference in average daily ED visits" ,
       title = "PRGH ED \nImpact of Day of Week on average daily ED visits", 
       subtitle = "These estimates control for year and previous day's ED visits \n\nBaseline - Monday") + 
  
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
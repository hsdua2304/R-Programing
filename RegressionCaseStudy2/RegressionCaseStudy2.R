# Regression Case Study-2
# Leading e-commerce company have point of sale data for each customer with demographics
# and would like to solve the following problems.
#   1. The drivers for the store purchase event count and would like to predict
#   the store purchase event count for given drivers
#   2. The drivers for the customer churn and predict the customer churn (churn_status)
#   given the drivers.

setwd('E:/R-Programing/RegressionCaseStudy2')
ecom <- read.csv('E:/R-Programing/RegressionCaseStudy2/E_Commerce_Data.csv')

# Understanding the Data

myStats <- function(x){
  n <- length(x)
  nmiss <- sum(is.na(x))
  mean <- mean(x,na.rm = T)
  std <- sd(x,na.rm = T)
  min <- min(x,na.rm = T)
  percentile <- quantile(x,na.rm = T,p=c(0.01,0.05,0.25,0.50,0.75,0.95,0.99))
  max <- max(x)
  LC <- mean - 3*std
  UC <- mean + 3*std
  
  return(c(N=n,Nmiss = nmiss,Mean=mean,StdDev=std,Min=min,P=percentile,Max=max,LowerCap=LC,UpperCap=UC))
}

vars <- c(  'session_length_seconds',
            'session_count',
            'event_count',
            'closed_session_event_count',
            'open_session_event_count',
            'quest_completed_event_count',
            'store_purchase_event_count',
            'active_days')

Stats_Summary <- t(apply(ecom[vars],2,FUN = myStats))

View(Stats_Summary)
write.csv(Stats_Summary,file = 'stats_Summary.csv')

# Outlier Treatment

OutlierTreatment <- function(x){
  x <- replace(x,x > quantile(x,.95,na.rm = T),quantile(x,.95,na.rm = T))
  x <- replace(x,x < quantile(x,.01,na.rm = T),quantile(x,.01,na.rm = T))
  
}

ecom[vars] <- apply(data.frame(ecom[,vars]), 2, FUN = OutlierTreatment)


# Relationship between Variables

require(corrplot)
corrplot(cor(ecom[, vars],use="pairwise.complete.obs"), method = "circle", tl.cex = 0.7)

# Creating training and Testing Dataset

sampleSize <- floor(0.70*nrow(ecom))
set.seed(1234)

trainIndex <- sample(seq_len(nrow(ecom)),size = sampleSize)

train <- ecom[trainIndex,]
test <- ecom[-trainIndex,]

#Building model Multiple Linear Regression

fit <- lm(store_purchase_event_count ~ churn_status + session_length_seconds + session_count + event_count
          + closed_session_event_count + open_session_event_count + quest_completed_event_count + active_days,
          data = train)

library(MASS)
step <- stepAIC(fit, direction="both")
step$anova

fit <- lm(store_purchase_event_count ~ session_length_seconds + session_count + 
            event_count + open_session_event_count + quest_completed_event_count + 
            active_days,data = train)

summary(fit)

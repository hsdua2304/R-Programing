#Importing the data
setwd("E:/R-Programing/Linear Regression in R")

mydata <- read.csv("E:/R-Programing/Linear Regression in R/Car_sales.csv")

#Understand the data
str(mydata)
names(mydata)
View(mydata)
head(mydata)
tail(mydata)
dim(mydata)
nrow(mydata)
ncol(mydata)
summary(mydata)

#Understand distribution of the data
summary1 <- summary(mydata)
summary2 <- str(mydata)
library(psych)
summary3 <- describe(mydata)

write.csv(summary1, file = "summary1.csv")
write.csv(summary2, file = "summary2.csv")
write.csv(summary3, file = "summary3.csv")

# user defined function for creating descriptive statistics
mystats <- function(x) {
  n <- length(x)
  nmiss <- sum(is.na(x))
  m <- mean(x,na.rm=T)
  s <- sd(x,na.rm=T)
  min <- min(x,na.rm=T)
  pctl <- quantile(x, na.rm=T, p=c(0.01,0.05,0.1,0.25,0.5,0.75,0.9, 0.95,0.99))
  max <- max(x,na.rm=T)
  UC <- m+3*s
  LC <- m-3*s
  return(c(n=n, nmiss=nmiss, mean=m, stdev=s,min = min, pctl=pctl, max=max, UC=UC, LC=LC))
}

vars <- 
  

stats<-apply(mydata[vars], 2, mystats)
write.csv(stats, file = "stats.csv")


#R code for missing impuatation data
#Method-1:

mydata$X__year_resale_value[is.na(mydata$X__year_resale_value ==  TRUE)] <- 18.0729752066116
mydata$Price_in_thousands[is.na(mydata$Price_in_thousands ==  TRUE)] <- 27.3907548387097
mydata$Engine_size[is.na(mydata$Engine_size ==  TRUE)] <- 3.06089743589744
mydata$Horsepower[is.na(mydata$Horsepower ==  TRUE)] <- 185.948717948718
mydata$Wheelbase[is.na(mydata$Wheelbase ==  TRUE)] <- 107.487179487179
mydata$Width[is.na(mydata$Width ==  TRUE)] <- 71.15
mydata$Length[is.na(mydata$Length ==  TRUE)] <- 187.34358974359
mydata$Curb_weight[is.na(mydata$Curb_weight ==  TRUE)] <- 3.37802580645161
mydata$Fuel_capacity[is.na(mydata$Fuel_capacity ==  TRUE)] <- 23.8441558441558
mydata$Fuel_efficiency[is.na(mydata$Fuel_efficiency ==  TRUE)] <- 17.9519230769231

#Method-2:
#Treating missing values
mydata[vars] <- apply(data.frame(mydata[,vars]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
#mydata[cat_var] <- apply(data.frame(mydata[,cat_var]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})


#Handling outliers in numeric variables
#Method1: Cap the highest value at 99th percentile
plot(mydata$Sales_in_thousands) 
mydata$Sales_in_thousands[mydata$Sales_in_thousands>257] <- quantile(mydata$Sales_in_thousands,.99, na.rm = T)

#Method-2: Filtering outliers in DV
mydata1 <- subset(mydata,mydata$Sales_in_thousands<257)

#code for categorical variables - Dummy variable creation
mydata1$Vehicle_type <- factor(mydata1$Vehicle_type)
levels(mydata1$Vehicle_type) <- c("Car","Passenger")
str(mydata1)

#Check the correlation metrics


#Understand data with graphs (univariate and Bi-variate analysis)
#Scatter plot
scatter.smooth(x=mydata1$Price_in_thousands, y=mydata1$Sales_in_thousands, main="sales ~ price")  # scatterplot

#Box plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(mydata1$Sales_in_thousands, main="Sales_in_thousands", sub=paste("Outlier rows: ", boxplot.stats(mydata1$Sales_in_thousands)$out))  # box plot for 'speed'
boxplot(mydata1$Price_in_thousands, main="Price_in_thousands", sub=paste("Outlier rows: ", boxplot.stats(mydata1$Price_in_thousands)$out))  # box plot for 'distance'

#Density plot – Check if the response variable is close to normality
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(mydata1$Sales_in_thousands), main="Density Plot: Sales_in_thousands", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(mydata1$Sales_in_thousands), 2)))  # density plot for 'speed'
polygon(density(mydata1$Sales_in_thousands), col="red")
plot(density(mydata1$Price_in_thousands), main="Density Plot: Price_in_thousands", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(mydata1$Price_in_thousands), 2)))  # density plot for 'dist'
polygon(density(mydata1$Price_in_thousands), col="red")

require(car)
scatterplot(mydata1$Sales_in_thousands,mydata1$X__year_resale_value)
scatterplotMatrix(~Sales_in_thousands+X__year_resale_value + Price_in_thousands+ Engine_size+Horsepower+Wheelbase+Width
                  +Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Vehicle_type, data=mydata1)

require(corrplot)
corrplot(cor(mydata1[, vars],use="pairwise.complete.obs"), method = "circle", tl.cex = 0.7)


## Creating Training & Validation Data sets(70:30)
smp_size <- floor(0.70 * nrow(mydata1))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(mydata1)), size = smp_size)

train <- mydata1[train_ind, ]
test <- mydata1[-train_ind, ]

View(train)

#Building model Multiple Linear Regression Example 
fit <- lm(Sales_in_thousands ~ X__year_resale_value + Price_in_thousands+ Engine_size+Horsepower+Wheelbase+Width
          +Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Vehicle_type, data=train)
summary(fit) # show results
hist(train$Sales_in_thousands)

train$ln_sales <- log(train$Sales_in_thousands)
hist(train$ln_sales)

fit <- lm(ln_sales ~ X__year_resale_value + Price_in_thousands+ Engine_size+Horsepower+Wheelbase+Width
          +Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Vehicle_type, data=train)
summary(fit) # show results


# Other useful functions 

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
hist(residuals(fit)) # residuals
anova(fit) # anova table 
influence(fit) # regression diagnostics
ls(influence(fit))
?influence()
AIC(fit)
BIC(fit)

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)
layout(matrix(c(1),1,1)) # optional 4 graphs/page 

# Stepwise Regression
library(MASS)
fit <- lm(ln_sales ~ X__year_resale_value + Price_in_thousands+ Engine_size+Horsepower+Wheelbase+Width
          +Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Vehicle_type, data=train)
step <- stepAIC(fit, direction="both")
step$anova # display results

View(train)

fit <- lm(ln_sales ~ Price_in_thousands + Engine_size + Wheelbase + Fuel_efficiency + 
            Vehicle_type ,data=train)

train$VT[train$Vehicle_type ==  "Passenger"] <- 1
train$VT[train$Vehicle_type ==  "Car"] <- 0

summary(fit)
library(car)
vif(fit)

############################### SCoring Data sets/Predicting the sales#####################################
#Method-1
train$Ln_pre_sales<- (-2.321593  +
                          train$Price_in_thousands* -0.054988 +
                          train$Engine_size*0.254696  +
                          train$Wheelbase*0.047546	+
                          train$Fuel_efficiency*0.068975+
                          train$VT*-0.573255)
train$Pre_sales= exp(train$Ln_pre_sales);

#Method-2
train<-cbind(train,Ln_pre_sales=predict(fit, newdata=train), Pre_sales=exp(predict(fit, newdata=train)))

#################### Creating Deciles####################################
# find the decile locations 
decLocations <- quantile(train$Pre_sales, probs = seq(0.1,0.9,by=0.1))
# use findInterval with -Inf and Inf as upper and lower bounds
mydata1$train <- findInterval(train$Pre_sales,c(-Inf,decLocations, Inf))

summary(train$decile)
xtabs(~decile,train)

#write.csv(train,"train.csv")

##################################Decile Analysis Reports

require(sqldf)
mydata1_DA <- sqldf("select decile, count(decile) as count, avg(Pre_sales) as avg_pre_sales,   avg(Sales_in_thousands) as sum_Actual_sales,
                    sum(Sales_in_thousands) as avg_Actual_sales                   
                    from train
                    group by decile
                    order by decile desc")

write.csv(mydata1_DA,"mydata1_DA.csv")

#Error rates
actuals_preds <- data.frame(cbind(actuals=train$Sales_in_thousands, predicted=train$Pre_sales))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals))/actuals_preds$actuals)

###################################Exercise

#You require to work on the below things using validation data set
  #1. Scoring using equation & predict function
  #2. Decile analysis for validation data
  #3. Calculating error metrics



##############################Important Information#########################

#How to know if the model is best fit for your data?
#The most common metrics to look at while selecting the model are:

#STATISTIC	                                CRITERION
#R-Squared	                                Higher the better (> 0.70)
#Adj R-Squared	                            Higher the better
#F-Statistic	                              Higher the better
#Std. Error	                              Closer to zero the better
#t-statistic	                              Should be greater 1.96 for p-value to be less than 0.05
#AIC	                                      Lower the better
#BIC	                                      Lower the better
#Mallows cp	                              Should be close to the number of predictors in model
#MAPE (Mean absolute percentage error)	    Lower the better
#MSE (Mean squared error)                	Lower the better
#Min_Max Accuracy => mean(min(actual, predicted)/max(actual, predicted))	Higher the better

#######



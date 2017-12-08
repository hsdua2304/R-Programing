setwd("E:/R-Programing/Linear Regression in R")

carSales<-read.csv('E:/R-Programing/Linear Regression in R/CAR_SALES.csv')

#Understanding the Data

str(carSales)
nrow(carSales)
ncol(carSales)
summary_carSales <- describe(carSales)

write.csv(summary_carSales,file = 'summary_carSales.csv')

vars <-c( "Sales_in_thousands" , "X__year_resale_value" ,  "Price_in_thousands",   
          "Engine_size" , "Horsepower", "Wheelbase" , "Width" ,"Power_perf_factor" , "Length" , "Curb_weight" , 
          "Fuel_capacity", "Fuel_efficiency" )

myStats <- function(x){
  n <- length(x)
  nmiss <- sum(is.na(x))
  mean <- mean(x,na.rm = T)
  std <- sd(x,na.rm = T)
  min <- min(x,na.rm = T)
  percentile <- quantile(x,na.rm = T,p=c(0.01,0.05,0.1,0.25,0.50,0.75,0.95,0.99))
  max <- max(x)
  LC <- mean - 3*std
  UC <- mean + 3*std
  
  return(c(N=n,Nmiss = nmiss,Mean=mean,StdDev=std,Min=min,Percentile=percentile,Max=max,LowerCap=LC,UpperCap=UC))
}

stats <- t(apply(carSales[vars],2,myStats))
write.csv(stats,file = 'stats.csv')


#Outlier Treatment

#plot(carSales$Sales_in_thousands)
#plot(carSales$Price_in_thousands)

carSales$Sales_in_thousands[carSales$Sales_in_thousands>257.086342425636] <- 257.086342425636
carSales$X__year_resale_value[carSales$X__year_resale_value>52.4331275042866] <- 52.4331275042866
carSales$Price_in_thousands[carSales$Price_in_thousands>70.4457144064253] <- 70.4457144064253
carSales$Engine_size[carSales$Engine_size>6.19485635572492] <- 6.19485635572492
carSales$Horsepower[carSales$Horsepower>356.049680523839] <- 356.049680523839
carSales$Wheelbase[carSales$Wheelbase>130.411088576609] <- 130.411088576609
carSales$Width[carSales$Width>81.5056155873781] <- 81.5056155873781
carSales$Power_perf_factor[carSales$Power_perf_factor>152.471583487459] <- 152.471583487459
carSales$Length[carSales$Length>227.63885259714] <- 227.63885259714
carSales$Curb_weight[carSales$Curb_weight>5.2695307097395] <- 5.2695307097395
carSales$Fuel_capacity[carSales$Fuel_capacity>29.615686871517] <- 29.615686871517
carSales$Fuel_efficiency[carSales$Fuel_efficiency>36.6922725298114] <- 36.6922725298114


#Missing Value Treatment

carSales[vars] <- apply(carSales[,vars],2,function(x){x<-replace(x,is.na(x),mean(x,na.rm = T))})
#t(apply(carSales[vars],2,myStats))


#Converting Categorical variable as Factor

carSales$Vehicle_type <- factor(carSales$Vehicle_type)


#Relatonship between the variables

install.packages('corrplot')
require(corrplot)
corrplot(cor(carSales[, vars],use="pairwise.complete.obs"), method = "circle", tl.cex = 0.7)

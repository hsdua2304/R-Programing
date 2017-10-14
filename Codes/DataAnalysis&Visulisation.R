stores<-read.csv(choose.files(),header = T)

data<-stores


# DESCRIPTIVE sTATISTICS

View(data)
summary(data)
names(data)
mean(data$TotalSales)
with(data,mean(TotalSales))
var(data$TotalSales)
quantile(data$TotalSales,c(.3,.6,.9))
quantile(data$TotalSales,probs = seq(0,1,0.2))
which.max(data$TotalSales)
data$Sales_rank<-rank(data$TotalSales,ties.method = 'first')
View(data)

describe(data)

# Ties.method specifies the method rank uses to break ties. 
# Suppose you have a vector c(1,2,3,3,4,5). It's obvious that 1 is first, and 2 is second. However,
# it's not clear what ranks should be assigned to the first and second 3s. 
# Ties.method determines how this is done. There are a few options:
#   
# average assigns each tied element the "average" rank. The ranks would therefore be 1, 2, 3.5, 3.5, 5, 6
# first lets the "earlier" entry "win", so the ranks are in numerical order (1,2,3,4,5,6)
# min assigns every tied element to the lowest rank, so you get 1,2,3,3,5,6 
# max does the opposite: tied elements get the highest rank (1,2,4,4,5,6)
# random breaks ties randomly, so you'd get either (1,2,3,4,5,6) or (1,2,4,3,5,6).

# FREQUENCY TABLES

# UNIVARIATE

s1<-table(stores$StoreType) #For frequency count for the categorical variables
View(s1)
s2<-prop.table(s1) #Probability count for the categorical variable
View(s2)
s2<-prop.table(table(stores$StoreType))
View(s2)
rbind(s1,s2) #combining row wise
cbind(s1,s2) #combining Columns wise

# install.packages('gmodels',dependencies = T)
require(gmodels)

CrossTable(stores$StoreType) #Combination of table() and prop.table()

# BIVARIATE

table(stores$StoreType,stores$Location)
prop.table(table(stores$StoreType,stores$Location))

CrossTable(stores$StoreType,stores$Location)
CrossTable(stores$StoreType,stores$Location,prop.r=F,prop.c=F,prop.chisq = F)

install.packages('vcd',dependencies = T)
require(vcd)


xtabs(~stores$Location+stores$StoreType)
xtabs(stores$TotalSales~stores$Location+stores$StoreType)
prop.table(xtabs(stores$TotalSales~stores$Location+stores$StoreType))


# SUMMARIZATON OF NUMERICAL VARIABLES

require(psych)
data.frame(stores)


# User Defined Function

mystatistics<-function(x){
  nmiss<-sum(is.na(x))
  a<-x[!is.na(x)]
  m<-mean(a)
  n<-length(a)
  s<-sd(a)
  v<-var(a)
  min<-min(a)
  q1<-quantile(a,.25)
  q3<-quantile(a,.75)
  max<-max(a)
  UL<-m+3*s
  LL<-m-3*s
  outlier_flag<-max>UL | min<LL
  
  return(c(n=n,nmiss=nmiss,outlier_flag=outlier_flag,mean=m,stdev=s,min=min,max=max,UL=UL,LL=LL))
}

mystats(stores$OperatingCost)
data_audit_report<-data.frame(t(apply(stores[6:15],2,mystatistics)))
View(data_audit_report)


# Descriptive Statistics by Group

require(psych)
# Method1
by(stores[c("OperatingCost","Total_Customers")],stores$OnlinePresence,
   summary)
by(stores,stores$StoreSegment,summary)
by(stores,stores$StoreSegment,describe)

# by(stores$TotalSales,stores$StoreSegment,mean)
# ?by()

# Method-2

describe.by(stores,group = stores$Location)
describeBy(stores,group = stores$Location)


# Method-3

install.packages('doBy',dependencies = T)
require(doBy)

summaryBy(TotalSales + ProfitPercust ~ Location + StoreSegment,data = stores,
          FUN = mean())

# Method-4

tapply(stores$TotalSales,stores$StoreType,mean)
tapply(stores$TotalSales,list(stores$StoreType,stores$Location),mean)
tapply(stores$TotalSales,list(stores$StoreType,stores$Location,
                              stores$OnlinePresence),mean)

# Method-5

aggregate(TotalSales~StoreType+Location,stores,FUN=mean)


# Method-6

require(dplyr)

gg <- group_by(stores,Location, StoreType)
cdata <- summarise(gg,
                   N = sum(!is.na(TotalSales)),
                   mean = mean(TotalSales, na.rm=TRUE),
                   sd = sd(TotalSales, na.rm=T),
                   se = sd / sqrt(N))

View(cdata)



# CORRELATION

# correlation between two variables
cor(stores$BasketSize,stores$TotalSales)

# Correlation Matrix

install.packages('corrplot')
require(corrplot)

corrplot(cor(stores[,5:10],use="pairwise.complete.obs"),method = 'number',
         tl.cex = 0.5)

# Calculate total number of missing values in dataset in each varibable

sapply(stores,function(x){sum(is.na(x))})
View(stores)

# Getting all numeric and Factor variables

names(stores)[sapply(stores, FUN = is.numeric)]
names(stores)[sapply(stores,FUN = is.factor)]
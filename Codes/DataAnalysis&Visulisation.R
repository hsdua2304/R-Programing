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

# FREQUENCY TABLES

# UNIVARIATE

s1<-table(stores$StoreType)
View(s1)
s2<-prop.table(s1)
View(s2)
s2<-prop.table(table(stores$StoreType))
View(s2)
rbind(s1,s2)
cbind(s1,s2)

install.packages('gmodels',dependencies = T)
require(gmodels)

CrossTable(stores$StoreType)

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

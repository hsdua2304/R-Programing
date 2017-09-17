#-----------Importtant Leaning during Practice----------------#

search()
# Detaching multiple packages at once
Vectorize(detach)(name=paste0("package:",c("xlsx","XLConnect")),unload=T,character.only=T)
?Vectorize()


#--------------------------------------------------------------#




#IMPORT AND UNDERSTAND THE DATA

stores <- read.csv(choose.files())
View(stores)
edit(stores) #edit the current version of dataset and return new version of dataset
fix(stores)  #edit the current verison of dataset and return the updated version of the dataset

str(stores)
require(psych)
describe(stores)

#SUBSETTING DATA

# Method:1

s0<-stores[stores$TotalSales>200,c("Total_Customers","BasketSize","Tenure")]
s1<-stores[stores$TotalSales>200,1:10] #From 1 to 10th column
s2<-stores[stores$TotalSales>100,-(1:10)] #Leaving 1 to 10th column
s3<-stores[stores$TotalSales>200,c("AcqCostPercust","BasketSize")]
View(s3)

#Method:2

r1<-cbind(No_cust=stores$Total_Customers,Avg_transValue=stores$BasketSize,No_Years=stores$Tenure)
View(r1)

#Method:3

q1<-subset(stores,TotalSales>100, select=c(1:10))
q2<-subset(stores,TotalSales>100,select=-c(1:10))
q3<subset(stores,TotalSales>100,select=c('AcqCostPercust','BasketSize'))
View(q1)

# Droping variables

d1<-stores[!names(stores)%in%c('Location')]
View(d1)

# CREATING NEW VARIABLES

# Mehtod:1

stores$TotalCost<-stores$AcqCostPercust * stores$Total_Customers + stores$OperatingCost
View(stores)

#Method:2

stores<-transform(stores, TotalCost1=AcqCostPercust*Total_Customers + OperatingCost)
View(stores)

require(plyr)
data1<-mutate(stores,S=TotalCost+OperatingCost)
View(data1)

#BINNING VARAIBLES


#Method:1
stores$TotalClass1[stores$TotalSales > 240]<-'High'
stores$TotalClass1[stores$TotalSales > 120 & stores$TotalSales <=240]<-'Average'
stores$TotalClass1[stores$TotalSales < 120]<-'Low'
View(stores)

# Method:2
stores$TotalClass2<-cut(stores$TotalSales,breaks=c(-Inf,120,240,Inf),label=c("Low",'Average','High'))  
View(stores)



#sORTING DATA

#Method:1

newstores1<-stores[order(stores$OperatingCost, decreasing = T),]
View(newstores1)

newstores2<-stores[order(-(stores$OperatingCost)),]
View(newstores2)

#Reverse Sorting
newstores3<-stores[order(stores$Location,stores$TotalSales,decreasing = T),]
View(newstores3)

newstores4<-with(stores,stores[order(StoreType,Location,-TotalSales),])
View(newstores4)

# Method:2
require(dplyr)
newstores5<-arrange(stores,StoreName)
View(newstores5)

newstores6<-arrange(stores,StoreName,desc(Location))
View(newstores6)

newstores7<-arrange(stores,-BasketSize)
View(newstores7)

detach(package:dplyr,unload = T)

# REMOVING DUPLICATES FROM DATA

# Method:1

UniqueVal<-unique(stores)
View(UniqueVal)


# FINDING DUPLICATES VALUE

stores[duplicated(stores),]
duplicates<-stores[duplicated(stores$StoreName) & duplicated(stores$AcqCostPercust),]
View(duplicates)


# JOINING DATASETS OR MERGING

dg<-read.csv('E:/R-Programing/Datasets/Demographic_Data.csv',header = T)
ts<-read.csv('E:/R-Programing/Datasets/Transaction_Summary.csv',header = T)

View(head(dg))
View(head(ts))

# Inner Join

dg_ts_inner<-merge(dg,ts, by.x = c('CustName'),by.y = c('CustomerName'),all = F)
View(dg_ts_inner)

# Full Join

dg_ts_full<-merge(dg,ts,by.x = c('CustName'),by.y = c('CustomerName'),all=T)
View(dg_ts_full)

# Left Join

dg_ts_Left<-merge(dg,ts,by.x=c('CustName'),by.y=c('CustomerName'),all.x=T)
View(dg_ts_Left)

# Right JOin

dg_ts_right<-merge(dg,ts,by.x = c('CustName'),by.y = c('CustomerName'),all.y=T)
View(dg_ts_right)

# Left Outer Join
ds_ts_outer_left<-merge(dg,ts,by.x = c('CustName'),by.y = c('CustomerName'),all.y = F)
View(ds_ts_outer_left)

# RESHAPING THE DATASETS

store_sales<-read.table(choose.files(),sep = ',',header = T)
View(store_sales)

v1<-names(store_sales)[3:13] # Vector of monthly sales column names
View(v1)

# reshaping wide to long
store_sales.Wide_Long1<-reshape(store_sales,idvar = c('StoreID','City'),varying = v1 ,timevar = c('Month'),
                                v.names=c('Sales'),times=c("Jan_Sales","Feb_sales","Mar_sales","Apr_sales",
                                                           "May_Sales","June_sales","Jul_sales","Aug_sales",
                                                           "Sep_Sales","Oct_Sales","Nov_Sales" ),direction = 'long')

View(store_sales.Wide_Long1)


# reshaping long to wide

store_sales.long_wide<-reshape(store_sales.Wide_Long1,idvar = c('StoreID','City'),v.names = c('Sales'),
                               timevar = c('Month'),direction='wide')

View(store_sales.long_wide)


# SAMPLING

# sampling
View(stores)
sample(stores$StoreCode,size=10)


# stratified sampling

install.packages('sampling')
require(sampling)
strata(stores, stratanames=NULL ,size, method=c("srswor","srswr","poisson","systematic"),pik,description=F)

p=0.1
d=stores
stratum='Location'

Text=paste0('stores$',stratum)
size<-ceiling(table(eval(parse(text=Text))) *p)
strat<-strata(d,stratanames = stratum,size = size,method = 'srswor')
View(strat)

dsample<-getdata(d,strat)
View(dsample)
table(dsample$Location)

# RENAMING VARIABLES IN A DATAFRAME

df2<-stores
names(df2)
names(df2)[names(df2)=='StoreCode'] <- 'StoreID'
names(df2)

install.packages('reshape')
require(reshape)
mydata<-rename(stores, c(Store='StoreCode',storeLoc='Location'))
View(mydata)

# RE-ORDERING COLUMNS

stores1<-stores[c('StoreName','StoreType','Location','OperatingCost','Total_Customers','AcqCostPercust',
                  'BasketSize','ProfitPercust','OwnStore')]
View(stores1)

stores1<-stores[c(5,4,2,1,3,6:10)]
View(stores1)

# APPENDING DATA

?table()
?prop.table()

s1<-table(stores$StoreType)
View(s1)
s2<-prop.table(s1)
View(s2)

rbind(s1,s2)
cbind(s1,s2)


# FORMATING DATA

# example:1

Sys.time()
format(Sys.time(),format = '%b_%d_%y_%H_%M')

#example:2
format(1:10)
format(1:10,trim=T)


# example:3

zz<-data.frame('(row names)' = c('aaaaa','b'),check.names = F)
format(zz)
format(zz,justify = 'left')

# example:4

format(13.7)
format(13.7, nsmall = 3)
format(c(6,13.12,123.234),digits = 5,trim = F)
format(c(6.0,13.12,123.234),digits = 2,nsmall = 1)

format(2^31-1)
format(2^31-1,scientific = T)

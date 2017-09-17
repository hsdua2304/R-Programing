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



# CREATING NEW VARIABLES

# Mehtod:1

stores$TotalCost<-stores$AcqCostPercust * stores$Total_Customers + stores$OperatingCost
View(stores)

#Method:2

stores<-transform(stores, TotalCost1=AcqCostPercust*Total_Customers + OperatingCost)
View(stores)


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




# 1. Printing Current R Version, OS information and Loaded packages in current session
sessionInfo()

# 2. Creating a new object
abc=3

# 3. Creating numerical, character and Logical Vectors
a = c(1,2,3,4,5,6)
b = c('a','b','c','d','e','f')
c = c(TRUE,FALSE)

# 4. Listing objects in current session
objects()

# 5. Problem-5
x = c(4,4,5,6,7,2,9)

n <- length(x)
mean <- mean(x)
sum <- sum(x,na.rm = T)
max <- max(x)
min <- min(x)
variance <- var(x)


x[3] # Third Position Element
x[seq(n) %% 2 == 1] # Odd Position Elements
x[2:6] # Elements from 2 to 6 th Position


# 6. 6x4 matrix
my_matrix <- matrix(data = 1:24,nrow = 6,ncol = 4,byrow = T)
my_matrix

# 7. Dataframe with following values
my_dataframe <- data.frame(storeID = c(111,208,113,408),
tenure = c(24,34,28,52),
storeType = c('Type1','Type2','Type1','Type1'),
status = c('Poor','Improved','Excellent','Poor'))
my_dataframe

# 8. Printing specific columns in dataframe
my_dataframe[,c('storeID','tenure')]
my_dataframe[,c('storeType','status')]
my_dataframe[,c('tenure')]


# 9. Creating labels for the factors

ethnicity <- c('White','African American','Brown','Asian')
status <- c('Poor','Improved','Excellent','Poor')
label <- c('Poor','Average','Good','Excellent')

outcome1 <- factor(c(1,3,2,4,3,1,1),levels = c(1,2,3,4),labels = label)
outcome2 <- factor(c(1,3,2,4,3,1,1),levels = c(1,2,3,4),labels = ethnicity)

outcome1
outcome2

# 10. Problem-10

mylist <- list(h=c(25,26,18,39),j=matrix(data = 1:10,nrow = 5,ncol = 2,byrow = T),k=c('One','Two','Three'))
mylist[[1]]
mylist[[2]]

# 11. Summary statistics of stores dataset
stores<-read.csv(choose.files())
summary(stores)

# 12. Difference between with() function.
summary(stores$OperatingCost)
with(stores,summary(OperatingCost))
# We have to mention the column names when we use with function

# 13. Apply following function on stores datasets

class(stores)
names(stores)
length(stores)
dim(stores)
str(stores)
head(stores)
tail(stores)
fix(stores)


# 14. Create new variable by simple calculation and tansform function

stores$Operating_AcqCostPercust <- stores$OperatingCost + stores$AcqCostPercust

stores<-transform(stores, TotalCost=AcqCostPercust + OperatingCost)


# 15. Create new class store_class as

stores$StoreClass[stores$TotalSales<120] <- 'Low Performance Store'
stores$StoreClass[stores$TotalSales>=120 & stores$TotalSales<240] <- 'Average Performance store'
stores$StoreClass[stores$TotalSales>=240] <- 'High Performance Store'


# 16. Rename variable

require(reshape)
stores<-rename(stores,c(AcqCostPercust='AcqCost'))


# 17. Missing Values

# Missing Value Summary
for(i in 1:18){
  print('Missing Value Summary')
  print(colnames(stores)[i])
  print(class(stores[,i]))
  print(sum(is.na(stores[i])))
}

# Replacing NA's with 0
stores$AcqCost[is.na(stores$AcqCost)] <- 0
stores$Operating_AcqCostPercust[is.na(stores$Operating_AcqCostPercust)] <- 0


# Deleting rows having NA's
stores<-stores[!(is.na(stores$TotalCost)),]
View(stores)


# 18. Sorting the store dataset by Storetype, location and Total salses

newstore <- stores[order(stores$StoreType),]
View(newstore)

newstore2 <- stores[order(stores$Location,-stores$TotalSales),]
View(newstore2)


# 19. Create date vectors with given format

date1 <- as.Date(c('2014-06-22','2014-02-13'))
class(date1)

date2 <- as.Date(c('01/05/1965','08/16/1975'),format = '%m/%d/%y')
date2


# 20. Subsetting Stores dataset

s1 <- stores[c(5,7,8,9)]
s2 <- stores[-c(5,7,8,9)]
s3 <- stores[1:10,]
s4 <- stores[stores$StoreType == 'Apparel' & stores$TotalSales > 100,]
s5 <- stores[stores$TotalSales>100 & stores$TotalSales<300,c('StoreCode','StoreName','Location','TotalSales')]
s6 <- stores[stores$StoreType=='Electronincs' & stores$TotalSales>100,1:10]

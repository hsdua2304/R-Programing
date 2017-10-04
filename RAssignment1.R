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


# 9. 


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

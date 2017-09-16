# Installing a R Package

install.packages("slidify")


# To Load dependencies with the package
# packs<-installed.packages()
# exc <- names(packs[,"package"])
# av <- names(available.packages()[,1])
# ins <- av[!av %in% exc]
# install.packages(ins)

# Loading R package into Memory
library(MASS)
shoes

# Finding available function in package

install.packages("dplyr")
library(dplyr)
ls("package:dplyr")
lsf.str("package:dplyr")

# how to save your file Image and get current working directory

save.image()
getwd()
save.image("E:\\R-Programing\\sample.txt")
setwd("E:/R-Programing/Codes")

# List a current objects

ls()

# to get history of your commands

history()

# To get information about R sessions

sessionInfo()

# TO see available datasets

data()
help(quakes)

# Dates and Times in R
# Dates as compared as number of days since 1970-01-01
# Times is compared as number of seconds since 1970-01-01
# as.POSIXlt() and as.Date() are used to obtain diffrent Values from DateTime
x <- as.Date('2016-04-23')
x

unclass(x)
unclass(as.Date('1970-01-01'))


time <- Sys.time()
time

timeAsString <- as.POSIXlt(time)
timeAsString
names(unclass(timeAsString))
timeAsString$hour

# Current Packages loaded into the memory currently
search()

install.packages("ggplot2")
# for loading packeges inside a function function
require("ggplot2")
library(help="ggplot2")

# To unload the package from the memory

detach("package:ggplot2" ,unload=TRUE)

# Basic about R

x <- 10 + 20
x
y = 1:10
y
z<-c(10,11,12,13,14)
z
string<-c('a','b','c','d','e')
string[3]

# Paste Function
# Eval Function
# Parse Function

a<-10;b<-40
MyText <- paste("sum(",a,",",b,")")
MyText
cat("The Value after the evaluation of the above expression is : ",eval(parse(text=MyText)))
?parse()
text<-"sum(30,10)"
eval(parse(text=text))
?eval()


# Dates in R

mydates <- c("2014-04-23","2009-03-16")
mydates1 <- c("23/april/2014","16/Mar/2015")
mydates2 <- as.Date(mydates)
mydates3 <- as.Date(mydates1,"%d/%b/%Y")
str(mydates1)
format(mydates3, "%A, %d-%B-%Y")

Sys.Date()
date()

today<-Sys.Date()
# unclass(today) - this give number of days from 1/1/1970
format(today, format="%B %d %Y")


# Vector

my_vector<-c(-1,2,5,6.4,0.45,9)

mode(my_vector)
typeof(my_vector)
class(my_vector)
is.numeric(my_vector)


x1<-c(1,2,3,4,5,5,6,23)
x2<-c(6,6,7,8,98)
x3<-c('DL','HR','MP','UP')

x1+x2 #sum of two vectors
data<-c(x1,x2) #Concatenation of Vectors
data

#numerical Computations
my_vector2<-2*my_vector
my_vector2
x1 * c(2,3)
x1 + c(2,3,4)

#Mathematical Operartions

sqrt(my_vector)
log(my_vector)
sum(my_vector)
prod(my_vector)
cumsum(my_vector)
cumprod(my_vector)

#Functions related to Vectors

length(my_vector)
unique(x1)
head(x1,3)
tail(x1,2)
sort(x1)
order(x1)
rev(x1) #reverse the order of the elements in the vector
which.min(x1) #returns the position of minimum value
which.max(x1) #returns the position of the maximum value
rank(my_vector) #rank the elements

#Indexing

my_vector
my_vector[2]
my_vector[-2]
my_vector[1:3]
my_vector[-(1:4)]
my_vector[c(2,5)]
my_vector[my_vector < 4]
which(my_vector < 4)
my_vector[my_vector>1 & my_vector < 5]
my_vector[my_vector %in% -4:5.0]
match(my_vector,1:5)
my_vector[c(rank(my_vector))==2]
my_vector[order(my_vector)]


#combining two vectors
x3
cbind(my_vector,x3) #appending columns
rbind(my_vector,x3) #appending rows


#sequences

4:7
seq(0,1,length.out = 16)
seq(1,19,by=2)
seq(1,18,by=2)
seq(9,1,by=-2)
seq(17)


all(seq(17) == 1:17)
?all()


rep(1:4,2) #repeating the values
rep(1:4,each=2)
rep(1:4,c(1,2,3,4))
rep(1:6,each=2,len=10)
rep(1:4,each=2,times=3)


#Matrices
#Matrices are defined using the matix() function and three values : vector of values,
#number of rows and number of columns

(my_matrix<-matrix(c(1,2,3,4),2,2))
(my_matrix<-matrix(c(1,2,3,4),4,1))
(my_matrix<-matrix(c(1,2,3,4),2,2,byrow = TRUE))


my_matrix[1,1]
my_matrix[,1]
my_matrix[1,]


dim(my_matrix)
det(my_matrix)
eigen(my_matrix)
t(my_matrix)
(t(my_matrix) %*% my_matrix)
(t(my_matrix) %/% 2)


rowSums(my_matrix)
colSums(my_matrix)
rowMeans(my_matrix)
colMeans(my_matrix)

#Arrays
#Defined with array() function with 3 arguments: Vector of values, dimensions, dimension names

dim_names<-list(dimension1=c('A','B','C','D'),
                dimension2=c('A','B','C','D'),
                dimension3=c('1','2','3'))

(my_array<- array(data=1:32,dim=c(4,4,3),dimnames=dim_names))


#Indexing

my_array[3,2,1]
my_array[3,4,]
my_array[3,,]

#List
#Set of objects that can have different types

(my_list<- list(name = c("Analytix","Labs"),age=c(24,34),club="alabs",matrix=my_matrix))

mode(my_list)
typeof(my_list)
class(my_list)


#Indexing

dim(list)
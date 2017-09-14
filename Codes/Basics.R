x=50
y=100
z=x+y


library(sqldf)
search(sqldf)
help("sqldf")
??sqldf

library(MASS)
help("MASS")
??MASS
#vector
location <- c("Delhi","Gurgaon","Chennai","Banglore")
location
print(location)
str(location)

gender<-factor(c("Male","Female","N/A","Male","Female"))
gender
gender<-factor(gender)
length(gender)
head(gender,3)

install.packages("readr")
library(readr)

ds<-read_csv("C:/Users/Harmandeep/OneDrive/Analytix Labs/R/1. Introduction to R-Data Importing/DataSets for R sessions/stores.csv")
view(ds)
str(ds)
summary(ds)
View(ds)
library()
getwd()
ls()
write.csv(ds,"~/ds.csv")
save.image(file="~/R/Environment/ds_image.rdata")
list=ls()
list
rm(list)
list()
list
ls()
rm(ls())
rm(x)
rm(list<-ls())
list<-ls()
list
list
rm(list)
load("~/R/Environment/ds_image.rdata")

View(ds)
edit(ds)
head(ds)
View(head(ds))
View(tail(ds,20))
summary(ds)
describe(ds)
install.packages("psych")
library(psych)
describe(psych)
describe(ds)
warning(describe(ds))
require(psych)
describe(ds)

# Installing a R Package

install.packages("slidify")

packs<-installed.packages()
exc <- names(packs[,"dplyr"])
av <- names(available.packages()[,1])
ins <- av[!av %in% exc]
install.packages(ins)

install.packages('MASS')

# Loading R package into Memory
library(MASS)
shoes

# Finding available function in package

install.packages("downloader")
library(downloader)
install.packages("dplyr")



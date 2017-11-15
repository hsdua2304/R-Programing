sample_vec<-c(0,0,0,2,0,3,4,5,4,3,2,67,2,3,4,6,6,5,4,3,3,22,3,4,76,5,5,5,66,5,4,4,3,2,2,2,3,4,5,66,43,55,4,3,3)
sorted<-sort(sample_vec)


percentile<-function(a){
  vector<-list(which(a!=-1,T))
  l<-vector('list',length(a))
  for(i in vector){
    l[i]<-(i/length(a))*100
  }
  return(l)
}

length(sorted)

percentile_list<-percentile(sorted)
percentile_list<-as.numeric(percentile_list)
length(percentile_list)
class(percentile_list)
class(percentile_list)
l<-list('list',20)
l[1]<-2
l[3]<-4
l[5]<-5
l
length(sample_vec)
class(list(which(sample_vec!=0,T)))
m<-c()


?split()

d<-cu_360
str<-as.character(d$Location)
str(str)
str<-strsplit(str,split=',')
str
city<-sapply(str,function(x) x[1])
city
state<-sapply(str,function(x) x[2])
state

d1<-subset(d,select=-c('Location'))
View(d)

require(sqldf)

del<-sqldf('Select * from d where SeriousDlqin2yrs = 1')
View(del)

drop<-c('Location')
d<-d[,!names(d) %in% drop]
d %>% group_by(state) %>% top_n(n=10,wt=MonthlyIncome)


set.seed(123)
d <- data.frame(
  x   = runif(90),
  grp = gl(3, 30))

View(d)

d %>%
  group_by(grp) %>%
  top_n(n = 5, wt = x)


library(readr)

superheroes <- "
name, alignment, gender,         publisher
Magneto,       bad,   male,            Marvel
Storm,      good, female,            Marvel
Mystique,       bad, female,            Marvel
Batman,      good,   male,                DC
Joker,       bad,   male,                DC
Catwoman,       bad, female,                DC
Hellboy,      good,   male, Dark Horse Comics
"
superheroes <- read_csv(superheroes, trim_ws = TRUE, skip = 1)

publishers <- "
publisher, yr_founded
DC,       1934
Marvel,       1939
Image,       1992
"
publishers <- read_csv(publishers, trim_ws = TRUE, skip = 1)

View(superheroes)
View(publishers)

(ijsp <- inner_join(superheroes, publishers,by=c('publisher'='publisher')))
(ljsp <- left_join(superheroes, publishers,by=c('publisher'='publisher')))
(rjsp <- right_join(superheroes, publishers,by=c('publisher'='publisher')))
(sjsp <- semi_join(superheroes, publishers,by=c('publisher'='publisher')))
(ajsp <- anti_join(superheroes, publishers,by=c('publisher'='publisher')))
(fjsp <- full_join(superheroes, publishers,by=c('publisher'='publisher')))


x <- c(NA, 1:3, -1:1/0);
x
ran
range(x)
range(x, na.rm = TRUE)
range(x, na.rm=T, finite = TRUE)
x[1]


sapply(stores,function(x){sum(is.na(x))})
View(stores)

names(stores)[sapply(stores, FUN = is.numeric)]
names(stores)[sapply(stores,FUN = is.factor)]


install.packages('rmarkdown',dependencies = T)
install.packages('markdown',dependencies = T)
install.packages('knitr',dependencies = T)


Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- xmode[1]
  return(xmode)
}

xtab <- table(stores$StoreType)
xmode <- names(which(xtab == max(xtab)))
length(xmode)
xmode
cu_360 <- stores
for (var in 1:ncol(cu_360)) {
  if (class(cu_360[,var])=="numeric") {
    cu_360[is.na(cu_360[,var]),var] <- mean(cu_360[,var], na.rm = TRUE)
  } else if (class(cu_360[,var]) %in% c("character", "factor")) {
    cu_360[is.na(cu_360[,var]),var] <- Mode(cu_360[,var], na.rm = TRUE)
  }
}

View(cu_360)

boxplot(stores$ProfitPercust,col = 'blue')

fun <- function(x,na.rm=T){
  quantiles <- quantile( x, c(.25, .75 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

fun(stores$ProfitPercust)



x <- stores$ProfitPercust
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)  (qnt[2] + H)] <- caps[2]

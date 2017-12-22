
#Clearing Environment
rm(list = ls())

#Set working directory
setwd("E:/R-Programing/Proactive Attrition Management")
getwd()

#Import dataset
file <- file.choose()
data <- read.csv(file,header = T)


#Create user defined function for descriptive analysis
var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    return( c(Var_Type=Var_Type, n=n,nmiss=nmiss))
  }
}


d <- subset(data,select = -c(CALIBRAT,CHURNDEP))

var= sapply(d,is.numeric)
Othervar= !sapply(d,is.numeric)
View(Othervar)
stats <- data.frame(t(apply(d[var], 2, var_Summ)))
ostats <- data.frame(t(apply(d[Othervar], 2, var_Summ)))

View(stats)
#MissingValue Treatment
install.packages("Hmisc")
require(Hmisc)
d[var] <-data.frame(apply(d[var],2, function(x) impute(x, mean)))
d[d$INCOME==0,"INCOME"] <- 4.33
d[d$SETPRC==0,"SETPRC"] <- 35.79

nstats <- data.frame(t(apply(d[var], 2, var_Summ)))
View(nstats)
#Split variables according to their type
v <- subset(d, select = c( REVENUE, MOU, RECCHRGE, DIRECTAS, OVERAGE, ROAM, CHANGEM, CHANGER, DROPVCE, 
                           BLCKVCE, UNANSVCE, CUSTCARE, THREEWAY, MOUREC, OUTCALLS, INCALLS,  PEAKVCE, 
                           OPEAKVCE, DROPBLK,  CALLFWDV, CALLWAIT, MONTHS, UNIQSUBS, ACTVSUBS, PHONES,  
                           MODELS, EQPDAYS, CUSTOMER, AGE1, AGE2,  RETCALLS, RETACCPT,  REFER, INCOME, 
                           CREDITAD, SETPRC))

o <- subset(d, select = c(CSA,CHURN, CHILDREN, CREDITA, CREDITAA, CREDITB, CREDITC, CREDITDE,
                          CREDITGY, CREDITZ, PRIZMRUR, PRIZMUB, PRIZMTWN, REFURB,
                          WEBCAP, TRUCK, RV, OCCPROF, OCCCLER, OCCCRFT, OCCSTUD, OCCHMKR,
                          OCCRET, OCCSELF, OWNRENT, MARRYUN, MARRYYES, MARRYNO, MAILORD, 
                          MAILRES, MAILFLAG, TRAVEL, PCOWN, CREDITCD, NEWCELLY, NEWCELLN,
                          INCMISS, MCYCLE, SETPRCM, RETCALL))

#Create function for outlier treatment
M1_fun <- function(x){
  quantiles <- quantile( x, c(.01, .99 ),na.rm=TRUE )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

v_st <- data.frame(t(apply(v[], 2, var_Summ)))

vars <- c("REVENUE",  "MOU", "RECCHRGE", "DIRECTAS", "OVERAGE",  "ROAM" ,"CHANGEM",  "CHANGER", "DROPVCE",
          "BLCKVCE",  "UNANSVCE", "CUSTCARE", "THREEWAY", "MOUREC", "OUTCALLS", "INCALLS",  "PEAKVCE",
          "OPEAKVCE", "DROPBLK", "CALLWAIT", "MONTHS", "UNIQSUBS", "ACTVSUBS", "PHONES", "MODELS", "EQPDAYS",
          "CUSTOMER", "AGE1", "AGE2", "INCOME")

v[,vars] <-lapply(v[,vars],M1_fun)

v_st1 <- data.frame(t(apply(v[], 2, var_Summ)))

## FACTOR ANALYSIS 
corrm<- cor(v)                                 ### CORRELATION MATRIX

require(psych)
require(GPArotation)

### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT

eigen(corrm)$values                                                     ### EIGEN VALUES

require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 

write.csv(eigen_values, "E:/R-Programing/Proactive Attrition Management/EigenValuesp2.csv")  ### EXPORTING EIGEN VALUE SUMMARY

FA<-fa(r=corrm, 11, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
#FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
Loadings<-data.frame(FA_SORT$loadings[1:ncol(v),])           ### CAPTURING ONLY LOADINGS INTO DATA FRAME


vars2<- c("OPEAKVCE","INCALLS","UNANSVCE","CUSTCARE","THREEWAY","MODELS","EQPDAYS","SETPRC","MONTHS","REVENUE","DIRECTAS",
          "ROAM","ACTVSUBS","AGE1","INCOME","BLCKVCE","CHANGER","OVERAGE","DROPVCE","CALLWAIT")

v1 <- subset(v,select = vars2)



#Create Dummy variables
str(o)

o1 <- data.frame(lapply(o[,c("CSA","CHURN","CHILDREN","CREDITAA","CREDITB","CREDITC","CREDITDE","CREDITGY",
                             "CREDITZ","PRIZMTWN", "PRIZMUB","WEBCAP","TRUCK","RV","OCCPROF", 
                             "OCCCLER","OCCCRFT","OCCSTUD","OCCHMKR","OCCRET","OWNRENT","MARRYUN","MARRYYES","MAILRES",
                             "MAILFLAG","TRAVEL","PCOWN","CREDITCD","NEWCELLY","MCYCLE","RETCALL")], factor))


#Select unique categorical variable using Chi Square Test for Independency
tab <- xtabs(~CREDITAA+CREDITB, data = o1)
chisq.test(tab)



o2 <- o1[,c("CHURN","CREDITB","OCCSTUD","OCCHMKR","NEWCELLY","MCYCLE","RETCALL")]



#Merging all variables after reduction
nd <- cbind(v1,o2)

sq <- function(x) x^2       #function to square a variable

#Transforming Numerical Variables and selecting transformation with best correlation
nd$LOG_OPEAKVCE <- log(nd$OPEAKVCE)
nd$SQRT_OPEAKVCE <- sqrt(nd$OPEAKVCE)
nd$EXP_OPEAKVCE <- exp(nd$OPEAKVCE)
nd$SQ_OPEAKVCE <- sq(nd$OPEAKVCE)
nd$REC_OPEAKVCE <- 1/nd$OPEAKVCE
optab1 <- xtabs(~CHURN+OPEAKVCE, data = nd)
chisq.test(optab1)
optab2 <- xtabs(~CHURN+LOG_OPEAKVCE, data = nd)
chisq.test(optab2)
optab3 <- xtabs(~CHURN+EXP_OPEAKVCE, data = nd)
chisq.test(optab3)
optab4 <- xtabs(~CHURN+SQRT_OPEAKVCE, data = nd)
chisq.test(optab4)
optab5 <- xtabs(~CHURN+SQ_OPEAKVCE, data = nd)
chisq.test(optab5)
optab6 <- xtabs(~CHURN+REC_OPEAKVCE, data = nd)
chisq.test(optab6)

optab <- xtabs(~CHURN+REC_OPEAKVCE, data = nd)
chisq.test(optab)
chisq.test(optab, simulate.p.value = T)


nd$LOG_INCALLS <- log(nd$INCALLS)
nd$SQRT_INCALLS <- sqrt(nd$INCALLS)
nd$EXP_INCALLS <- exp(nd$INCALLS)
nd$SQ_INCALLS <- sq(nd$INCALLS)
nd$REC_INCALLS <- 1/nd$INCALLS

incatab <- xtabs(~CHURN+REC_INCALLS, data = nd)
chisq.test(incatab)
chisq.test(incatab, simulate.p.value = T)


nd$LOG_CALLWAIT <- log(nd$CALLWAIT)
nd$SQRT_CALLWAIT <- sqrt(nd$CALLWAIT)
nd$EXP_CALLWAIT <- exp(nd$CALLWAIT)
nd$SQ_CALLWAIT <- sq(nd$CALLWAIT)
nd$REC_CALLWAIT <- 1/nd$CALLWAIT

calltab <- xtabs(~CHURN+REC_CALLWAIT, data = nd)
chisq.test(calltab)
chisq.test(calltab, simulate.p.value = T)

nd$LOG_UNANSVCE <- log(nd$UNANSVCE)
nd$SQRT_UNANSVCE <- sqrt(nd$UNANSVCE)
nd$EXP_UNANSVCE <- exp(nd$UNANSVCE)
nd$SQ_UNANSVCE <- sq(nd$UNANSVCE)
nd$REC_UNANSVCE <- 1/nd$UNANSVCE

untab <- xtabs(~CHURN+REC_UNANSVCE, data = nd)
chisq.test(untab)
chisq.test(calltab, simulate.p.value = T)


nd$LOG_CUSTCARE <- log(nd$CUSTCARE)
nd$SQRT_CUSTCARE <- sqrt(nd$CUSTCARE)
nd$EXP_CUSTCARE <- exp(nd$CUSTCARE)
nd$SQ_CUSTCARE <- sq(nd$CUSTCARE)
nd$REC_CUSTCARE <- 1/nd$CUSTCARE

custab <- xtabs(~CHURN+REC_CUSTCARE, data = nd)
chisq.test(custab, simulate.p.value = T)

nd$LOG_THREEWAY <- log(nd$THREEWAY)
nd$SQRT_THREEWAY <- sqrt(nd$THREEWAY)
nd$EXP_THREEWAY <- exp(nd$THREEWAY)
nd$SQ_THREEWAY <- sq(nd$THREEWAY)
nd$REC_THREEWAY <- 1/nd$THREEWAY

thtab <- xtabs(~CHURN+REC_THREEWAY, data = nd)
chisq.test(thtab)

nd$LOG_MODELS <- log(nd$MODELS)
nd$SQRT_MODELS <- sqrt(nd$MODELS)
nd$EXP_MODELS <- exp(nd$MODELS)
nd$SQ_MODELS <- sq(nd$MODELS)
nd$REC_MODELS <- 1/nd$MODELS

modtab <- xtabs(~CHURN+REC_MODELS, data = nd)
chisq.test(modtab)
chisq.test(modtab, simulate.p.value = T)


nd$LOG_EQPDAYS <- log(nd$EQPDAYS)
nd$SQRT_EQPDAYS <- sqrt(nd$EQPDAYS)
nd$EXP_EQPDAYS <- exp(nd$EQPDAYS)
nd$SQ_EQPDAYS <- sq(nd$EQPDAYS)
nd$REC_EQPDAYS <- 1/nd$EQPDAYS

eqtab <- xtabs(~CHURN+REC_EQPDAYS, data = nd)
chisq.test(eqtab, simulate.p.value = T)

nd$LOG_SETPRC <- log(nd$SETPRC)
nd$SQRT_SETPRC <- sqrt(nd$SETPRC)
nd$EXP_SETPRC <- exp(nd$SETPRC)
nd$SQ_SETPRC <- sq(nd$SETPRC)
nd$REC_SETPRC <- 1/nd$SETPRC

setab <- xtabs(~CHURN+REC_SETPRC, data = nd)
chisq.test(setab, simulate.p.value = T)

nd$LOG_MONTHS <- log(nd$MONTHS)
nd$SQRT_MONTHS <- sqrt(nd$MONTHS)
nd$EXP_MONTHS <- exp(nd$MONTHS)
nd$SQ_MONTHS <- sq(nd$MONTHS)
nd$REC_MONTHS <- 1/nd$MONTHS

montab <- xtabs(~CHURN+REC_MONTHS, data = nd)
chisq.test(montab)

nd$LOG_REVENUE <- log(nd$REVENUE)
nd$SQRT_REVENUE <- sqrt(nd$REVENUE)
nd$EXP_REVENUE <- exp(nd$REVENUE)
nd$SQ_REVENUE <- sq(nd$REVENUE)
nd$REC_REVENUE <- 1/nd$REVENUE
##REVENUE Highest

revtab <- xtabs(~CHURN+REC_REVENUE, data = nd)
chisq.test(revtab)
chisq.test(revtab, simulate.p.value = T)

nd$LOG_DIRECTAS <- log(nd$DIRECTAS)
nd$SQRT_DIRECTAS <- sqrt(nd$DIRECTAS)
nd$EXP_DIRECTAS <- exp(nd$DIRECTAS)
nd$SQ_DIRECTAS <- sq(nd$DIRECTAS)
nd$REC_DIRECTAS <- 1/nd$DIRECTAS

dirtab <- xtabs(~CHURN+LOG_DIRECTAS, data = nd)
chisq.test(dirtab)
chisq.test(dirtab, simulate.p.value = T)

nd$LOG_ROAM <- log(nd$ROAM+0.1)
nd$SQRT_ROAM <- sqrt(nd$ROAM)
nd$EXP_ROAM <- exp(nd$ROAM)
nd$SQ_ROAM <- sq(nd$ROAM)
nd$REC_ROAM <- 1/nd$ROAM
##All same
rotab <- xtabs(~CHURN+REC_ROAM, data = nd)
chisq.test(rotab)
chisq.test(rotab, simulate.p.value = T)

nd$LOG_ACTVSUBS <- log(nd$ACTVSUBS)
nd$SQRT_ACTVSUBS <- sqrt(nd$ACTVSUBS)
nd$EXP_ACTVSUBS <- exp(nd$ACTVSUBS)
nd$SQ_ACTVSUBS <- sq(nd$ACTVSUBS)
nd$REC_ACTVSUBS <- 1/nd$ACTVSUBS

actab <- xtabs(~CHURN+REC_ACTVSUBS, data = nd)
chisq.test(actab)

nd$LOG_AGE1 <- log(nd$AGE1)
nd$SQRT_AGE1 <- sqrt(nd$AGE1)
nd$EXP_AGE1 <- exp(nd$AGE1)
nd$SQ_AGE1 <- sq(nd$AGE1)
nd$REC_AGE1 <- 1/nd$AGE1

agtab <- xtabs(~CHURN+REC_AGE1, data = nd)
chisq.test(agtab)

nd$LOG_INCOME <- log(nd$INCOME)
nd$SQRT_INCOME <- sqrt(nd$INCOME)
nd$EXP_INCOME <- exp(nd$INCOME)
nd$SQ_INCOME <- sq(nd$INCOME)
nd$REC_INCOME <- 1/nd$INCOME

intab <- xtabs(~CHURN+INCOME, data = nd)
chisq.test(intab)

nd$LOG_CHANGER <- log(abs(nd$CHANGER))# 
nd$SQRT_CHANGER <- sqrt(abs(nd$CHANGER))#
nd$EXP_CHANGER <- exp(nd$CHANGER)
nd$SQ_CHANGER <- sq(nd$CHANGER)#
nd$REC_CHANGER <- 1/nd$CHANGER

chtab <- xtabs(~CHURN+REC_CHANGER, data = nd)
chisq.test(chtab)
chisq.test(chtab, simulate.p.value = T)

nd$LOG_OVERAGE <- log(nd$OVERAGE)
nd$SQRT_OVERAGE <- sqrt(nd$OVERAGE)#
nd$EXP_OVERAGE <- exp(nd$OVERAGE)
nd$SQ_OVERAGE <- sq(nd$OVERAGE)
nd$REC_OVERAGE <- 1/nd$OVERAGE

ovtab <- xtabs(~CHURN+REC_OVERAGE, data = nd)
chisq.test(ovtab)
chisq.test(ovtab, simulate.p.value = T)

nd$LOG_DROPVCE <- log(nd$DROPVCE)
nd$SQRT_DROPVCE <- sqrt(nd$DROPVCE)
nd$EXP_DROPVCE <- exp(nd$DROPVCE)
nd$SQ_DROPVCE <- sq(nd$DROPVCE)
nd$REC_DROPVCE <- 1/nd$DROPVCE

drtab <- xtabs(~CHURN+REC_DROPVCE, data = nd)
chisq.test(drtab)
chisq.test(drtab, simulate.p.value = T)

nd1 <- subset(nd,select = c(OPEAKVCE,INCALLS,CALLWAIT,UNANSVCE,CUSTCARE,THREEWAY,MODELS,EQPDAYS,SETPRC,MONTHS,DIRECTAS,
                            ACTVSUBS,AGE1,INCOME,BLCKVCE,DROPVCE,CHURN,CREDITB,OCCSTUD,OCCHMKR,
                            NEWCELLY,MCYCLE,RETCALL,REVENUE,ROAM,SQ_CHANGER,SQRT_OVERAGE))


set.seed(123)

#Splitting data into Training, Validaton and Testing Dataset
t1 <- sample(1:nrow(nd1), size = floor(0.75 * nrow(nd1)))

train<-nd1[t1,]
test<-nd1[-t1,]


#Building Models for training dataset
source(file.choose())

fit<-glm(CHURN ~ . ,data = train, family = binomial(logit))
summary(fit)

###STEP-WISE MODELING
step1=step(fit)
summary(step1)
Concordance(step1)  #NOTE: To run these command, first run concordance function in Concordance.R 

fit1 <- glm(formula = CHURN ~ OPEAKVCE + INCALLS + CALLWAIT + CUSTCARE + 
              THREEWAY + MODELS + EQPDAYS + SETPRC + MONTHS + DIRECTAS + 
              ACTVSUBS + AGE1 + BLCKVCE + DROPVCE + CREDITB + NEWCELLY + 
              MCYCLE + RETCALL + REVENUE + ROAM + SQ_CHANGER + 
              SQRT_OVERAGE, family = binomial(logit), data = train)

summary(fit1)
Concordance(fit1)
require(car)
vif(fit1)

###BACKWARD MODELING
fit2 <- glm(formula = CHURN ~ INCALLS + CALLWAIT + CUSTCARE + UNANSVCE +
              THREEWAY + EQPDAYS + SETPRC + MONTHS + DIRECTAS +
              ACTVSUBS + AGE1 + BLCKVCE + DROPVCE + CREDITB + NEWCELLY +
              MCYCLE + RETCALL + REVENUE + ROAM + SQ_CHANGER +
              SQRT_OVERAGE, family = binomial(logit), data = train)
summary(fit2)
Concordance(fit2)
vif(fit2)


###FORWARD MODELING
fit3 <- glm(formula = CHURN~ INCALLS + CUSTCARE + THREEWAY + EQPDAYS + SETPRC + MONTHS + 
              ACTVSUBS + AGE1 + BLCKVCE + DROPVCE + CREDITB + NEWCELLY +
              MCYCLE + RETCALL + REVENUE + ROAM + SQ_CHANGER + SQRT_OVERAGE, family = binomial(logit), data = train)
summary(fit3)
Concordance(fit3)
vif(fit3)

################################VALIDATION ##############################
#Decile Scoring for 
##Training dataset
train1<- cbind(train, Prob=predict(fit1, type="response")) 
View(train1)

##Creating Deciles
decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))
View(train1)
require(dplyr)
train1$decile<-factor(train1$decile)
decile_grp<-group_by(train1,decile)
decile_summ_train<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=Prob), max_prob=max(Prob), churn_cnt=sum(as.numeric(levels(CHURN))[CHURN]), 
                             non_churn_cnt=total_cnt -churn_cnt )
decile_summ_train<-arrange(decile_summ_train, desc(decile))
View(decile_summ_train)

write.csv(decile_summ_train,"fit1_train_DA1_P2.csv",row.names = F)

##Testing dataset
test1<- cbind(test, Prob=predict(fit1,test, type="response")) 

View(test1)

##Creating Deciles
decLocations1 <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations1, Inf))
names(test1)

test1$decile<-factor(test1$decile)
test_decile_grp<-group_by(test1,decile)
decile_summ_test<-summarize(test_decile_grp, total_cnt=n(), min_prob=min(p=Prob), max_prob=max(Prob), churn_cnt=sum(as.numeric(levels(CHURN))[CHURN]), 
                            non_churn_cnt=total_cnt -churn_cnt )
decile_summ_test<-arrange(decile_summ_test, desc(decile))
View(decile_summ_test)


write.csv(decile_summ_test,"fit1_test_DA1_P2.csv",row.names = F)

#########




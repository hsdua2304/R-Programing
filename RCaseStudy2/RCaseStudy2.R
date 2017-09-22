# CASE STUDY 2
# ------------


# PROBLEM:1
# ---------
# IMPORT CUST_DATA AND CUST_DEMO

cust_data<-read.csv(choose.files(),header = T)
cust_demo<-read.csv(choose.files(),header = T)
View(cust_data)
View(cust_demo)
# PROBLEM:2
# ---------
# EXPLORE THE TWO DATASETS USING DIFFRENT FUNCTIONS
# USER DEFINED FUNCTION TO COMBINE ALL FUNCTION INTO SINGLE FUNCTION NAME
explore_dataset<-function(x){
  require(psych)
  v<-View(x)
  h<-head(x)
  t<-tail(x)
  s<-str(x)
  n<-names(x)
  nr<-nrow(x)
  nc<-ncol(x)
  sum<-summary(x)
  dup<-sum(duplicated(x))
  des<-describe(x)

  return(list(view=v,first6=h,last6=t,structure=s,columnNames=n,numOfCol=nc,numOfRow=nr,
         summary=sum,duplicates=dup,description=des))
}

# EXPLORING CUST_DATA DATASET
cust_data_explore<-explore_dataset(cust_data)
cust_data_explore$first6
cust_data_explore$last6
cust_data_explore$structure
cust_data_explore$columnNames
cust_data_explore$numOfCol
cust_data_explore$numOfRow
cust_data_explore$duplicates
cust_data_explore$summary
cust_data_explore$description

# EXPLORING CUST_DEMO DATASET
cust_demo_explore<-explore_dataset(cust_demo)
cust_demo_explore$first6
cust_demo_explore$last6
cust_demo_explore$structure
cust_data_explore$columnNames
cust_demo_explore$numOfCol
cust_demo_explore$numOfRow
cust_demo_explore$duplicates
cust_demo_explore$summary
cust_demo_explore$description


# PROBLEM:3
# ---------

customer_360<-merge(cust_data,cust_demo,by.x = c('ID'),by.y = c('ID'),all.x = T)
View(customer_360)
# str(customer_360)


# PROBLEM:4
# ---------

# Conversion of #NA's to NA

customer_360$NumberOfDependents[customer_360$NumberOfDependents=='#N/A']<-NA
customer_360$MonthlyIncome[customer_360$MonthlyIncome=='#N/A']<-NA

customer_360$MonthlyIncome<-as.numeric(as.character(customer_360$MonthlyIncome))
customer_360$NumberOfDependents<-as.numeric(as.character(customer_360$NumberOfDependents))


# Dropping the rows have NA's in the list
# customer_360<-customer_360[!(is.na(customer_360$MonthlyIncome) | is.na(customer_360$NumberOfDependents)),]

View(customer_360)
# str(customer_360)


# PROBLEM:5
# ---------

# Sorting the customer_360 dataset according to monthly income
customer_360<-arrange(customer_360,MonthlyIncome)

# User defined function to calculate the percentile score of each customer
# based on the monthly income
percentile<-function(a){
  vector<-list(which(a!=-1,T))
  l<-vector('list',length(a))
  for(i in vector){
    l[i]<-(i/length(a))*100
  }
  return(l)
}

# Calculating the Percentile score based on monthly income
customer_360$percentile_score<-as.numeric(as.character(percentile(customer_360$MonthlyIncome)))
customer_360$percentile_score[customer_360$percentile_score=='#N/A']<-NA

# Binning the monthly income based on the percentile score calculated for each customer
customer_360$Income_segment<-cut(customer_360$percentile_score,breaks = c(-Inf,20,40,60,80,Inf),
                                 labels = c('Very Low Value Customers','Low Value Customers',
                                            'Medium Value Customers','High Value Customers',
                                            'Prime Value Customers'))

View(customer_360)


# PROBLEM:6
# ---------

customer_360$Life_Stage_Segment<-cut(customer_360$age,breaks = c(-Inf,22,27,40,60,Inf),
                                     labels = c('Student','UnMarried_employed',
                                                'Married_with_youndKids','Pre-retired',
                                                'Retired'))
# View(customer_360)

# PROBLEM:7
# ---------

# Converting Location as character variable so we can split the location based on comma.
Location<-as.character(customer_360$Location)
Location<-strsplit(Location,',')

# Storing the splitted values in city and state variable respectively
customer_360$City<-sapply(Location,function(x) x[1])
customer_360$State<-sapply(Location,function(x) x[2])

# Dropping the location variable from the dataset.
drop<-c('Location')
customer_360<-customer_360[,!names(customer_360) %in% drop]

View(customer_360)


# PROBLEM:8
# ---------

# Dividing the Dataset into delinquent and non-delinquent datasets
require(sqldf)
delinquent<-sqldf('Select * from customer_360  where SeriousDlqin2yrs = 1')
non_delinquent<-sqldf('Select * from customer_360  where SeriousDlqin2yrs = 0')

write.csv(delinquent,file = 'E:/R-Programing/RCaseStudy2/delinquent.csv')
write.csv(non_delinquent,file = 'E:/R-Programing/RCaseStudy2/non_delinquent.csv')


# PROBLEM:9
# ---------

Top_50_Customers<-top_n(customer_360,50,MonthlyIncome)
View(Top_50_Customers)


# PROBLEM:10
# ----------

Top_10_Customer<-arrange(customer_360,desc(State),desc(MonthlyIncome)) %>% group_by(State) %>% top_n(n=10,wt=MonthlyIncome)
View(Top_10_Customer)


# PROBLEM : 11
#-------------

customer_360<-mutate(customer_360,Gender=as.factor(Gender),City=as.factor(City),
                     no_of_30_120_DPD=No_of_30_59_DPD+No_of_90_DPD+No_of_60_89_DPD)

cust_group<-group_by(customer_360,City,Gender)

summary_report = summarise(cust_group,Percent_Delinquency=(sum(no_of_30_120_DPD,na.rm = T)/sum(customer_360$no_of_30_120_DPD,na.rm = T))*100,
                           Average_Dependents=mean(NumberOfDependents,na.rm = T),
                           Average_Debt_Ratio=mean(DebtRatio,na.rm = T),
                           Min_Monthly_Income=range.default(MonthlyIncome,na.rm = T)[1],
                           Max_Monthly_Income=range.default(MonthlyIncome,na.rm = T)[2],
                           Min_Age=range.default(age,na.rm = T)[1],
                           Max_Age=range.default(age,na.rm = T)[2])

View(summary_report)

# PROBLEM : 12
#-------------

# Removing rows having duplicate ID's
customer_360<-customer_360[unique(customer_360$ID),]

str(customer_360)

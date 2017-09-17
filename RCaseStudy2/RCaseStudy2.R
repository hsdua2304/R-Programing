# CASE STUDY 2
# ------------


# PROBLEM:1
# ---------
# IMPORT CUST_DATA AND CUST_DEMO

cust_data<-read.csv(choose.files(),header = T)
cust_demo<-read.csv(choose.files(),header = T)

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
str(customer_360)


# PROBLEM:4
# ---------

customer_360$MonthlyIncome<-as.numeric(as.character(customer_360$MonthlyIncome))
customer_360$NumberOfDependents<-as.numeric(as.character(customer_360$NumberOfDependents))
str(customer_360)


# PROBLEM:5
# ---------


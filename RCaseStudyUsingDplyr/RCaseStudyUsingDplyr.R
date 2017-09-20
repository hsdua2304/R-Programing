# Case Study : Data Manipulation Using DPLYR package

library(dplyr)
cust_data<-read.csv('E:/R-Programing/RCaseStudyUsingDplyr/Cust_data.csv',header = T)
cust_demo<-read.csv('E:/R-Programing/RCaseStudyUsingDplyr/Cust_demo.csv',header = T)
cust_new<-read.csv('E:/R-Programing/RCaseStudyUsingDplyr/cust_new.csv',header = T)


str(cust_demo)
# Problem : 1
#------------

cust_s1 <- cust_demo %>% select(ID,age,Gender,Location,Martial_Status,Own_House,NumberOfDependents) %>% filter(Martial_Status == 'Married' & Own_House==1 & age > 28)
View(cust_s1)

# Problem : 2
#------------

cust_s1<- cust_s1 %>% arrange(Location,desc(age))
View(cust_s1)

# Problem : 3
#------------

cust_s1<-rename(cust_s1,Number_of_dependents = NumberOfDependents)
View(cust_s1)


# Problem : 4
#-----------

cust_data<-distinct(cust_data)
cust_demo<-distinct(cust_demo)

# Problem : 5
#------------

cust_data<-mutate(cust_data,No_of_30_Plus_DPD = No_of_30_59_DPD+No_of_90_DPD+No_of_60_89_DPD,
                  No_of_60_Plus_DPD=No_of_90_DPD+No_of_60_89_DPD)

new_vars<-transmute(cust_data,No_of_30_Plus_DPD = No_of_30_59_DPD+No_of_90_DPD+No_of_60_89_DPD,
                    No_of_60_Plus_DPD=No_of_90_DPD+No_of_60_89_DPD)

# Problem : 6
#------------

cust_leftjoin<-left_join(cust_data,cust_demo,by=c('ID'='ID'))
cust_rightjoin<-right_join(cust_data,cust_demo,by=c('ID'='ID'))
cust_innerjoin<-inner_join(cust_data,cust_demo,by=c('ID'='ID'))
cust_fulljoin<-full_join(cust_data,cust_demo,by=c('ID'='ID'))
cust_semijoin<-semi_join(cust_data,cust_demo,by=c('ID'='ID'))
cust_antijoin<-semi_join(cust_data,cust_demo,by=c('ID'='ID'))


# Problem : 7
#------------

cust_union<-union(cust_demo,cust_new)
cust_intersect<-intersect(cust_demo,cust_new)
cust_x_not_y<-setdiff(cust_demo,cust_new)
cust_y_not_x<-setdiff(cust_new,cust_demo)
cust_demo_new<-bind_rows(cust_demo,cust_y_not_x)


# Problem : 8
#------------

sample_dataset1<-sample_frac(cust_demo,0.05,replace = T)
sample_dataset2<-sample_n(cust_demo,10000,replace = T)

View(sample_dataset1)

# Problem : 9
#------------

(count(cust_leftjoin,Martial_Status))

# Problem : 10
#-------------


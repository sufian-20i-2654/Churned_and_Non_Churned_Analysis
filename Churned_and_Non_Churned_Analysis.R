library(dplyr)
library(readr)
library(data.table)
library(lubridate)
library(ggplot2)
library(lattice)
dataset<-read.csv("E:/20i_2654_Detailed_Dataset.csv", na.strings = c(""))
data_1 <-dataset

#Q1
#Does the purchase amount of the customers increase over the period of time?
#H0=mean_2-mean_1=0, Ha: mean_2-maen_1 !=0
mean <- c(mean(data_1$w1_total_sales),mean(data_1$w2_total_sales),mean(data_1$w3_total_sales),mean(data_1$w4_total_sales),mean(data_1$w5_total_sales))

barplot(mean,xlab = 'Number of weeks',ylab='Total Sales',)
#the graph indicates the increase in the average sales (from week 1 - week 5)
t.test(data_1$w5_total_sales,data_1$w1_total_sales,paired=TRUE)
#since the p-value is lesser than the alpha value of 0.05 so we will reject the null hypothesis

#Q2
#We believe rich people are prone to churn as compared to poor customers. Verify.
#Ho: label and status are independent
#Ha: label and status are dependent

quantile_1<-quantile((dataset$total_revenue),0.25)
quantile_2<-quantile((dataset$total_revenue),0.5)
quantile_3<-quantile((dataset$total_revenue),0.75)
quantile_4<-quantile((dataset$total_revenue),1)

dataset<-dataset %>%
  mutate(Category  = case_when(
    dataset$total_revenue < quantile_1 ~ "Poor",
    dataset$total_revenue > quantile_1 & dataset$total_revenue < quantile_3 ~ "Mediocre",
    dataset$total_revenue > quantile_3 ~ "Rich"
  ))
churned<-data.frame(filter(dataset,label=="churned"))
table_1<-table(churned$label,churned$Category)
barplot(table_1)
chisq.test(table_1)
#the P value is less than the alpha value so we reject null hypothesis so the status and label are dependent

#Q3
#We believe that the churned customers visit the stores occasionally. Verify.
mean_2 <-mean(dataset$visits)
mean_2
#Ho=mean_2>5 , Ha=mean_2<5

mean(churned$visits)
sd(churned$visits)
hist(churned$visits)
t.test(churned$visits,mu=5,alternative = 'greater')
#the p-value is equal to 1 so we accept that churned customers visit the store occasionally

#Q4
#Overall spending of churned customers is significantly lesser than the non-churned customers.Verify.
#Ho= overall spending of churned customers are greater than or equal then spending non-churned customers
#Ha= overall spending of churned customers is less than overall spending of non-churned customers

churned_1<-dataset$total_revenue[dataset$label=='churned']
non_churned<-dataset$total_revenue[dataset$label=='not churned']

barplot(churned_1)
barplot(non_churned)
t.test(churned_1,non_churned,alternative = 'less')
#the p value is less than the alpha value so we reject the null hypothesis, overall spending of churned customers is less than non churned customers

#Q5
data_3<-dataset
ml <- lm(total_revenue ~ label, data=data_3)
summary (ml)
coef(ml)
predicted_1<-fitted.values(ml)
residuals_1<-residuals(ml)
qplot(predicted_1,residuals_1)+geom_point()+geom_smooth(method="lm",se=F)
predict(ml,newdata=dataset)

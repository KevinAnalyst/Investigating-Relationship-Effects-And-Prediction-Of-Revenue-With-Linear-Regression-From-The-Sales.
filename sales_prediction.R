#The purpose of the studying is to investigate the association $ effects and prediction of Revenue From Sales_data with Linear Regression
---------------
# Loading the data in two way;by copying the path or use file choosing format
 #Reading the path by copying the path  
pd=read.csv('F:\\Sales_predictions\\sales_data.csv')
 #Reading the path using file.choose
pd=read.csv(file.choose())
#Checking the first 1,2,3,4,5,,,,, but default is first five
head(pd)
#Checking the  last1,2,3,4,5,,,,, but default is last five
tail(pd)
#Checking the variables names
names(pd)
#Check the variable type
str(pd)
#Checking columns
ncol(pd)
#checking rows
nrow(pd)
#To determine if there is missing values in between variables
sum(is.na(pd))# In general
sum(is.na(pd$Product))
sum(is.na(pd$Order_Quantity))
#We are going to conduct descriptive analysis that is central tendency and dispersion
summary(pd)
# we will need to select relevant variable for our analysis by loading tidyverse and dplyr
library(tidyverse)
library(dplyr)
#selecting the variable required leaving others
pd1=select(pd,Order_Quantity,Unit_Cost,Unit_Price,Cost,Revenue)
#Checking the associations
cor(pd1)
# checking again the summary statistics
summary(pd1)

# Conducting EDA through visualization
hist(pd1$Revenue,col = c(1,2),main = 'Frequency graph for Revenue')
hist(pd1$Cost,col = c(1,2,3,4,6),main = 'Frequency graph for Cost')
hist(pd1$Unit_Price,col = c(1,2,3,4,5,6,7,8,9),main = 'Frequency graph for unit_price')
hist(pd1$Unit_Cost,col = c(1,2,3,4,5,6,7,8,9,10),main = 'Frequency graph for Unit cost')
hist(pd1$Order_Quantity,col = c(1,2,3,4,5,6,7,8,9,10),main = 'Frequency graph for Order_Quantity')
# Here we are going to utilize regression analysis in fitting the model
fit=lm(pd1$Revenue~pd1$Order_Quantity+pd1$Unit_Cost+pd1$Unit_Price+pd1$Cost)
#Viewing the intercepts
fit
#To check all regressions coefficient
summary(fit)
# Running Analysis of variance
anova(fit)







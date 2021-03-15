#loading Packages
library(tidyverse)
library(readxl)
library(tidyr)
library(tree)
library(ranger)
library(mgcv)
library(vtreat)

#Importing Dataset
car_data<-read_csv(file.choose())

#Data Info
head(car_data)
summary(car_data)
dim(car_data)
str(car_data)
names(car_data)
glimpse(car_data)
colSums(is.na(car_data))

#Remove rows containing NA values
#train<-na.omit(car_data)
#dim(car_data)

#Remove/drop Unnessesary columns 
car_data$Car_Name<-NULL
car_data$Owner<-NULL

#Data Manipulation(Creating a New column with help of present coluumns)
car_data$Current_Year=2021
car_data$no_year=car_data$Current_Year-car_data$Year

car_data$Year=NULL
car_data$Current_Year=NULL

#Data_Visualization
ggplot(data = car_data,aes(x=Kms_Driven,y=Selling_Price))+geom_point()
ggplot(data = car_data,aes(x=no_year,y=Selling_Price))+geom_point()
#From this two point graphs that we can say that Kms_Driven and no of years are inversely Proportional to Selling Price of Car

ggplot(data = car_data,aes(x=Fuel_Type,y=Selling_Price))+geom_boxplot()
#From this boxplot we can conclude that Cars having Fuel_Type Diesel have higher Selling Price.

#Conversion of Categorical variables with N levels into N-1 indicater variables
mmat=as.data.frame(model.matrix(fmla,car_data))
View(mmat)

#Adding columns contaning indicator variables to Original Dataset
car_data$Fuel_TypeDiesel=mmat$Fuel_TypeDiesel
car_data$Fuel_TypePetrol=mmat$Fuel_TypePetrol
car_data$Seller_TypeIndividual=mmat$Seller_TypeIndividual
car_data$TransmissionManual=mmat$TransmissionManual

#Removing Categorical Columns
car_data$Fuel_Type=NULL
car_data$Seller_Type=NULL
car_data$Transmission=NULL

#Expressing Dependent Variable in terms of Independent Variables
fmla=Selling_Price~Present_Price+Kms_Driven+no_year+Fuel_TypeDiesel+Fuel_TypePetrol+Seller_TypeIndividual+TransmissionManual

model=lm(fmla,car_data)
#model=ranger(fmla,car_data,num.trees=500,respect.unordered.factors = "order")
#model=gam(fmla,family=gaussian(),car_data)

car_data$Predictions<-0
splitplan<-kWayCrossValidation(nrow(car_data),3,NULL,NULL)
for(i in 1:3)
{
  split<-splitplan[[i]]
  model=lm(fmla,data=car_data[split$train,])
  car_data$Predictions[split$app]=predict(model,newdata = car_data[split$app,])
}


summary(model)
#R-squared=0.8995
#p-value: < 2.2e-16


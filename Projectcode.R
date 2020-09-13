library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)
library(ggplot2)
#Objective:Prediction of Customer Lifetime Values(CLV) and examination of the factors that influence it

#Setting up the working directory
Path<-"C:/Users/SREEJITA BHAUMIK/Desktop/ivy/Stats+r/r/IVY--R PREDICTIVE ANALYTICS -- RESOURCES/Final R Project_IVY"
setwd(Path)
getwd()
Project=read.csv("Fn-UseC_-Marketing-Customer-Value-Analysis.csv")
data1=Project #creating a back up of original data
options(scipen = 100)
data1$Number.of.Open.Complaints=as.factor(data1$Number.of.Open.Complaints)
data1$Number.of.Policies=as.factor(data1$Number.of.Policies)


#Exploration of the data 
str(data1)
summary(data1)
dim(data1)


#Renaming the Dependent var
colnames(data1)[which(names(data1)=="Customer.Lifetime.Value")]="clv"




#Outlier Treatment through quantile method
#For clv

quantile(data1$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))

data2=data1[data1$clv <36000,]

nrow(data1)

nrow(data2)

nrow(data1)-nrow(data2)

quantile(data2$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))



data3=data2[data2$clv <14722, ]

nrow(data3)

quantile(data3$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))

data4=data3[data3$clv <10261, ]

quantile(data4$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))

data5=data4[data4$clv <9119, ]

quantile(data5$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))

#For Monthly Premium Auto
quantile(data5$Monthly.Premium.Auto,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))

data6=data5[data5$Monthly.Premium.Auto <140, ]
quantile(data6$Monthly.Premium.Auto,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
summary(data6)
#For Income
quantile(data6$Income,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
data7=data6[data6$Income <82412, ]
quantile(data6$Income,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
summary(data7)



#Missing values Identification and Treatment
as.data.frame(colSums(is.na(data7)))




#Droping the rendundant variables from the data frame
as.data.frame(colnames(data7))
data10=select(data7,-c(Customer,State,Effective.To.Date))

str(data10)#final dataset for modelling


#Exporting the treated data into csv file

write.csv(data10,"Projectdata.csv")



#Splitting the data into training and test data set

set.seed(123)

spl = sample.split(data10$clv, 0.7)

train.data = subset(data10, spl == TRUE)
str(train.data)
dim(train.data)

test.data = subset(data10, spl == FALSE)
str(test.data)
dim(test.data)


#Fitting the model
#Iterations

LinearModel0=lm(clv~.,data=train.data)
summary(LinearModel0)


LinearModel=lm(clv~	Coverage+	Education+	EmploymentStatus+	Income+	Gender+Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+	Months.Since.Policy.Inception+	Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+Total.Claim.Amount+Vehicle.Class+	Vehicle.Size, data=train.data)
summary(LinearModel)
LinearModel=lm(clv~	Coverage+	Education+	EmploymentStatus+	Income+	Gender+Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+	Months.Since.Policy.Inception+	Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+Total.Claim.Amount, data=train.data)
summary(LinearModel)
LinearModel=lm(clv~	Coverage+	Education+	EmploymentStatus+	Income+	Gender+Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+	Months.Since.Policy.Inception+	Number.of.Open.Complaints+	Number.of.Policies+Total.Claim.Amount, data=train.data)
summary(LinearModel)
LinearModel=lm(clv~	Coverage+	Education+	EmploymentStatus+	Income+	Gender+Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+Number.of.Open.Complaints+	Number.of.Policies+Total.Claim.Amount, data=train.data)
summary(LinearModel)
LinearModel=lm(clv~	Coverage+	I(Education=="Doctor")+(Education=="High School or Below")+(Education=="Master")+	EmploymentStatus+	Income+	Gender+Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+Number.of.Open.Complaints+	Number.of.Policies+Total.Claim.Amount, data=train.data)
summary(LinearModel)
LinearModel=lm(clv~	Coverage+	I(Education=="Doctor")+(Education=="High School or Below")+(Education=="Master")+	I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Retired")+I(EmploymentStatus=="Unemployed")+	Income+	Gender+Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+Number.of.Open.Complaints+	Number.of.Policies+Total.Claim.Amount, data=train.data)
summary(LinearModel)



## Get the predicted or fitted values
fitted(LinearModel)

par(mfrow=c(2,2))
plot(LinearModel)



## MAPE
train.data$pred <- fitted(LinearModel)
write.csv(train.data,"mape.csv")


#Calculating MAPE
attach(train.data)
MAPE<-print((sum((abs(clv-pred))/clv))/nrow(train.data))

# Checking multicollinearity
vif(LinearModel) # should be within 2. If it is greater than 10 then serious problem


#Autocorrelation test
durbinWatsonTest(LinearModel)
#Since, the p-value is >0.05, we fail to reject H0: (No Autocorrelation)



#Heteroscedasticity: Breusch-Pagan test
bptest(LinearModel)  # Null hypothesis -> error is non-homogenious (p value should be more than 0.05)


## Normality testing Null hypothesis is data is normal.

resids <- LinearModel$residuals


ad.test(resids) #get Anderson-Darling test for normality 


# Testing the model on test data 
test=lm(clv~	Coverage+	I(Education=="Doctor")+(Education=="High School or Below")+(Education=="Master")+	I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Retired")+I(EmploymentStatus=="Unemployed")+	Income+	Gender+Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+Number.of.Open.Complaints+	Number.of.Policies+Total.Claim.Amount, data=test.data)
summary(test)

test=lm(clv~	Coverage+	I(Education=="Doctor")+(Education=="High School or Below")+(Education=="Master")+	I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Retired")+I(EmploymentStatus=="Unemployed")+	Income+	Gender+Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+Number.of.Open.Complaints+	Number.of.Policies, data=test.data)
summary(test)

test=lm(clv~	Coverage+	I(Education=="Doctor")+(Education=="High School or Below")+(Education=="Master")+	I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Retired")+I(EmploymentStatus=="Unemployed")+	Income+	Gender+Marital.Status+	Monthly.Premium.Auto+Number.of.Open.Complaints+	Number.of.Policies, data=test.data)
summary(test)
test=lm(clv~	I(Coverage=="Premium")+	I(Education=="Doctor")+(Education=="High School or Below")+(Education=="Master")+	I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Retired")+I(EmploymentStatus=="Unemployed")+	Income+	Gender+Marital.Status+	Monthly.Premium.Auto+Number.of.Open.Complaints+	Number.of.Policies, data=test.data)
summary(test)
test=lm(clv~	I(Coverage=="Premium")+	I(Education=="Doctor")+(Education=="High School or Below")+(Education=="Master")+	I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Unemployed")+	Income+	Gender+Marital.Status+	Monthly.Premium.Auto+Number.of.Open.Complaints+	Number.of.Policies, data=test.data)
summary(test)
summary(test.data)
## Get the predicted or fitted values
fitted(test)
par(mfrow=c(2,2))
plot(test)

## MAPE
test.data$pred <- fitted(test)
write.csv(test.data,"mapetest.csv")

#Calculating MAPE
attach(test.data)
(sum((abs(clv-pred))/clv))/nrow(test.data)

#Check Vif, vif>2 means presence of multicollinearity
vif(test)

#Autocorrelation
durbinWatsonTest(test)
dwt(test)

#Heteroscedasticity


# Breusch-Pagan test
bptest(test)  # Null hypothesis -> error is homogenious (p value should be more than 0.05)





## Normality testing Null hypothesis is data is normal.

resids1 <- test$residuals


ad.test(resids1) #get Anderson-Darling test for normality 

#Coefficient
coefftest<-as.data.frame(coefficients(test))

#Visualization
Customer.Lifetime.Value<-data1$clv
hist(Customer.Lifetime.Value,col="sky blue")

Mon.pre_plot<-ggplot(test.data,aes(Monthly.Premium.Auto,clv))
Mon.pre_plot+geom_point(col="skyblue")+theme_minimal()


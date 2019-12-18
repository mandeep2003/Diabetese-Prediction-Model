data_diabetes<-read.csv("D://Old Laptop/1 PG in Business Analytics XLRI/Study Material/Regression Technique & Forcasting/End Term Project/diabetes2.csv")
#Summary for individual variable to copy paste in report.
ummary(data_diabetes$Pregnancies)
summary(data_diabetes$Glucose)
summary(data_diabetes$BloodPressure)
summary(data_diabetes$SkinThickness)
summary(data_diabetes$Insulin)
summary(data_diabetes$BMI)
summary(data_diabetes$DiabetesPedigreeFunction)
summary(data_diabetes$Age)
str(data_diabetes)
data.frame(data_diabetes)

#Check if any data has null, NA, N/A, values
any(is.null(data_diabetes)) # no nullvalues in data
any(is.na(data_diabetes)) # no na in data.

class(data_diabetes$Outcome)

cor(data_diabetes)
#Output does not hint multicollinearity 

#Create a copy of data to prepare for regression
dc<-data_diabetes
class(dc)
str(dc)
dc$Outcome <- as.factor(dc$Outcome) #convert Numeric to Factor for Outcome variable.

library(ggplot2)
library(tidyverse)

mean_bp<-round(mean(dc$BloodPressure), digits = 2)
dc$BloodPressure<-ifelse(dc$BloodPressure == 0, mean_bp, dc$BloodPressure)
#check if values replaced correctly
tb<-cbind(dc$BloodPressure,data_diabetes$BloodPressure)

#Plot the data.
plot(dc)

library(corrplot)
corrplot(cor(dc[,-9]),method="number")

# To prepare train and test data.
library(caret)
set.seed(100)
dc[,]
train=dc
train=createDataPartition(dc$Outcome, times = 1, p = .7, list = FALSE)
class(train)
?createDataPartition()
dc.train=dc[train,]
dc.test=dc[-train,]
nrow(dc.train)
nrow(dc.test)
#Regression model
#reg.mod=glm(dc.train$Outcome~dc.train$Pregnancies+dc.train$Glucose+dc.train$BloodPressure
#              +dc.train$SkinThickness+dc.train$Insulin+dc.train$BMI+dc.train$DiabetesPedigreeFunction
 #             +dc.train$Age, family = binomial(link = "logit"))
reg.mod=glm(dc.train$Outcome~., data=dc.train, family = binomial(link = "logit"))
summary(reg.mod)
#We can ignore below variables as p <<< .05
#BloodPressure
#SkinThickness
#Insulin
#Age
#DiabetesPedigreeFunction
# New model after ignomring above variables - 
#reg.mod1=glm(dc.train$Outcome~dc.train$Pregnancies+dc.train$Glucose
#            +dc.train$BMI, family = binomial(link = "logit"), data = dc.train)
#Above gives vector issue thats changing to below format 
reg.mod1=glm(Outcome~Pregnancies+Glucose
             +BMI, family = binomial(link = "logit"), data = dc.train)
summary(reg.mod1)

#Check for model fitness: 77.13%
pr<-predict(reg.mod1, type = "response")

dclc<-data.frame(dc.train,pr)
head(dclc)
lc<-ifelse(pr>=.5,1,0)
head(lc)
tb<-table(dc.train$Outcome,lc)
tb
library(car)
vif(reg.mod1)

#Predict the values for test data:
pr.test<-predict(reg.mod1, newdata = dc.test,  type = "response")
pr.test.db <-ifelse(pr.test>=.5,1,0)
head(pr.test.db)
tb.test.db<-table(dc.test$Outcome,pr.test.db)
tb.test.db

setwd("E:/AMIT_IITK/SEM6/CE775/Ass2")

library(tidyverse)
library(lmtest)
library(xtable)###For exporting the output of regression summary, 
library(stargazer)
library(reporttools)
#Import data
pedvars <- read.csv("ped_exposure_data_sample.csv")

## summarize numeric variables for LaTeX
# tableContinuous(pedvars[,sapply(pedvars, is.numeric)],
#                 stats = c("min", "q1", "median", "mean", "q3", "max"))

colnames(pedvars)

# get training and test data (80-20 split)
n<-nrow(pedvars)
set.seed(12345) ##changing the seed will change the training/test data sets
index <- sample(n, round(n*0.8))
train <- pedvars[index,] #80% of data
test <- pedvars[-index,] #20% of data

##########Initial models#########
#Modeling AnnualEst#########
model_1 <- lm(AnnualEst~PopT,
              data=train)
summary(model_1)


# ##Exporting model output using "xtable"
# print(xtable(model_1),file="model_AADT.html",type="html")
print(xtable(model_1),digits=2) #default settings provide output for LaTeX

##Exporting model output using "stargazer"
stargazer(model_1,out="model_AADT.html",type="html",digits=2)
stargazer(model_1,out="model_AADT.html",digits=2) #default settings provide output for LaTeX



###Modeling logAnnualEst#########
model_2 <- lm(logAnnualEst~PopT,
          data=train)
summary(model_2)


##Exporting model output using "xtable"
print(xtable(model_2),file="model_logAADT.html",type="html")
print(xtable(model_2,digits=2)) #default settings provide output for LaTeX


##Exporting model output using "stargazer"
stargazer(model_2,out="model_logAADT.html",type="html",digits=2)
stargazer(model_2,out="model_logAADT.html",digits=2) #default settings provide output for LaTeX



####model 1 goodness-of-Fit
logLik(model_1)
AIC(model_1)
BIC(model_1)

#model 1: Prediction on test data
AnnualEst_pred_model_1<-predict(model_1,test)

#model 1: training RMSE
model_1_rmse_train<-(sum(model_1$residuals^2)/nrow(train))^0.5


#model 1: test RMSE
model_1_rmse_test<-(sum((test$AnnualEst-AnnualEst_pred_model_1)^2)/nrow(test))^0.5



####model 2 Goodness-of-Fit
logLik(model_2)
AIC(model_2)
BIC(model_2)

#model 2: Prediction on train and test data and converting back from log
AnnualEst_pred_model_2_train<-exp(predict(model_2,train))
AnnualEst_pred_model_2_test<-exp(predict(model_2,test))



#model 2: training RMSE of not log, but back-transformed estimates
model_2_rmse_train<-(sum((train$AnnualEst-AnnualEst_pred_model_2_train)^2)/nrow(train))^0.5

#model 2: test RMSE
model_2_rmse_test<-(sum((test$AnnualEst-AnnualEst_pred_model_2_test)^2)/nrow(test))^0.5


#####################################################################################################################

#model 3: #UPDATED Model AnnualEst#########
model_3 <- lm(AnnualEst~PopH+WalkComH+WalkComPctH+EmpH+StSegH, data=train)
summary(model_3)

# ##Exporting model output using "xtable"
# print(xtable(model_3),file="model3_AADT.html",type="html")
print(xtable(model_3),digits=2) #default settings provide output for LaTeX

##Exporting model output using "stargazer"
stargazer(model_3,out="model3_AADT.html",type="html",digits=2)
stargazer(model_3,out="model3_AADT.html",digits=2) #default settings provide output for LaTeX

####model 3 goodness-of-Fit
logLik(model_3)
AIC(model_3)
BIC(model_3)

#model 3: Prediction on test data
AnnualEst_pred_model_3<-predict(model_3,test)

#model 3: training RMSE
model_3_rmse_train<-(sum(model_3$residuals^2)/nrow(train))^0.5     

#model 3: test RMSE
model_3_rmse_test<-(sum((test$AnnualEst-AnnualEst_pred_model_3)^2)/nrow(test))^0.5      

#plot(model_3)

#####################################################################################################################

#model 4: #UPDATED Model logAnnualEst#########
model_4 <- lm(logAnnualEst~PopH+WalkComH+WalkComPctH+EmpH+StSegH, data=train)
summary(model_4)

##Exporting model output using "xtable"
print(xtable(model_4),file="model4_logAADT.html",type="html")
print(xtable(model_4,digits=2)) #default settings provide output for LaTeX


##Exporting model output using "stargazer"
stargazer(model_4,out="model4_logAADT.html",type="html",digits=2)
stargazer(model_4,out="model4_logAADT.html",digits=2) #default settings provide output for LaTeX


####model 4 Goodness-of-Fit
logLik(model_4)
AIC(model_4)
BIC(model_4)

#model 4: Prediction on train and test data and converting back from log
AnnualEst_pred_model_4_train<-exp(predict(model_4,train))
AnnualEst_pred_model_4_test<-exp(predict(model_4,test))


#model 4: training RMSE of not log, but back-transformed estimates
model_4_rmse_train<-(sum((train$AnnualEst-AnnualEst_pred_model_4_train)^2)/nrow(train))^0.5    

#model 4: test RMSE
model_4_rmse_test<-(sum((test$AnnualEst-AnnualEst_pred_model_4_test)^2)/nrow(test))^0.5   

#plot(model_4)

#####################################################################################################################


#model 5: #UPDATED Model AnnualEst that includes infrastructure variables#########
model_5 <- lm(AnnualEst~PopH+WalkComH+WalkComPctH+EmpH+StSegH+HseHldH+EmpSF_H+StMetersH+PrincArt+MinorArt+Collector+Int4way+SchoolsH+Signal, data=train)
summary(model_5)

# ##Exporting model output using "xtable"
# print(xtable(model_5),file="model5_AADT.html",type="html")
print(xtable(model_5),digits=2) #default settings provide output for LaTeX

##Exporting model output using "stargazer"
stargazer(model_5,out="model5_AADT.html",type="html",digits=2)
stargazer(model_5,out="model5_AADT.html",digits=2) #default settings provide output for LaTeX


####model 5 goodness-of-Fit
logLik(model_5)
AIC(model_5)
BIC(model_5)

#model 5: Prediction on test data
AnnualEst_pred_model_5<-predict(model_5,test)

#model 5: training RMSE
model_5_rmse_train<-(sum(model_5$residuals^2)/nrow(train))^0.5


#model 5: test RMSE
model_5_rmse_test<-(sum((test$AnnualEst-AnnualEst_pred_model_5)^2)/nrow(test))^0.5

#plot(model_5)

#####################################################################################################################

#model 6: #UPDATED Model logAnnualEst that includes infrastructure variables#########
model_6 <- lm(logAnnualEst~PopH+WalkComH+WalkComPctH+EmpH+StSegH+HseHldH+EmpSF_H+StMetersH+PrincArt+MinorArt+Collector+Int4way+SchoolsH+Signal, data=train)
summary(model_6)

##Exporting model output using "xtable"
print(xtable(model_6),file="model6_logAADT.html",type="html")
print(xtable(model_6,digits=2)) #default settings provide output for LaTeX


##Exporting model output using "stargazer"
stargazer(model_6,out="model6_logAADT.html",type="html",digits=2)
stargazer(model_6,out="model6_logAADT.html",digits=2) #default settings provide output for LaTeX


####model 6 Goodness-of-Fit
logLik(model_6)
AIC(model_6)
BIC(model_6)

#model 6: Prediction on train and test data and converting back from log
AnnualEst_pred_model_6_train<-exp(predict(model_6,train))
AnnualEst_pred_model_6_test<-exp(predict(model_6,test))


#model 6: training RMSE of not log, but back-transformed estimates
model_6_rmse_train<-(sum((train$AnnualEst-AnnualEst_pred_model_6_train)^2)/nrow(train))^0.5    

#model 6: test RMSE
model_6_rmse_test<-(sum((test$AnnualEst-AnnualEst_pred_model_6_test)^2)/nrow(test))^0.5   

#plot(model_6)

#####################################################################################################################

#model 7: #UPDATED Model logAnnualEst with transformed variables#########
model_7 <- lm(logAnnualEst~PopH+logWalkComH+WalkComPctH+logEmpH+logStSegH, data=train)
summary(model_7)

##Exporting model output using "xtable"
print(xtable(model_7),file="model7_logAADT.html",type="html")
print(xtable(model_7,digits=2)) #default settings provide output for LaTeX


##Exporting model output using "stargazer"
stargazer(model_7,out="model7_logAADT.html",type="html",digits=2)
stargazer(model_7,out="model7_logAADT.html",digits=2) #default settings provide output for LaTeX


####model 7 Goodness-of-Fit
logLik(model_7)
AIC(model_7)
BIC(model_7)

#model 7: Prediction on train and test data and converting back from log
AnnualEst_pred_model_7_train<-exp(predict(model_7,train))
AnnualEst_pred_model_7_test<-exp(predict(model_7,test))


#model 7: training RMSE of not log, but back-transformed estimates
model_7_rmse_train<-(sum((train$AnnualEst-AnnualEst_pred_model_7_train)^2)/nrow(train))^0.5    

#model 7: test RMSE
model_7_rmse_test<-(sum((test$AnnualEst-AnnualEst_pred_model_7_test)^2)/nrow(test))^0.5   

#plot(model_7)

#####################################################################################################################
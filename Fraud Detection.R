# load the libraries
rm(list = ls())
library(caret)
library(klaR)
library(randomForest)
library(smotefamily)
# load  dataset
training= read.csv("training.csv")
sample_submission= read.csv("sample_submission.csv")
training= training[,-c(1:7)]
mydates= strptime(training$TransactionStartTime,format='%Y-%m-%dT%H:%M:%SZ')

Weekday= mydates$wday
Hour=mydates$hour
TimeofDay= ifelse(Hour>=0 & Hour<6,"Early Morning", 
                  ifelse(Hour>=6 & Hour<12,"Late Morning",
                         ifelse(Hour>=12 & Hour<18,"Early Afternoon", ifelse(Hour>=18 & Hour<=24,"Evening",0))))
TimeofDay=as.factor(TimeofDay)
training = cbind(training[,1:6], Hour, Weekday, TimeofDay, training[,8:9])
training$FraudResult= as.factor(training$FraudResult)


test_model= randomForest(FraudResult~., data=training, importance=T)
######END MODELLING


validation= read.csv("C:/Users/Administrator/Desktop/Zindi/Fraud Detection/test.csv")
validation_model= validation[,-c(1:7)]

mydates= strptime(validation_model$TransactionStartTime,format='%Y-%m-%dT%H:%M:%SZ')

Weekday= mydates$wday
Hour=mydates$hour
TimeofDay= ifelse(Hour>=0 & Hour<6,"Early Morning", 
                  ifelse(Hour>=6 & Hour<12,"Late Morning",
                         ifelse(Hour>=12 & Hour<18,"Early Afternoon", ifelse(Hour>=18 & Hour<=24,"Evening",0))))
TimeofDay=as.factor(TimeofDay)
PricingStrategy= validation_model$PricingStrategy
validation_model = cbind(validation_model[,1:6], Hour, Weekday, TimeofDay, PricingStrategy)


#Use lapply to get around issue
#validation_model= lapply(validation_model,as.numeric)
#Conform into as.data.frame
validation_model= as.data.frame(validation_model)

validation_model$ProductId <- factor(validation$ProductId, levels = levels(training$ProductId))
validation_model$ChannelId <- factor(validation$ChannelId, levels = levels(training$ChannelId))
validation_model$ProductCategory <- factor(validation$ProductCategory, levels = levels(training$ProductCategory))
predictions <- predict(test_model, validation_model)


a= as.data.frame(predictions)
a$predictions=as.numeric(a$predictions)
a=a-1
a= cbind(sample_submission$TransactionId,a)
a= as.data.frame(a)
colnames(a)= c("TransactionID, FraudResult")
write.csv(a,"/test_submission.csv",row.names = F,col.names = F)

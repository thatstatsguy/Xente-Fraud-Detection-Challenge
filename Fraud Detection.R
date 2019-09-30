#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
#a Split in RR
# load the libraries
rm(list = ls())
library(caret)
library(klaR)
library(randomForest)
library(smotefamily)
# load  dataset
training= read.csv("C:/Users/Chris Dunderdale/Desktop/Zindi/Fraud Detection/training2.csv")
sample_submission= read.csv("C:/Users/Chris Dunderdale/Desktop/Zindi/Fraud Detection/sample_submission.csv")
training= training[,-c(1:7)]
mydates= strptime(training$TransactionStartTime,format='%Y-%m-%dT%H:%M:%SZ')

Weekday= mydates$wday
Hour=mydates$hour
TimeofDay= ifelse(Hour>=0 & Hour<6,"Early Morning", 
                  ifelse(Hour>=6 & Hour<12,"Late Morning",
                         ifelse(Hour>=12 & Hour<18,"Early Afternoon", ifelse(Hour>=18 & Hour<=24,"Evening",0))))
TimeofDay=as.factor(TimeofDay)

training = cbind(training[,1:6], Hour, Weekday, TimeofDay, training[,8:9])

#To remove the transaction time
#training= training[,-7]
training$FraudResult= as.factor(training$FraudResult)

# attach(training)
# training_fraud=training[FraudResult==1,]
# training_nofraud=training[FraudResult==0,]
# 
# training_Sample= rbind(training_nofraud,training_fraud)
# training_Sample$FraudResult= as.factor(training_Sample$FraudResult)
# training_Sample= training_Sample[,-c(1,7)]

#frauddata.summary= frauddata %>% group_by(Fraudulent) %>% summarise(Class_count=n())
#smotedata= SMOTE(X= training_Sample[-8],target =training_Sample[8] ,dup_size = 0)



####SMOTE STUFF



#SMOTE/ADAS/DBSMOTE
#
# SMOTE_training= training
# detach(training)
# attach(SMOTE_training)
#
#As.numeric Can't be applied onto a entire list
#a= as.numeric(a)

#Use lapply to get around issue
#SMOTE_training= lapply(SMOTE_training,as.numeric)
#Conform into as.data.frame
#SMOTE_training= as.data.frame(SMOTE_training)
#names_list= names(SMOTE_training)#FOR USE LATER IN RENAMING COLS
#Apply SMOTE
# synthdata= SMOTE(SMOTE_training[,-8],target=SMOTE_training[,8],dup_size = 50)
# training= synthdata$data
# colnames(training)=names_list
#Rounding values doesn't help
#training[,1:7]= lapply(training[,1:7],round)
#training$FraudResult= as.factor(training$FraudResult)

###### END SMOTE STUFF

#####Modelling
library("caret")
library("mlbench")
library("pROC")

my_control <- trainControl(
  method="boot",
  number=2,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(training$FraudResult, 2),
  summaryFunction=twoClassSummary
)
library("rpart")
library("caretEnsemble")

model_list <- caretList(
  FraudResult~., data=training,
  trControl=my_control,
  methodList=c("rf")
)

greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))

library("caTools")
model_preds <- lapply(model_list, predict, newdata=testing, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"M"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=testing, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testing$Class)

glm_ensemble <- caretStack(
  model_list,
  method="glm",
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds2 <- model_preds
model_preds2$ensemble <- predict(glm_ensemble, newdata=testing, type="prob")
CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
colAUC(model_preds2, testing$Class)

library("gbm")
gbm_ensemble <- caretStack(
  model_list,
  method="gbm",
  verbose=FALSE,
  tuneLength=10,
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds3 <- model_preds
model_preds3$ensemble <- predict(gbm_ensemble, newdata=testing, type="prob")
colAUC(model_preds3, testing$Class)

test_model= randomForest(FraudResult~., data=training, importance=T)
#test_model <- train(FraudResult~., data=training,method= c("gbm"),verbose=F)
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
#To remove the transaction time
#validation_model= validation_model[,-7]

#As.numeric Can't be applied onto a entire list
#a= as.numeric(a)

#Use lapply to get around issue
validation_model= lapply(validation_model,as.numeric)
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
write.csv(a,"C:/Users/Administrator/Desktop/Zindi/Fraud Detection/test_submission.csv",row.names = F,col.names = F)

#Checking Empty Values in training

print("Number of NAs: ")
print(sum(is.na(training)))
print("Number of Nulls: ")
print(sum(is.null(training)))


# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(training$FraudResult, p=split, list=FALSE)
data_train <- training[ trainIndex,]
data_test <- training[-trainIndex,]
# train a naive bayes model
#Model Bottelnecking here due to the size of the data
model <- train(FraudResult~., data=training_Sample,method= c("nb"))
# make predictions
x_test <- data_test[,1:9]
y_test <- data_test[,10]
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions$Class, y_test)

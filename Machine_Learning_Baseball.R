
library(pROC)
library(ROCR)
library(xgboost)
library(caTools)
library(magrittr)
library(dplyr)
library(caret)
library(bst)
library(Ckmeans.1d.dp)



# boost<-function(trainSet, testSet)
#   
# {
#   
#   trainSet$Flag<-as.factor(ifelse(trainSet$Flag==1, "N", "Y")) 
#   
#   predictors<-c(names(trainSet[,2:ncol(testSet)]))
#   
#   outcomeName<-'Flag'
#   
#   fitControl <- trainControl(
#     method = "cv",
#     number = 10,
#     savePredictions = 'final',
#     classProbs = T)
#   
#   #Training the random forest model
#   model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='glmboost',trControl=fitControl,tuneLength=3)
#   
#   #Training the knn model
#   model_knn<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=3)
#   
#   #Training the logistic regression model
#   model_lr<-train(trainSet[,predictors],trainSet[,outcomeName],method='LogitBoost',trControl=fitControl,tuneLength=3)
#   
#   #Predicting the out of fold prediction probabilities for training data
#   trainSet$OOF_pred_rf<-model_rf$pred$N[order(model_rf$pred$rowIndex)]
#   trainSet$OOF_pred_knn<-model_knn$pred$N[order(model_knn$pred$rowIndex)]
#   trainSet$OOF_pred_lr<-model_lr$pred$N[order(model_lr$pred$rowIndex)]
#   
#   #Predicting probabilities for the test data
#   testSet$OOF_pred_rf<-predict(model_rf,testSet[predictors],type='prob')$N
#   testSet$OOF_pred_knn<-predict(model_knn,testSet[predictors],type='prob')$N
#   testSet$OOF_pred_lr<-predict(model_lr,testSet[predictors],type='prob')$N
#   
#   #Predictors for top layer models 
#   predictors_top<-c('OOF_pred_rf','OOF_pred_knn', 'OOF_pred_lr') 
#   
#   #GBM as top layer model 
#   model_gbm<- 
#     train(trainSet[,predictors_top],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=3)
#   
#   #Logistic regression as top layer model
#   model_glm<-
#     train(trainSet[,predictors_top],trainSet[,outcomeName],method='glm',trControl=fitControl,tuneLength=3)
#   
#   #predict using GBM top layer model
#   testSet$gbm_stacked<-predict(model_gbm,testSet[,predictors_top], type='prob')$N
#   
#   return(testSet$gbm_stacked)
#   
# }


boost<-function(train_sample, control_sample)
{
  
  #set.seed(501)
  
  split <- sample.split(train_sample$Flag, SplitRatio = 0.8)
  
  train_set <- subset(train_sample, split == T)
  
  test_set <- subset(train_sample, split == F)
  
  train_set <- data.frame(sapply(train_set, as.numeric))
  
  test_set <- data.frame(sapply(test_set, as.numeric))
  
  train_set <- xgb.DMatrix(data=data.matrix(train_set %>% select(-Flag)), label=train_set$Flag)
  
  test_set <- xgb.DMatrix(data=data.matrix(test_set %>% select(-Flag)), label=test_set$Flag)
  
  #watchlist <- list(val=test_set, train=train_set)
  
  #params <- list(eta=0.01, max_depth =8, min_child_weight = 2 , gamma = 10, subsample = 0.5, colsampla_bytree = 0.5, nthread=2, objective='binary:logistic')
  
  #xgb_cv <- xgb.cv(params, data = data.matrix(train[,-1]), label = train$Flag_1x ,nround = 300, nfold=5, metrics={'auc'}, early.stop.round = 50) 
  
  set.seed(189)
  
  xgb <- xgboost(data = data.matrix(train_sample %>% select(-Flag)), label = train_sample$Flag, eta = 0.01,
                 max_depth =10, gamma = 10, subsample = 0.6, colsample_bytree =0.5, min_child_weight = 5,
                 nthread = 2, nrounds = 1000, eval_metric = "auc",objective='binary:logistic') 
  
  return (predict(xgb, data.matrix(control_sample %>% select(-Flag))))
  
}

weighted_mean<-function(x)
{
  a<-c(rep(0,length(x)))
    
  for(i in 1:length(x))
  {
    a[i]<-i/length(x)
  }
  
  return (sum(a*x)/length(x))
}


count<-function(x)
{
  a<-c(rep(0,length(x)))
  
    for (i in 1:length(x))
  {
    a[i]<-length(which(x[1:i]==x[i]))
  }
  
  return (a)
}

merge_function<-function(sample,x,Flag,name)
{
  y<-c(rep(0,length(unique(x))))
  
  for (i in 1:length(unique(x)))
  {
    y[i]<- weighted_mean(subset(sample, x==unique(x)[i])$Flag)
  }
  
  result<-data.frame(unique(x),y)
  names(result)<-c(name, "mean")
  return(result)
}

profit<-function(data_sample)
  
{
  balance<-0
  
  balance[1]<-1
  
  for (i in 1:length(unique(data_sample$Date)))
  {
    calc<-subset(data_sample, data_sample$Date==unique(data_sample$Date)[i])
    
    calc$CNT<-count(calc$Team_1)
    
    
    balance[i+1]<-balance[i]+(sum(calc[!duplicated(calc$Team_1),]$Koef*calc[!duplicated(calc$Team_1),]$Flag)-nrow(calc[!duplicated(calc$Team_1),]))/
      nrow(calc[!duplicated(calc$Team_1),])*balance[i]*0.3
    
    if (nrow(subset(calc, calc$CNT>1))>0)
    {
      balance[i+1]<-balance[i+1]+(sum(subset(calc, calc$CNT>1)$Koef*subset(calc, calc$CNT>1)$Flag)-nrow(subset(calc, calc$CNT>1)))/
        nrow(subset(calc, calc$CNT>1))*balance[i+1]*0.3
    }
    
  }
  return (balance) 
  
}


###############################################################################################################################################################


################################################################################# Win #####################################################################


data<-read.csv("Factors_Calculation_All_Season_Baseball.csv",sep=",",h=T)
data$Date<-as.Date(data$Date,"%m/%d/%Y")
data<-data[with(data,order(data$Date)),]

test<-read.csv("Factors_Calculation_All_Season_Baseball_TEST.csv",sep=",",h=T)
test$Date<-as.Date(test$Date,"%m/%d/%Y")
test<-test[with(test,order(test$Date)),]

low_bound<-12
upper_bound<-20
mean_bound<-21

 cot_1<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Season_Flag>=low_bound & data$Season_Flag<=upper_bound)

 control<-
   subset(test, test$Match_T1_Home>=5 & test$Match_T2_Guest>=5 & test$Season_Flag==mean_bound)

  # control<-
  #   subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Season_Flag==mean_bound)
 
contr_date<-control$Date
contr_Team_1<-control$Team_1 
contr_Team_2<-control$Team_2 
contr_Goal_1<-control$Goal_1 
contr_Goal_2<-control$Goal_2


train<-cot_1[,c(15,34:ncol(cot_1))]
names(train)[1]<-"Flag"

control<-control[,c(16,37:ncol(control))]
names(control)[1]<-"Flag"

sample=data.frame(contr_date,  contr_Team_1, contr_Team_2, contr_Goal_1, contr_Goal_2, control$Flag, 
                  boost(train, control), 2.0)


names(sample)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef")
sample<-sample[with(sample,order(sample$Pred, decreasing = TRUE)),]
plot.roc(sample$Flag, sample$Pred,print.auc=T) 
#sample_Win<-sample[c(1:(nrow(sample)/4)),]
sample_Win<-subset(sample,sample$Pred>0.508)
#sample_Win<-sample[c(1:(nrow(sample)/3.0)),]
sample_Win<-sample_Win[with(sample_Win,order(sample_Win$Date)),]
#sample_Win$Flag<-1-sample_Win$Flag
sample_Win<-subset(sample_Win, is.na(sample_Win$Flag)==FALSE)
sum(sample_Win$Flag)/nrow(sample_Win)

profit(sample_Win)


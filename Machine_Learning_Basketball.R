
library(pROC)
library(ROCR)
library(xgboost)
library(caTools)
library(magrittr)
library(dplyr)
library(Ckmeans.1d.dp)


setwd("C:/Users/skudriashov/Documents/Cotirovki/Machine_Learning/Basketball")

data<-read.csv("Factors_Calculation_All_Season_Basketball.csv",sep=",",h=T)
data$Date<-as.Date(data$Date,"%m/%d/%Y")


low_bound<-12
upper_bound<-25

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
                 max_depth =20, gamma = 10, subsample = 0.6, colsample_bytree =0.5, min_child_weight = 5,
                 nthread = 2, nrounds = 1000, eval_metric = "auc",objective='binary:logistic', verbose=0) 
  
  return (predict(xgb, data.matrix(control_sample %>% select(-Flag))))
  
}



profit<-function(data_sample)
  
{
  balance<-0
  
  balance[1]<-1
  
  for (i in 1:length(unique(data_sample$Date)))
  {
    calc<-subset(data_sample, data_sample$Date==unique(data_sample$Date)[i])
    
    balance[i+1]<-balance[i]+(sum(calc$Koef*calc$Flag)-nrow(calc))/nrow(calc)*balance[i]*0.1
    
  }
  return (balance) 
  
}


###############################################################################################################################################################


################################################################################# Win #####################################################################

data$OT_Flag<-ifelse(data$Overtime_Flag=="OT",1,0)
data$Champ_Part_Flag<-ifelse(data$Champ_Part=="Regular",1,0)
data$Day<-as.POSIXlt(data$Date)$mday
data$Month<-as.POSIXlt(data$Date)$mon+1


cot_1<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Season_Flag>=low_bound & data$Season_Flag<upper_bound)

control<-
  subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Season_Flag==upper_bound)
names(control)[13]<-"Flag"

cot_1<-cot_1[,c("Flag_Win",
                 "Points_T2_All",             "Points_T1_All",           "Total_Win_T1_All",          "Total_Win_T2_All",          "Minus_Goals_T1_All",       
                 "Minus_Goals_T2_All",        "Ind_Total_more_90_T2_All",  "Ind_Total_more_90_T1_All",  "Total_more_200_T1_All",     "Plus_Goals_T1_All",        
                 "Ind_Total_more_110_T1_All", "Minus_Goals_T2_5M",         "Ind_Total_more_110_T2_All", "Plus_Goals_T2_5M_Guest",    "Total_Defeat_T1_All",      
                 "Plus_Goals_T2_5M",          "Total_more_200_T2_All",     "Ind_Total_more_100_T2_All", "Minus_Goals_T1_5M_Home",    "Total_Defeat_T2_All",      
                 "Total_more_190_T2_All",     "Total_more_190_T1_All",     "Total_more_210_T1_All",     "Minus_Goals_T2_5M_Guest",   "Minus_Goals_T1_5M",        
                 "Plus_Goals_T1_5M",          "Ind_Total_more_100_T1_All", "Total_more_220_T2_All",     "Plus_Goals_T2_All",        "Total_more_210_T2_All",    
                 "Ind_Total_more_80_T1_All",  "Plus_Goals_T1_5M_Home",     "Total_more_220_T1_All",     "Ind_Total_more_80_T2_All",  "Total_more_230_T2_All",    
                 "Total_more_230_T1_All",     "Total_Win_T1_10M_Home",     "Day",                       "Ind_Total_more_120_T1_All", "Total_Win_T2_10M_Guest" )]

train<-cot_1
names(train)[1]<-"Flag"


sample=data.frame(control$Date,  control$Team_1, control$Team_2, control$Goal_1, control$Goal_2, control$Flag, boost(train, control), 1.6) 
names(sample)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef")
sample<-sample[with(sample,order(sample$Pred, decreasing = TRUE)),]
sample<-sample[c(1:(nrow(sample)/5)),]
sample<-sample[with(sample,order(sample$Date)),]
sum(sample$Koef*sample$Flag)-nrow(sample)

sample_Win<-sample

profit(sample_Win)

################################################################################# More 210 #####################################################################

data$OT_Flag<-ifelse(data$Overtime_Flag=="OT",1,0)
data$Champ_Part_Flag<-ifelse(data$Champ_Part=="Regular",1,0)
data$Day<-as.POSIXlt(data$Date)$mday
data$Month<-as.POSIXlt(data$Date)$mon+1


cot_1<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Season_Flag>=low_bound & data$Season_Flag<upper_bound)

control<-
  subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Season_Flag==upper_bound)
names(control)[19]<-"Flag"

train<-cot_1
names(train)[19]<-"Flag"


sample=data.frame(control$Date,  control$Team_1, control$Team_2, control$Goal_1, control$Goal_2, control$Flag, boost(train, control), 1.8) 
names(sample)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef")
sample<-sample[with(sample,order(sample$Pred, decreasing = TRUE)),]
sample<-sample[c(1:(nrow(sample)/5)),]
sample<-sample[with(sample,order(sample$Date)),]
sum(sample$Koef*sample$Flag)-nrow(sample)

sample_More_210<-sample

profit(sample_More_210)

################################################################################# Overall #####################################################################

sample_final<-rbind(sample_Win, sample_More_210)

profit(sample_final)



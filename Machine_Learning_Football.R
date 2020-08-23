
library(pROC)
library(ROCR)
library(xgboost)
library(caTools)
library(magrittr)
library(dplyr)
library(Ckmeans.1d.dp)
library(tibble)

boost<-function(train_sample, control_sample)
{
  
  #set.seed(501)
  
    # split <- sample.split(train_sample$Flag, SplitRatio = 0.8)
    # 
    # train_set <- subset(train_sample, split == T)
    # 
    # test_set <- subset(train_sample, split == F)
    # 
    # train_set <- data.frame(sapply(train_set, as.numeric))
    # 
    # test_set <- data.frame(sapply(test_set, as.numeric))
    # 
    # train_set <- xgb.DMatrix(data=data.matrix(train_set %>% select(-Flag)), label=train_set$Flag)
    # 
    # test_set <- xgb.DMatrix(data=data.matrix(test_set %>% select(-Flag)), label=test_set$Flag)
  
  #watchlist <- list(val=test_set, train=train_set)
  
  #params <- list(eta=0.01, max_depth =8, min_child_weight = 2 , gamma = 10, subsample = 0.5, colsampla_bytree = 0.5, nthread=2, objective='binary:logistic')
  
  #xgb_cv <- xgb.cv(params, data = data.matrix(train[,-1]), label = train$Flag_1x ,nround = 300, nfold=5, metrics={'auc'}, early.stop.round = 50) 
  
  set.seed(189)
  
  xgb <- xgboost(data = data.matrix(train_sample %>% select(-Flag)), label = train_sample$Flag, eta = 0.01,
                 max_depth =20, gamma = 10, subsample = 0.6, colsample_bytree =0.5, min_child_weight = 5,
                 nthread = 2, nrounds = 1000, eval_metric = "auc",objective='binary:logistic') 
  
  return (predict(xgb, data.matrix(control_sample %>% select(-Flag))))
  
}


profit<-function(data_sample)
  
{
  balance<-0
  
  
  balance[1]<-1
  
  for (i in 1:length(unique(data_sample$Date)))
  {
    calc<-subset(data_sample, data_sample$Date==unique(data_sample$Date)[i])
    
    balance[i+1]<-balance[i]+(sum(calc$Koef*calc$Flag)-nrow(calc))/nrow(calc)*balance[i]*0.3
    
  }
  return (balance) 
  
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


williams<-function(sample)
{
  R<-c(rep(0, length(sample)))
  
  R[1:5]<-sample[1:5]
  
  for (i in 6:(length(sample)))
  {
    R[i]<-(max(sample[(i-5):i])-sample[i-1])/(min(sample[(i-5):i])-sample[i-1])*2
  }
  return (R)
}

williams(profit(sample_Win))

change_proc<-function(sample)
{
  R<-c(rep(0, length(sample)))
  
  R[1]<-sample[1]
  
  for (i in 2:(length(sample)))
  {
    R[i]<-sample[i]/max(sample[1:(i-1)])
  }
  return (R)
  
}



setwd("C:/Users/skudriashov/Documents/Cotirovki/Machine_Learning/Football")

#####################################################################################Onex##########################################################################

        data<-read.csv("Factors_Calculation_All_Data.csv",sep=",",h=T)
        data$Date<-as.Date(data$Date,"%m/%d/%Y")
        data<-data[with(data,order(data$Date)),]
        data$Day<-as.POSIXlt(data$Date)$mday
        data$Month<-as.POSIXlt(data$Date)$mon+1
        colnames(data)[91]<-"Flag"
        
        low_bound<-16
        upper_bound<-23
        mean_bound<-24
  
        validation<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Onex >= 1.6 & data$Season_Flag>=low_bound & data$Season_Flag<=upper_bound)
        
        control<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Onex >= 1.6  & data$Season_Flag==mean_bound)
        
        Football_League_aggr<-data.frame(unique(validation$League))
        names(Football_League_aggr)<-c("League")
        
        for (i in 1:length(unique(Football_League_aggr$League)))
        {
          Football_League_aggr$mean[i]<- mean(subset(validation, validation$League==unique(Football_League_aggr$League)[i])$Flag)*
            mean(subset(validation, validation$League==unique(Football_League_aggr$League)[i])$Onex, na.rm=TRUE)
        }
        Football_League_aggr<-Football_League_aggr[with(Football_League_aggr,order(Football_League_aggr$mean, decreasing = TRUE)),]
        
        validation<-subset(validation, validation$League %in% Football_League_aggr$League[1:10])
        validation<-validation[with(validation,order(validation$Date)),]
        
        # ttt<-subset(data, data$League %in% Football_League_aggr$League[1:10] & data$Defeat >= 1.6 & data$Season_Flag==24)
        # 
        # mean(ttt$Defeat)*mean(ttt$Flag)
        
        # validation<-merge(validation, Football_League_aggr, by ="League")
        # colnames(validation)[ncol(validation)]<-"League_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Day, validation$Flag, "Day"), by="Day")
        # colnames(validation)[ncol(validation)]<-"Day_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Month, validation$Flag, "Month"), by="Month")
        # colnames(validation)[ncol(validation)]<-"Month_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_1, validation$Flag, "Team_1"), by="Team_1")
        # colnames(validation)[ncol(validation)]<-"Team_1_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_2, validation$Flag, "Team_2"), by="Team_2")
        # colnames(validation)[ncol(validation)]<-"Team_2_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_1Team_2, validation$Flag, "Team_1Team_2"), by="Team_1Team_2")
        # colnames(validation)[ncol(validation)]<-"Team_1Team_2_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        control<-subset(control, control$League %in% Football_League_aggr$League[1:10])
        control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, Football_League_aggr, by ="League")
        # colnames(control)[ncol(control)]<-"League_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Day, validation$Flag, "Day"), by="Day")
        # colnames(control)[ncol(control)]<-"Day_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Month, validation$Flag, "Month"), by="Month")
        # colnames(control)[ncol(control)]<-"Month_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_1, validation$Flag, "Team_1"), by="Team_1")
        # colnames(control)[ncol(control)]<-"Team_1_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_2, validation$Flag, "Team_2"), by="Team_2")
        # colnames(control)[ncol(control)]<-"Team_2_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_1Team_2, validation$Flag, "Team_1Team_2"), by="Team_1Team_2")
        # colnames(control)[ncol(control)]<-"Team_1Team_2_num"
        # control<-control[with(control,order(control$Date)),]
        
        validation_t<-subset(validation, validation$Onex>=1.6)
        control_t<-subset(control, control$Onex>=1.6)
        
        names(validation_t)
        
        sample=data.frame(control_t$Date,  control_t$Team_1, control_t$Team_2, control_t$Goal_1, control_t$Goal_2, control_t$Flag, 
                           boost(validation_t[,c(91,53:54,108:ncol(validation_t))],
                                 control_t[,c(91,53:54,108:ncol(control_t))]),
                          control_t$Onex*1.05)
        
        names(sample)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef")
        sample<-sample[with(sample,order(sample$Pred, decreasing = TRUE)),]
        plot.roc(sample$Flag, sample$Pred,print.auc=T) 
        #sample_Win<-subset(sample, sample$Koef>1.7)
        sample_Onex<-sample[c(1:(nrow(sample)/10.0)),]
        sample_Onex<-sample_Onex[with(sample_Onex,order(sample_Onex$Date)),]
        sum(sample_Onex$Flag)/nrow(sample_Onex)
        
        profit(sample_Onex)
  
        #plot(unique(sample_Win$Date), profit(sample_Win)[2:length(profit(sample_Win))])
  
        
  #####################################################################################Twox##########################################################################
        
        data<-read.csv("Factors_Calculation_All_Data.csv",sep=",",h=T)
        data$Date<-as.Date(data$Date,"%m/%d/%Y")
        data<-data[with(data,order(data$Date)),]
        data$Day<-as.POSIXlt(data$Date)$mday
        data$Month<-as.POSIXlt(data$Date)$mon+1
        colnames(data)[92]<-"Flag"
        
  
        validation<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Twox >= 1.6 & data$Season_Flag>=low_bound & data$Season_Flag<=upper_bound)
        
        control<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Twox >= 1.6  & data$Season_Flag==mean_bound)
        
        Football_League_aggr<-data.frame(unique(validation$League))
        names(Football_League_aggr)<-c("League")
        
        for (i in 1:length(unique(Football_League_aggr$League)))
        {
          Football_League_aggr$mean[i]<- mean(subset(validation, validation$League==unique(Football_League_aggr$League)[i])$Flag)*
            mean(subset(validation, validation$League==unique(Football_League_aggr$League)[i])$Twox, na.rm=TRUE)
        }
        Football_League_aggr<-Football_League_aggr[with(Football_League_aggr,order(Football_League_aggr$mean, decreasing = TRUE)),]
        
        validation<-subset(validation, validation$League %in% Football_League_aggr$League[1:10])
        validation<-validation[with(validation,order(validation$Date)),]
        
        # validation<-merge(validation, Football_League_aggr, by ="League")
        # colnames(validation)[ncol(validation)]<-"League_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Day, validation$Flag, "Day"), by="Day")
        # colnames(validation)[ncol(validation)]<-"Day_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Month, validation$Flag, "Month"), by="Month")
        # colnames(validation)[ncol(validation)]<-"Month_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_1, validation$Flag, "Team_1"), by="Team_1")
        # colnames(validation)[ncol(validation)]<-"Team_1_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_2, validation$Flag, "Team_2"), by="Team_2")
        # colnames(validation)[ncol(validation)]<-"Team_2_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_1Team_2, validation$Flag, "Team_1Team_2"), by="Team_1Team_2")
        # colnames(validation)[ncol(validation)]<-"Team_1Team_2_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        control<-subset(control, control$League %in% Football_League_aggr$League[1:10])
        control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, Football_League_aggr, by ="League")
        # colnames(control)[ncol(control)]<-"League_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Day, validation$Flag, "Day"), by="Day")
        # colnames(control)[ncol(control)]<-"Day_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Month, validation$Flag, "Month"), by="Month")
        # colnames(control)[ncol(control)]<-"Month_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_1, validation$Flag, "Team_1"), by="Team_1")
        # colnames(control)[ncol(control)]<-"Team_1_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_2, validation$Flag, "Team_2"), by="Team_2")
        # colnames(control)[ncol(control)]<-"Team_2_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_1Team_2, validation$Flag, "Team_1Team_2"), by="Team_1Team_2")
        # colnames(control)[ncol(control)]<-"Team_1Team_2_num"
        # control<-control[with(control,order(control$Date)),]
        
        validation_t<-subset(validation, validation$Twox>=1.6)
        control_t<-subset(control, control$Twox>=1.6)
        
        sample=data.frame(control_t$Date,  control_t$Team_1, control_t$Team_2, control_t$Goal_1, control_t$Goal_2, control_t$Flag, 
                          boost(validation_t[,c(92,53:54,108:ncol(validation_t))],
                                control_t[,c(92,53:54,108:ncol(control_t))]),
                          control_t$Twox*1.05)
        
        
        names(sample)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef")
        sample<-sample[with(sample,order(sample$Pred, decreasing = TRUE)),]
        plot.roc(sample$Flag, sample$Pred,print.auc=T) 
        #sample_Win<-subset(sample, sample$Koef>1.7)
        sample_Twox<-sample[c(1:(nrow(sample)/10.0)),]
        sample_Twox<-sample_Twox[with(sample_Twox,order(sample_Twox$Date)),]
        sum(sample_Twox$Flag)/nrow(sample_Twox)
        
        profit(sample_Twox)
  
  ###########################################################################################Win###################################################################
        
        data<-read.csv("Factors_Calculation_All_Data.csv",sep=",",h=T)
        data$Date<-as.Date(data$Date,"%m/%d/%Y")
        data<-data[with(data,order(data$Date)),]
        data$Day<-as.POSIXlt(data$Date)$mday
        data$Month<-as.POSIXlt(data$Date)$mon+1
        colnames(data)[88]<-"Flag"
        
        validation<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Win >= 1.6 & data$Season_Flag>=low_bound & data$Season_Flag<=upper_bound)
        
        control<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Win >= 1.6  & data$Season_Flag==mean_bound)
        
        Football_League_aggr<-data.frame(unique(validation$League))
        names(Football_League_aggr)<-c("League")
        
        for (i in 1:length(unique(Football_League_aggr$League)))
        {
          Football_League_aggr$mean[i]<- mean(subset(validation, validation$League==unique(Football_League_aggr$League)[i])$Flag)*
            mean(subset(validation, validation$League==unique(Football_League_aggr$League)[i])$Win, na.rm=TRUE)
        }
        Football_League_aggr<-Football_League_aggr[with(Football_League_aggr,order(Football_League_aggr$mean, decreasing = TRUE)),]
        
        validation<-subset(validation, validation$League %in% Football_League_aggr$League[1:10])
        validation<-validation[with(validation,order(validation$Date)),]
        
        # validation<-merge(validation, Football_League_aggr, by ="League")
        # colnames(validation)[ncol(validation)]<-"League_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Day, validation$Flag, "Day"), by="Day")
        # colnames(validation)[ncol(validation)]<-"Day_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Month, validation$Flag, "Month"), by="Month")
        # colnames(validation)[ncol(validation)]<-"Month_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_1, validation$Flag, "Team_1"), by="Team_1")
        # colnames(validation)[ncol(validation)]<-"Team_1_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_2, validation$Flag, "Team_2"), by="Team_2")
        # colnames(validation)[ncol(validation)]<-"Team_2_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_1Team_2, validation$Flag, "Team_1Team_2"), by="Team_1Team_2")
        # colnames(validation)[ncol(validation)]<-"Team_1Team_2_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        control<-subset(control, control$League %in% Football_League_aggr$League[1:10])
        control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, Football_League_aggr, by ="League")
        # colnames(control)[ncol(control)]<-"League_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Day, validation$Flag, "Day"), by="Day")
        # colnames(control)[ncol(control)]<-"Day_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Month, validation$Flag, "Month"), by="Month")
        # colnames(control)[ncol(control)]<-"Month_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_1, validation$Flag, "Team_1"), by="Team_1")
        # colnames(control)[ncol(control)]<-"Team_1_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_2, validation$Flag, "Team_2"), by="Team_2")
        # colnames(control)[ncol(control)]<-"Team_2_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_1Team_2, validation$Flag, "Team_1Team_2"), by="Team_1Team_2")
        # colnames(control)[ncol(control)]<-"Team_1Team_2_num"
        # control<-control[with(control,order(control$Date)),]
        
        validation_t<-subset(validation, validation$Win>=1.6)
        control_t<-subset(control, control$Win>=1.6)
        
        sample=data.frame(control_t$Date,  control_t$Team_1, control_t$Team_2, control_t$Goal_1, control_t$Goal_2, control_t$Flag, 
                          boost(validation_t[,c(88,50:52,108:ncol(validation_t))],
                                control_t[,c(88,50:52,108:ncol(control_t))]),
                          control_t$Win*1.05)
        
        
        names(sample)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef")
        sample<-sample[with(sample,order(sample$Pred, decreasing = TRUE)),]
        plot.roc(sample$Flag, sample$Pred,print.auc=T) 
        #sample_Win<-subset(sample, sample$Koef>1.7)
        sample_Win<-sample[c(1:(nrow(sample)/10.0)),]
        sample_Win<-sample_Win[with(sample_Win,order(sample_Win$Date)),]
        sum(sample_Win$Flag)/nrow(sample_Win)
        
        profit(sample_Win)      
        
              
  ###########################################################################################Defeat###################################################################
        
        data<-read.csv("Factors_Calculation_All_Data.csv",sep=",",h=T)
        data$Date<-as.Date(data$Date,"%m/%d/%Y")
        data<-data[with(data,order(data$Date)),]
        data$Day<-as.POSIXlt(data$Date)$mday
        data$Month<-as.POSIXlt(data$Date)$mon+1
        colnames(data)[90]<-"Flag"
        
  
        validation<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Defeat >= 1.6 & data$Season_Flag>=low_bound & data$Season_Flag<=upper_bound)
        
        control<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Defeat >= 1.6  & data$Season_Flag==mean_bound)
        
        Football_League_aggr<-data.frame(unique(validation$League))
        names(Football_League_aggr)<-c("League")
        
        for (i in 1:length(unique(Football_League_aggr$League)))
        {
          Football_League_aggr$mean[i]<- mean(subset(validation, validation$League==unique(Football_League_aggr$League)[i])$Flag)*
            mean(subset(validation, validation$League==unique(Football_League_aggr$League)[i])$Defeat, na.rm=TRUE)
        }
        Football_League_aggr<-Football_League_aggr[with(Football_League_aggr,order(Football_League_aggr$mean, decreasing = TRUE)),]
        
        validation<-subset(validation, validation$League %in% Football_League_aggr$League[1:10])
        validation<-validation[with(validation,order(validation$Date)),]
        
        # validation<-merge(validation, Football_League_aggr, by ="League")
        # colnames(validation)[ncol(validation)]<-"League_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Day, validation$Flag, "Day"), by="Day")
        # colnames(validation)[ncol(validation)]<-"Day_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Month, validation$Flag, "Month"), by="Month")
        # colnames(validation)[ncol(validation)]<-"Month_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_1, validation$Flag, "Team_1"), by="Team_1")
        # colnames(validation)[ncol(validation)]<-"Team_1_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_2, validation$Flag, "Team_2"), by="Team_2")
        # colnames(validation)[ncol(validation)]<-"Team_2_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_1Team_2, validation$Flag, "Team_1Team_2"), by="Team_1Team_2")
        # colnames(validation)[ncol(validation)]<-"Team_1Team_2_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        control<-subset(control, control$League %in% Football_League_aggr$League[1:10])
        control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, Football_League_aggr, by ="League")
        # colnames(control)[ncol(control)]<-"League_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Day, validation$Flag, "Day"), by="Day")
        # colnames(control)[ncol(control)]<-"Day_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Month, validation$Flag, "Month"), by="Month")
        # colnames(control)[ncol(control)]<-"Month_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_1, validation$Flag, "Team_1"), by="Team_1")
        # colnames(control)[ncol(control)]<-"Team_1_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_2, validation$Flag, "Team_2"), by="Team_2")
        # colnames(control)[ncol(control)]<-"Team_2_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_1Team_2, validation$Flag, "Team_1Team_2"), by="Team_1Team_2")
        # colnames(control)[ncol(control)]<-"Team_1Team_2_num"
        # control<-control[with(control,order(control$Date)),]
        
        validation_t<-subset(validation, validation$Defeat>=1.6)
        control_t<-subset(control, control$Defeat>=1.6)
        
        sample=data.frame(control_t$Date,  control_t$Team_1, control_t$Team_2, control_t$Goal_1, control_t$Goal_2, control_t$Flag, 
                          boost(validation_t[,c(90,50:52,108:ncol(validation_t))],
                                control_t[,c(90,50:52,108:ncol(control_t))]),
                          control_t$Defeat*1.05)
  
        
        names(sample)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef")
        sample<-sample[with(sample,order(sample$Pred, decreasing = TRUE)),]
        plot.roc(sample$Flag, sample$Pred,print.auc=T) 
        #sample_Win<-subset(sample, sample$Koef>1.7)
        sample_Defeat<-sample[c(1:(nrow(sample)/10.0)),]
        sample_Defeat<-sample_Defeat[with(sample_Defeat,order(sample_Defeat$Date)),]
        sum(sample_Defeat$Flag)/nrow(sample_Defeat)
        
        profit(sample_Defeat)
    
        
        
  ###########################################################################################Total 2.5 more ###################################################################
        
        data<-read.csv("Factors_Calculation_All_Data.csv",sep=",",h=T)
        data$Date<-as.Date(data$Date,"%m/%d/%Y")
        data<-data[with(data,order(data$Date)),]
        data$Day<-as.POSIXlt(data$Date)$mday
        data$Month<-as.POSIXlt(data$Date)$mon+1
        colnames(data)[97]<-"Flag"
  
    
        
        validation<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$BbMx.2.5 >= 1.6 & data$Season_Flag>=low_bound & data$Season_Flag<=upper_bound)
        
        control<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$BbMx.2.5 >= 1.6  & data$Season_Flag==mean_bound)
        
        Football_League_aggr<-data.frame(unique(validation$League))
        names(Football_League_aggr)<-c("League")
        
        for (i in 1:length(unique(Football_League_aggr$League)))
        {
          Football_League_aggr$mean[i]<- mean(subset(validation, validation$League==unique(Football_League_aggr$League)[i])$Flag)*
            mean(subset(validation, validation$League==unique(Football_League_aggr$League)[i])$B365.2.5, na.rm=TRUE)
        }
        Football_League_aggr<-Football_League_aggr[with(Football_League_aggr,order(Football_League_aggr$mean, decreasing = TRUE)),]
        
        validation<-subset(validation, validation$League %in% Football_League_aggr$League[1:10])
        validation<-validation[with(validation,order(validation$Date)),]
        
        # validation<-merge(validation, Football_League_aggr, by ="League")
        # colnames(validation)[ncol(validation)]<-"League_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Day, validation$Flag, "Day"), by="Day")
        # colnames(validation)[ncol(validation)]<-"Day_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Month, validation$Flag, "Month"), by="Month")
        # colnames(validation)[ncol(validation)]<-"Month_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_1, validation$Flag, "Team_1"), by="Team_1")
        # colnames(validation)[ncol(validation)]<-"Team_1_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_2, validation$Flag, "Team_2"), by="Team_2")
        # colnames(validation)[ncol(validation)]<-"Team_2_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        # validation<-merge(validation, merge_function(validation, validation$Team_1Team_2, validation$Flag, "Team_1Team_2"), by="Team_1Team_2")
        # colnames(validation)[ncol(validation)]<-"Team_1Team_2_num"
        # validation<-validation[with(validation,order(validation$Date)),]
        # 
        control<-subset(control, control$League %in% Football_League_aggr$League[1:10])
        control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, Football_League_aggr, by ="League")
        # colnames(control)[ncol(control)]<-"League_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Day, validation$Flag, "Day"), by="Day")
        # colnames(control)[ncol(control)]<-"Day_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Month, validation$Flag, "Month"), by="Month")
        # colnames(control)[ncol(control)]<-"Month_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_1, validation$Flag, "Team_1"), by="Team_1")
        # colnames(control)[ncol(control)]<-"Team_1_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_2, validation$Flag, "Team_2"), by="Team_2")
        # colnames(control)[ncol(control)]<-"Team_2_num"
        # control<-control[with(control,order(control$Date)),]
        # 
        # control<-merge(control, merge_function(validation, validation$Team_1Team_2, validation$Flag, "Team_1Team_2"), by="Team_1Team_2")
        # colnames(control)[ncol(control)]<-"Team_1Team_2_num"
        # control<-control[with(control,order(control$Date)),]
        
        validation_t<-subset(validation, validation$BbMx.2.5>=1.6)
        control_t<-subset(control, control$BbMx.2.5>=1.6)
        
        sample=data.frame(control_t$Date,  control_t$Team_1, control_t$Team_2, control_t$Goal_1, control_t$Goal_2, control_t$Flag, 
                          boost(validation_t[,c(97,57:58,108:ncol(validation_t))],
                                control_t[,c(97,57:58,108:ncol(control_t))]),
                          control_t$BbMx.2.5*1.03)
        
        
        names(sample)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef")
        sample<-sample[with(sample,order(sample$Pred, decreasing = TRUE)),]
        plot.roc(sample$Flag, sample$Pred,print.auc=T) 
        #sample_Win<-subset(sample, sample$Koef>1.7)
        sample_Total_3_more<-sample[c(1:(nrow(sample)/10.0)),]
        sample_Total_3_more<-sample_Total_3_more[with(sample_Total_3_more,order(sample_Total_3_more$Date)),]
        sum(sample_Total_3_more$Flag)/nrow(sample_Total_3_more)
        
        profit(sample_Total_3_more) 
        
        #plot(unique(sample_Win$Date), profit(sample_Win)[2:length(profit(sample_Win))])
        
        
  #######################################################################################################################################################
        
         sample_overall<-rbind(sample_Total_3_more,rbind(rbind(sample_Win,sample_Onex), rbind(sample_Twox, sample_Defeat)))
         #sample_overall<-rbind(rbind(sample_Win, sample_Onex), sample_Defeat)
         sample_overall<-sample_overall[with(sample_overall,order(sample_overall$Date)),]
         sum(sample_overall$Flag)/nrow(sample_overall)
         profit(sample_overall)
        
        
        
        profit(rbind(rbind(sample_Win, sample_Onex), sample_Defeat))
        
        param<-0.75
        
        sample_overall<-subset(sample_Win, sample_Win$Date<unique(sample_Win$Date)[3])
        
        for (i in 3:length(unique(sample_Win$Date)))
        {
          if (change_proc(profit(sample_Win)[1:i-1])[i-1]>param)
          {
            sample_overall<-rbind(sample_overall, subset(sample_Win, sample_Win$Date==unique(sample_Win$Date)[i]))
          }
        }
        
        for (i in 3:length(unique(sample_Onex$Date)))
        {
          if (change_proc(profit(sample_Onex)[1:i-1])[i-1]>param)
          {
            sample_overall<-rbind(sample_overall, subset(sample_Onex, sample_Onex$Date==unique(sample_Onex$Date)[i]))
          }
        }
        
        for (i in 3:length(unique(sample_Twox$Date)))
        {
          if (change_proc(profit(sample_Twox)[1:i-1])[i-1]>param)
          {
            sample_overall<-rbind(sample_overall, subset(sample_Twox, sample_Twox$Date==unique(sample_Twox$Date)[i]))
          }
        }
        
        for (i in 3:length(unique(sample_Defeat$Date)))
        {
          if (change_proc(profit(sample_Defeat)[1:i-1])[i-1]>param)
          {
            sample_overall<-rbind(sample_overall, subset(sample_Defeat, sample_Defeat$Date==unique(sample_Defeat$Date)[i]))
          }
        }
        
        for (i in 3:length(unique(sample_Total_3_more$Date)))
        {
          if (change_proc(profit(sample_Total_3_more)[1:i-1])[i-1]>param)
          {
            sample_overall<-rbind(sample_overall, subset(sample_Total_3_more, sample_Total_3_more$Date==unique(sample_Total_3_more$Date)[i]))
          }
        }
        
        sample_overall<-sample_overall[with(sample_overall,order(sample_overall$Date)),]
        profit(sample_overall)      
      

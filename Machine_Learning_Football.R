


 directory<-"/root/"
 #directory<-"C:/Users/Сергей/Desktop/НЕ ЗАХЛАМЛЯЙ РАБОЧИЙ СТОЛ, СКЛАДЫВАТЬ ВСЕ СЮДА/Сергей/Machine_Learning/Machine_Learning/SPORTS/Football/"

 #directory<-"C:/Users/mb29945/Desktop/SK/Machine_Learning/Machine_Learning/SPORTS/Football/"
  
 download.file("https://www.football-data.co.uk/mmz4281/1920/all-euro-data-2019-2020.xlsx",
               paste(directory, "football_past.xlsx", sep=""), mode = "wb")

 download.file("https://www.football-data.co.uk/fixtures.xlsx",
               paste(directory, "football_future.xlsx", sep=""), mode = "wb")

 
library(downloader)
library(pROC)
library(ROCR)
library(xgboost)
library(caTools)
library(magrittr)
library(dplyr)
library(Ckmeans.1d.dp)
library(readxl)
library(bitops)
library(RCurl)
library(xlsx)
library(ggplot2)
library(factoextra)

weighted_mean<-function(x)
{
  a<-c(rep(0,length(x)))
  
  for(i in 1:length(x))
  {
    a[i]<-i/length(x)
  }
  
  return (sum(a*x)/length(x))
}

boost<-function(train_sample, control_sample, string)

  {
  
  set.seed(189)
  
  bst <- xgboost(data = data.matrix(train_sample[,!names(train_sample) %in% "Flag"]), label = train_sample$Flag, eta = 0.01,
                 max_depth =20, gamma = 10, subsample = 0.6, colsample_bytree =0.5, min_child_weight = 5,
                 nthread = 2,  nrounds = 1000, eval_metric = "auc",objective='binary:logistic') 
  
  
  xgb.save(bst, paste(directory, 'xgb.model_', string, sep=""))
  
  #return (predict(xgb, data.matrix(control_sample)))
  #return(xgb)
  
  return ()
}


profit<-function(data_sample)
  
{
  balance<-0
  
  balance[1]<-1
  
  for (i in 1:length(unique(data_sample$Date)))
  {
    calc<-subset(data_sample, data_sample$Date==unique(data_sample$Date)[i])
    
    balance[i+1]<-balance[i]+(sum(calc$Koef*as.numeric(calc$Flag))-nrow(calc))/nrow(calc)*balance[i]*0.12
    
  }
  
  return (balance) 
  
}

find_best_strategy<-function(sample, ts)
  
{  
  
  
  for (i in 1:length(unique(sample$Date)))
    
  {
    
    
    max<-0
    kmaxup<-0
    
    for (j in 1:30)
      
    {
      
      
      if (nrow(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Pred>=ts+j/100))>0)
      {
        
        if(profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Pred>=ts+j/100))[length(profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Pred>=ts+j/100)))]>max)    
          
        {
          
          max<-profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Pred>=ts+j/100))[length(profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Pred>=ts+j/100)))]
          kmaxup<-j
        }
        
        
      } 
      
      
    }
    
    if (exists("sample_overall")==TRUE)
      
    {
      
      sample_overall<-rbind(sample_overall, subset(sample, sample$Date==unique(sample$Date)[i] & sample$Pred>=ts+kmaxup/100))
      
      
    }
    
    
    if (exists("sample_overall")==FALSE)
    {
      
      sample_overall<-subset(sample, sample$Date==unique(sample$Date)[i] & sample$Date==unique(sample$Date)[i] & sample$Pred>=ts+kmaxup/100)
      
    }
    
  }
  
  return(sample_overall)
  
}

# 
# trend<-function(sample,n)
#   
# {
#   
#   result<-data.frame(matrix(0,nrow(sample), ncol(sample)-5))
#   colnames(result)<-colnames(sample)[c(1:(ncol(sample)-5))]
#   
#   
#   for (i in 1:nrow(result))
#     
#   {
#     for (j in 1:ncol(result))
#     {
#       
#       result[i,j]<-sample[i,j]/sample[i,j-min(n,j)+1]-1
#     }
#   }
#   
#   result$Strategy<-sample$Strategy
#   
#   
#   return(result) 
# }
# 
# 
# 
# find_best_strategy<-function(sample, n_1,n_2,n_3)
#   
# { 
#   
#   result<-data.frame(matrix(0,n_1*n_2*n_3, length(unique(sample$Date))))
#   colnames(result)<-sort(unique(sample$Date))
#   result$Strategy<-""
#   result$Low<-0
#   result$High<-0
#   result$Pred<-0
#   result$Count<-0
#   result[,1]<-1
#   
#   for (i in 1:length(unique(sample$Date)))
#     
#   {
#     count<-0
#     
#     for (j in 1:n_1)
#       
#     {
#       for (l in j:n_2)
#         
#         
#       {
#         
#         for (m in 1:n_3)
#           
#         {
#           
#           count<-count+1
#           kdown<-1+j/(n_1)
#           #kdown<-1
#           kup<-1+l/(n_2)
#           #kup<-30
#           kpred<-0.5+m/100
#           
#           result$Strategy[count]<-paste("Low:", toString(j/(n_1)+1), "UP:", toString (l/(n_2)+1), "Pred:", toString(m/100+0.5), sep=" ")
#           
#           if (nrow(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Koef>=kdown & sample$Koef<=kup & sample$Pred>=kpred))>0)
#             
#           {
#             
#             result[count,i]=profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Koef>=kdown & sample$Koef<=kup & sample$Pred>=kpred))[length(profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Koef>=kdown & sample$Koef<=kup & sample$Pred>=kpred)))]
#             
#             
#           }
#           
#           else 
#             
#           {
#             
#             
#             result[count,i]=result[count,ifelse(i-1>=1,i-1,1)] 
#             
#           }
#           
#           result$Low[count]<-kdown
#           result$High[count]<-kup
#           result$Pred[count]<-kpred
#           result$Count[count]<-nrow(subset(sample, sample$Koef>=result$Low[count] & sample$Koef<=result$High[count] & sample$Pred>=result$Pred[count])) 
#         }
#         
#       }
#     }
#     
#   }
#   
#   result<-result[result$Strategy!="",]
#   
#   return(result)
#   
# }
# 
# 
# find_best_strategy<-function(sample, n_1)
#   
# { 
#   
#   result<-data.frame(matrix(0,n_1, length(unique(sample$Date))))
#   colnames(result)<-sort(unique(sample$Date))
#   result$Strategy<-""
#   result$Pred<-0
#   result$Count<-0
#   result[,1]<-1
#   
#   for (i in 1:length(unique(sample$Date)))
#     
#   {
#     count<-0
#     
#     
#     for (m in 1:n_1)
#       
#     {
#       
#       count<-count+1
#       #kdown<-1+j/(n_1)
#       #kdown<-1
#       #kup<-1+l/(n_2)
#       #kup<-30
#       kpred<-0.5+m/100
#       
#       result$Strategy[count]<-paste("Pred:", toString(m/100+0.5), sep=" ")
#       
#       if (nrow(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Pred>=kpred))>0)
#         
#       {
#         
#         result[count,i]=profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Pred>=kpred))[length(profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Pred>=kpred)))]
#         
#         
#       }
#       
#       else 
#         
#       {
#         
#         
#         result[count,i]=result[count,ifelse(i-1>=1,i-1,1)] 
#         
#       }
#       
#       result$Pred[count]<-kpred
#       result$Count[count]<-nrow(subset(sample, sample$Pred>=result$Pred[count])) 
#     }
#     
#     
#   }
#   
#   result<-result[result$Strategy!="",]
#   
#   return(result)
#   
# }
# 
# 
# strategy<-function(trend_sample, data_sample, test_sample)
# 
# {
# 
#   for (i in 2:length(unique(test_sample$Date)))
# 
#   {
# 
# 
# 
# 
#     if (exists("sample_overall")==TRUE)
# 
#     {
# 
#       test_sample$Low<-data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$Low
#       test_sample$High<-data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$High
#       test_sample$Pred_TS<-data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$Pred
# 
# 
# 
#       sample_overall<-rbind(sample_overall, subset(test_sample, test_sample$Date==unique(test_sample$Date)[i] &
#                                                      test_sample$Koef>=data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$Low &
#                                                      test_sample$Koef<=data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$High &
#                                                      test_sample$Pred>=data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$Pred))
# 
#     }
# 
# 
# 
#     if (exists("sample_overall")==FALSE)
#     {
# 
#       test_sample$Low<-data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$Low
#       test_sample$High<-data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$High
#       test_sample$Pred_TS<-data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$Pred
#       sample_overall<-subset(test_sample, test_sample$Date==unique(test_sample$Date)[i] &
#                                test_sample$Koef>=data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$Low &
#                                test_sample$Koef<=data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$High &
#                                test_sample$Pred>=data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$Pred)
# 
#     }
# 
#   }
# 
#   return(sample_overall)
# 
# }
# 
# 
# strategy<-function(trend_sample, data_sample, test_sample)
#   
# {
#   
#   for (i in 2:length(unique(test_sample$Date)))
#     
#   {
#     
#     
#     
#     
#     if (exists("sample_overall")==TRUE)
#       
#     {
#       
#       
#       test_sample$Pred_TS<-data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$Pred
#       
#       
#       
#       sample_overall<-rbind(sample_overall, subset(test_sample, test_sample$Date==unique(test_sample$Date)[i] &
#                                                      test_sample$Pred>=data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$Pred))
#       
#     }
#     
#     
#     
#     if (exists("sample_overall")==FALSE)
#     {
#       
#       test_sample$Pred_TS<-data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$Pred
#       sample_overall<-subset(test_sample, test_sample$Date==unique(test_sample$Date)[i] &
#                                test_sample$Pred>=data_sample[min(which(trend_sample[,i-1]==max(trend_sample[,i-1]))),]$Pred)
#       
#     }
#     
#   }
#   
#   return(sample_overall)
#   
# }
# 
# strategy<-function(data_sample, test_sample)
# 
# {
# 
#   for (i in 3:length(unique(test_sample$Date)))
# 
#   {
# 
#   
#     data<-data_sample[with(data_sample, order(data_sample[,i-1], decreasing = TRUE)),][c(1:3),]
#     tr<-trend(subset(data, data$Count>=10),5)
# 
#     if (exists("sample_overall")==TRUE)
# 
#     {
# 
#       #test_sample$Low<-data[tr[,i-1]==max(tr[,i-1]),]$Low[1]
#       #test_sample$High<-data[tr[,i-1]==max(tr[,i-1]),]$High[1]
#       test_sample$Pred_TS<-data[tr[,i-1]==max(tr[,i-1]),]$Pred[1]
#       sample_overall<-rbind(sample_overall, subset(test_sample, test_sample$Date==unique(test_sample$Date)[i] &
#                                                    test_sample$Pred>=data[tr[,i-1]==max(tr[,i-1]),]$Pred[1]))
# 
#     }
# 
# 
# 
#     if (exists("sample_overall")==FALSE)
#     {
# 
#     #  test_sample$Low<-data[tr[,i-1]==max(tr[,i-1]),]$Low[1]
#     #  test_sample$High<-data[tr[,i-1]==max(tr[,i-1]),]$High[1]
#       test_sample$Pred_TS<-data[tr[,i-1]==max(tr[,i-1]),]$Pred[1]
#       sample_overall<-subset(test_sample, test_sample$Date==unique(test_sample$Date)[i] &
#                              test_sample$Pred>=data[tr[,i-1]==max(tr[,i-1]),]$Pred[1])
# 
#     }
# 
#   }
# 
#   return(sample_overall)
# 
# }
# 
# ml<-function (i)
#   
# {
#   data<-read.csv("Factors_Calculation_Data_SK_New.csv",sep=";",h=T)
#   data$Date<-as.Date(data$Date,"%d.%m.%Y")
#   
#   colnames(data)[30]<-"Flag"
#   
#   validation<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Season!=i)
#   control<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Season==i)
#   validation<-validation[with(validation,order(validation$Date)),]
#   control<-control[with(control,order(control$Date)),]
#   
#   validation_t<-validation[,c("Flag",
#                               #"Win",
#                               "W_Points_T1_All",
#                               "W_Points_T2_All",
#                               "W_Total_Win_T1_All",
#                               "W_Total_Win_T2_All",
#                               "W_Total_Defeat_T1_All",
#                               "W_Total_Defeat_T2_All",
#                               "W_Plus_Goals_T1_All",
#                               "W_Plus_Goals_T2_All",
#                               "W_Minus_Goals_T1_All",
#                               "W_Minus_Goals_T2_All",
#                               "W_Plus_Shots_T1_All",
#                               "W_Plus_Shots_T2_All",
#                               "W_Minus_Shots_T1_All",
#                               "W_Minus_Shots_T2_All",
#                               "W_Plus_TShots_T1_All",
#                               "W_Plus_TShots_T2_All",
#                               "W_Minus_TShots_T1_All",
#                               "W_Minus_TShots_T2_All",
#                               "W_Plus_Corners_T1_All",
#                               "W_Plus_Corners_T2_All",
#                               "W_Minus_Corners_T1_All",
#                               "W_Minus_Corners_T2_All",
#                               "W_Plus_HGoals_T1_All",
#                               "W_Plus_HGoals_T2_All",
#                               "W_Minus_HGoals_T1_All",
#                               "W_Minus_HGoals_T2_All",
#                               "W_Plus_YCards_T1_All",
#                               "W_Plus_YCards_T2_All",
#                               "W_Minus_YCards_T1_All",
#                               "W_Minus_YCards_T2_All",
#                               "W_Plus_RCards_T1_All",
#                               "W_Plus_RCards_T2_All",
#                               "W_Minus_RCards_T1_All",
#                               "W_Minus_RCards_T2_All",
#                               "W_Plus_Fouls_T1_All",
#                               "W_Plus_Fouls_T2_All",
#                               "W_Minus_Fouls_T1_All",
#                               "W_Minus_Fouls_T2_All")]
#   
#   
#                                   
#                                   control_t<-control[,c( #"Win",
#                                     "W_Points_T1_All",
#                                     "W_Points_T2_All",
#                                     "W_Total_Win_T1_All",
#                                     "W_Total_Win_T2_All",
#                                     "W_Total_Defeat_T1_All",
#                                     "W_Total_Defeat_T2_All",
#                                     "W_Plus_Goals_T1_All",
#                                     "W_Plus_Goals_T2_All",
#                                     "W_Minus_Goals_T1_All",
#                                     "W_Minus_Goals_T2_All",
#                                     "W_Plus_Shots_T1_All",
#                                     "W_Plus_Shots_T2_All",
#                                     "W_Minus_Shots_T1_All",
#                                     "W_Minus_Shots_T2_All",
#                                     "W_Plus_TShots_T1_All",
#                                     "W_Plus_TShots_T2_All",
#                                     "W_Minus_TShots_T1_All",
#                                     "W_Minus_TShots_T2_All",
#                                     "W_Plus_Corners_T1_All",
#                                     "W_Plus_Corners_T2_All",
#                                     "W_Minus_Corners_T1_All",
#                                     "W_Minus_Corners_T2_All",
#                                     "W_Plus_HGoals_T1_All",
#                                     "W_Plus_HGoals_T2_All",
#                                     "W_Minus_HGoals_T1_All",
#                                     "W_Minus_HGoals_T2_All",
#                                     "W_Plus_YCards_T1_All",
#                                     "W_Plus_YCards_T2_All",
#                                     "W_Minus_YCards_T1_All",
#                                     "W_Minus_YCards_T2_All",
#                                     "W_Plus_RCards_T1_All",
#                                     "W_Plus_RCards_T2_All",
#                                     "W_Minus_RCards_T1_All",
#                                     "W_Minus_RCards_T2_All",
#                                     "W_Plus_Fouls_T1_All",
#                                     "W_Plus_Fouls_T2_All",
#                                     "W_Minus_Fouls_T1_All",
#                                     "W_Minus_Fouls_T2_All")]
#   
#   control_Date<-control$Date
#   control_Team_1<-control$Team_1
#   control_Team_2<-control$Team_2
#   control_Goal_1<-control$Goal_1
#   control_Goal_2<-control$Goal_2
#   control_Flag<-control$Flag
#   control_Koef<-control$Win
# 
#   train_sample<-validation_t
#   control_sample<-control_t
#   
#   xgb <- xgboost(data = data.matrix(train_sample %>% select(-Flag)), label = train_sample$Flag, eta = 0.01,
#                  max_depth =10, gamma = 10, subsample = 0.6, colsample_bytree =0.5, min_child_weight = 5,
#                  nthread = 2, nrounds = 1000, eval_metric = "auc",objective='binary:logistic') 
#   
#   sample=data.frame(control_Date,  
#                     control_Team_1, 
#                     control_Team_2, 
#                     control_Goal_1, 
#                     control_Goal_2, 
#                     control_Flag,
#                     predict(xgb, data.matrix(control_sample)),
#                     control_Koef)
#   
# 
#   names(sample)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef")
#   sample<-sample[with(sample,order(sample$Pred, decreasing = TRUE)),]
#   plot.roc(sample$Flag, sample$Pred,print.auc=T) 
#   sample<-sample[with(sample,order(sample$Date)),]
#   
#   return (sample)
#   
# }

#setwd("/root/")

E0<-read_excel(paste(directory, "football_past.xlsx", sep=""), sheet="E0")
E1<-read_excel(paste(directory, "football_past.xlsx", sep=""), sheet="E1")
# E2<-read_excel("all-euro-data-2019-2020.xlsx", sheet="E2")
# E3<-read_excel("all-euro-data-2019-2020.xlsx", sheet="E3")
# SC0<-read_excel("all-euro-data-2019-2020.xlsx", sheet="SC0")
# SC1<-read_excel("all-euro-data-2019-2020.xlsx", sheet="SC1")
# SC2<-read_excel("all-euro-data-2019-2020.xlsx", sheet="SC2")
# SC3<-read_excel("all-euro-data-2019-2020.xlsx", sheet="SC3")
D1<-read_excel(paste(directory, "football_past.xlsx", sep=""), sheet="D1")
# D2<-read_excel("all-euro-data-2019-2020.xlsx", sheet="D2")
I1<-read_excel(paste(directory, "football_past.xlsx", sep=""), sheet="I1")
#I2<-read_excel("all-euro-data-2019-2020.xlsx", sheet="I2")
SP1<-read_excel(paste(directory, "football_past.xlsx", sep=""), sheet="SP1")
#SP2<-read_excel("all-euro-data-2019-2020.xlsx", sheet="SP2")
F1<-read_excel(paste(directory, "football_past.xlsx", sep=""), sheet="F1")
#F2<-read_excel("all-euro-data-2019-2020.xlsx", sheet="F2")
# B1<-read_excel("all-euro-data-2019-2020.xlsx", sheet="B1")
# N1<-read_excel("all-euro-data-2019-2020.xlsx", sheet="N1")
# P1<-read_excel("all-euro-data-2019-2020.xlsx", sheet="P1")
# T1<-read_excel("all-euro-data-2019-2020.xlsx", sheet="T1")
# G1<-read_excel("all-euro-data-2019-2020.xlsx", sheet="G1")

# names(G1)[names(G1) == "HFKC"] <- "HF"
# names(G1)[names(G1) == "AFKC"] <- "AF"
# names(B1)[names(B1) == "HFKC"] <- "HF"
# names(B1)[names(B1) == "AFKC"] <- "AF"

past<-rbind(data.frame(), E0[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",     
                 "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A", "Max>2.5", "Max<2.5")])

past<-rbind(past, E1[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",     
      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A", "Max>2.5", "Max<2.5")])

# past<-rbind(past,E2[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",
#                  "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])
# 
# past<-rbind(past,E3[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])
# 
# past<-rbind(past,SC0[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])
# 
# past<-rbind(past,SC1[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])
# 
# past<-rbind(past,SC2[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])
# 
# past<-rbind(past,SC3[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])

past<-rbind(past,D1[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",     
                     "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A", "Max>2.5", "Max<2.5")])

# past<-rbind(past,D2[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",     
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])

past<-rbind(past,I1[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",     
                     "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A", "Max>2.5", "Max<2.5")])

# past<-rbind(past,I2[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",     
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])

past<-rbind(past,SP1[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",     
                     "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A", "Max>2.5", "Max<2.5")])

# past<-rbind(past,SP2[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])

past<-rbind(past,F1[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",     
                     "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A", "Max>2.5", "Max<2.5")])

# past<-rbind(past,F2[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",     
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])

# past<-rbind(past,B1[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])
# 
# past<-rbind(past,N1[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])
# 
# past<-rbind(past,P1[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])
# 
# past<-rbind(past,G1[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])
# 
# past<-rbind(past,T1[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR", "HS","AS","HST",
#                      "AST","HF","AF","HC","AC","HY","AY","HR","AR","B365H","B365D","B365A")])

#leag="B1"

#mean(subset(past, past$League==leag & past$Goal_1>past$Goal_2)$Win, na.rm = TRUE)*(nrow(subset(past, past$League==leag & past$Goal_1>past$Goal_2))/nrow(subset(past, past$League==leag)))

#past<-past[with(past,order(past$Date)),]

#cot<-read_excel("All_Season_past_SK.xlsx")

colnames(past)<-c("League", "Date", "Team_1","Team_2","Goal_1","Goal_2","FTR","HTHG","HTAG","HTR","HS","AS","HST","AST",     
                 "HF","AF", "HC",   "AC",   "HY",    "AY",    "HR",    "AR", "Win", "Draw", "Defeat", "BbMx_More_2", "BbMx_Less_2")


future<-read_excel(paste(directory, "football_future.xlsx", sep=""))

time<-data.frame(future[,c("HomeTeam", "AwayTeam", "Time")])
time$Time<-substr(as.character(time$Time), 12, 20)
colnames(time)<-c("Team_1", "Team_2", "Time")

future<-data.frame(future$Div,
                   future$Date,
                   future$HomeTeam,
                   future$AwayTeam,
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   future$B365H,
                   future$B365D,
                   future$B365A,
                   future$`Max>2.5`, 
                   future$`Max<2.5`)  

colnames(future)<-c("League", "Date", "Team_1","Team_2","Goal_1","Goal_2","FTR","HTHG","HTAG","HTR","HS","AS","HST","AST",     
                  "HF","AF", "HC",   "AC",   "HY",    "AY",    "HR",    "AR", "Win", "Draw", "Defeat", "BbMx_More_2", "BbMx_Less_2")

future<-subset(future, future$League %in% c("E0", "E1", "D1", "SP1", "I1", "F1"))
  
new<-rbind(past, future)

#cot$Date<-as.Date(cot$Date,"%d.%m.%Y")
new<-new[with(new,order(new$Date)),]

cot<-new

cot$Points_1<-ifelse(cot$Goal_1>cot$Goal_2,3,ifelse(cot$Goal_1==cot$Goal_2,1,0))
cot$Points_2<-ifelse(cot$Goal_1<cot$Goal_2,3,ifelse(cot$Goal_1==cot$Goal_2,1,0))
cot$Flag_Win<-ifelse(cot$Goal_1>cot$Goal_2,1,0)
cot$Flag_Draw<-ifelse(cot$Goal_1==cot$Goal_2,1,0)
cot$Flag_Defeat<-ifelse(cot$Goal_1<cot$Goal_2,1,0)

cot$Total_more_2<-ifelse(cot$Goal_1+cot$Goal_2>2,1,0)
cot$Total_more_3<-ifelse(cot$Goal_1+cot$Goal_2>3,1,0)
cot$Total_more_4<-ifelse(cot$Goal_1+cot$Goal_2>4,1,0)
cot$Total_more_5<-ifelse(cot$Goal_1+cot$Goal_2>5,1,0)

cot$Total_less_2<-ifelse(cot$Goal_1+cot$Goal_2<=2,1,0)

cot$Flag_Both_scored<-ifelse(cot$Goal_1>0 & cot$Goal_2>0,1,0)

cot$Flag_Ind_total_Team1_more_0<-ifelse(cot$Goal_1>0,1,0)
cot$Flag_Ind_total_Team2_more_0<-ifelse(cot$Goal_2>0,1,0)

cot$Flag_Ind_total_Team1_more_1<-ifelse(cot$Goal_1>1,1,0)
cot$Flag_Ind_total_Team2_more_1<-ifelse(cot$Goal_2>1,1,0)

cot$Flag_1X<-ifelse(cot$Goal_1-cot$Goal_2>=0,1,0)
cot$Flag_2X<-ifelse(cot$Goal_2-cot$Goal_1>=0,1,0)

cot$Koef_1X<-1/(1/cot$Win+1/cot$Draw)
cot$Koef_2X<-1/(1/cot$Defeat+1/cot$Draw)

cot$Flag_1X_2_More<-ifelse(cot$Goal_1-cot$Goal_2>=0 & cot$Goal_1+cot$Goal_2>=2,1,0)
cot$Flag_2X_2_More<-ifelse(cot$Goal_2-cot$Goal_1>=0 & cot$Goal_1+cot$Goal_2>=2,1,0)
cot$Koef_1X_2_More<-1/(1/cot$Win+1/cot$Draw)*cot$BbMx_More_2/1.6
cot$Koef_2X_2_More<-1/(1/cot$Defeat+1/cot$Draw)*cot$BbMx_More_2/1.6

cot$Flag_1X_2.5_More<-ifelse(cot$Goal_1-cot$Goal_2>=0 & cot$Goal_1+cot$Goal_2>2,1,0)
cot$Flag_2X_2.5_More<-ifelse(cot$Goal_2-cot$Goal_1>=0 & cot$Goal_1+cot$Goal_2>2,1,0)
cot$Koef_1X_2.5_More<-1/(1/cot$Win+1/cot$Draw)*cot$BbMx_More_2
cot$Koef_2X_2.5_More<-1/(1/cot$Defeat+1/cot$Draw)*cot$BbMx_More_2

cot$Flag_1X_2.5_Less<-ifelse(cot$Goal_1-cot$Goal_2>=0 & cot$Goal_1+cot$Goal_2<=2,1,0)
cot$Flag_2X_2.5_Less<-ifelse(cot$Goal_2-cot$Goal_1>=0 & cot$Goal_1+cot$Goal_2<=2,1,0)
cot$Koef_1X_2.5_Less<-1/(1/cot$Win+1/cot$Draw)*cot$BbMx_Less_2
cot$Koef_2X_2.5_Less<-1/(1/cot$Defeat+1/cot$Draw)*cot$BbMx_Less_2

cot$Flag_W1_2.5_Less<-ifelse(cot$Goal_1-cot$Goal_2>0 & cot$Goal_1+cot$Goal_2<=2,1,0)
cot$Flag_W2_2.5_Less<-ifelse(cot$Goal_2-cot$Goal_1>0 & cot$Goal_1+cot$Goal_2<=2,1,0)
cot$Koef_W1_2.5_Less<-1/(1/cot$Win+1/cot$Draw)*cot$BbMx_Less_2
cot$Koef_W2_2.5_Less<-1/(1/cot$Defeat+1/cot$Draw)*cot$BbMx_Less_2

cot$Flag_W1_2.5_More<-ifelse(cot$Goal_1-cot$Goal_2>0 & cot$Goal_1+cot$Goal_2>2,1,0)
cot$Flag_W2_2.5_More<-ifelse(cot$Goal_2-cot$Goal_1>0 & cot$Goal_1+cot$Goal_2>2,1,0)
cot$Koef_W1_2.5_More<-1/(1/cot$Win+1/cot$Draw)*cot$BbMx_More_2
cot$Koef_W2_2.5_More<-1/(1/cot$Defeat+1/cot$Draw)*cot$BbMx_More_2

cot$Flag_W1_1.5_More<-ifelse(cot$Goal_1-cot$Goal_2>0 & cot$Goal_1+cot$Goal_2>=2,1,0)
cot$Flag_W2_1.5_More<-ifelse(cot$Goal_2-cot$Goal_1>0 & cot$Goal_1+cot$Goal_2>=2,1,0)
cot$Koef_W1_1.5_More<-1/(1/cot$Win+1/cot$Draw)*cot$BbMx_More_2/1.6
cot$Koef_W2_1.5_More<-1/(1/cot$Defeat+1/cot$Draw)*cot$BbMx_More_2/1.6

cot$Season<-14

n<-5

result<-data.frame()

# for (i in 1:length(unique(cot$Season)))
# 
#   {
  
 # cot_2<-subset(cot, cot$Season==unique(cot$Season)[i])
  
  cot_2<-cot

  for (j in 1:nrow(cot_2))
  {
    
    
    
    # ???????????????????? ?????????????????? ???????????? ?????? 1 ??????????????
    
    cot_2$Match_T1_All[j]<-nrow(rbind(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))))),
                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))))
    
    # ???????????????????? ?????????????????? ???????????? ?????? 2 ??????????????
    
    cot_2$Match_T2_All[j]<-nrow(rbind(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j]))))),
                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))))
    
    
    # ???????????????????? ?????????????????? ???????????????? ???????????? ?????? 1 ??????????????
    
    cot_2$Match_T1_Home[j]<-nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & cot_2$Team_1==cot_2$Team_1[j]))
    
    
    # ???????????????????? ?????????????????? ???????????????? ???????????? ?????? 2 ??????????????
    
    
    cot_2$Match_T2_Guest[j]<-nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & cot_2$Team_2==cot_2$Team_2[j]))
    
  }
  
  
  
  
  ########################################################################## ???????? ####################################################################
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Points_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Points_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Points_2))
    
    cot_2$W_Points_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Points_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Points_1))
    
    
    
  } 
  ##################################################################### ?????????????? ???????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_Goals_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Goal_1,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Goal_2))
    
    cot_2$W_Plus_Goals_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Goal_2,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Goal_1))
    
    
    
  }     
  ##################################################################### ?????????????????????? ???????? ####################################################################      
  for (j in 1:nrow(cot_2))
  {  
    
    
    cot_2$W_Minus_Goals_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Goal_2,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Goal_1))
    
    cot_2$W_Minus_Goals_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Goal_1,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Goal_2))
    
    
    
  }
  
  
  ##################################################################### ???????????? ####################################################################    
  
  ##################################################################### ???????????? 1.5 ?? ####################################################################     
  
  for (j in 1:nrow(cot_2))
  { 
    
    
    
    cot_2$W_Total_More_2_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_2,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_2))
    
    
    cot_2$W_Total_More_2_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_2,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_2))
    
  }
  
  ##################################################################### ???????????? 2.5 ?? ####################################################################       
  
  for (j in 1:nrow(cot_2))
  { 
    
    
    cot_2$W_Total_more_3_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_3,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_3))
    
    cot_2$W_Total_more_3_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_3,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_3))
    
  }
  
  ##################################################################### ???????????? 3.5 ?? ####################################################################   
  
  for (j in 1:nrow(cot_2))
  { 
    
    
    cot_2$W_Total_more_4_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_4,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_4))
    
    cot_2$W_Total_more_4_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_4,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_4))
    
  }
  
  
  ##################################################################### ???????????? 4.5 ?? ####################################################################   
  
  for (j in 1:nrow(cot_2))
  { 
    
    
    cot_2$W_Total_more_5_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_5,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_5))
    
    cot_2$W_Total_more_5_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_5,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_5))
    
    
  }
  
  
  ##################################################################### ???????????????????????????? ???????????? 0.5 ?? ####################################################################   
  
  for (j in 1:nrow(cot_2))
  { 
    
    
    cot_2$W_Ind_Total_more_0_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Ind_total_Team1_more_0,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Ind_total_Team2_more_0))
    
    
    cot_2$W_Ind_Total_more_0_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Ind_total_Team2_more_0,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Ind_total_Team1_more_0))
    
    
  }
  
  ##################################################################### ???????????????????????????? ???????????? 1.5 ?? #################################################################### 
  
  for (j in 1:nrow(cot_2))
  { 
    
    
    cot_2$W_Ind_Total_more_1_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Ind_total_Team1_more_1,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Ind_total_Team2_more_1))
    
    
    cot_2$W_Ind_Total_more_1_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Ind_total_Team2_more_1,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Ind_total_Team1_more_1))
    
    
  }
  
  ##################################################################### ?????? ???????????? #################################################################### 
  
  for (j in 1:nrow(cot_2))
  { 
    
    
    cot_2$W_Both_Scored_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Both_scored,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Both_scored))
    
    
    cot_2$W_Both_Scored_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Both_scored,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Both_scored))
    
  }  
  
  
  ##################################################################### ???????????? ####################################################################  
  
  for (j in 1:nrow(cot_2))
  { 
    
    
    cot_2$W_Total_Win_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_Win,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_Defeat))
    
    cot_2$W_Total_Win_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Defeat,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_Win))
    
  }  
  
  
  ##################################################################### ?????????? ####################################################################  
  
  for (j in 1:nrow(cot_2))
  { 
    
    
    cot_2$W_Total_Draw_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_Draw,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_Draw))
    
    
    cot_2$W_Total_Draw_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Draw,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_Draw))
    
    
  }  
  
  ##################################################################### ?????????????????? ####################################################################  
  
  for (j in 1:nrow(cot_2))
  { 
    
    
    cot_2$W_Total_Defeat_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_Defeat,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_Win))
    
    
    cot_2$W_Total_Defeat_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Win,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_Defeat))
    
    
  }
  
  
  
  ##################################################################### 1X ####################################################################  
  
  for (j in 1:nrow(cot_2))
  { 
    
    
    cot_2$W_Total_1X_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_1x,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_2x))
    
    cot_2$W_Total_1X_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_2x,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_1x))
    
  }
  
  
  ##################################################################### 2X ####################################################################  
  
  
  for (j in 1:nrow(cot_2))
  { 
    
    
    cot_2$W_Total_2X_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_2x,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_1x))
    
    cot_2$W_Total_2X_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_1x,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_2x))
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_HGoals_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HTHG,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HTAG))
    
    
    cot_2$W_Plus_HGoals_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HTAG,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HTHG))
    
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {  
    
    
    cot_2$W_Minus_HGoals_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HTAG,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HTHG))
    
    cot_2$W_Minus_HGoals_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HTHG,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HTAG))
    
    
    
  }
  
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_Shots_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HS,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$AS))
    
    cot_2$W_Plus_Shots_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$AS,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HS))
    
    
    
  } 
  
  for (j in 1:nrow(cot_2))
  {  
    
    
    cot_2$W_Minus_Shots_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$AS,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HS))
    
    cot_2$W_Minus_Shots_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HS,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$AS))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_TShots_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HST,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$AST))
    
    
    cot_2$W_Plus_TShots_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$AST,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HST))
    
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {  
    
    
    cot_2$W_Minus_TShots_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$AST,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HST))
    
    cot_2$W_Minus_TShots_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HST,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$AST))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    cot_2$W_Plus_Fouls_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HF,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$AF))
    
    cot_2$W_Plus_Fouls_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$AF,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HF))
    
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {  
    
    
    cot_2$W_Minus_Fouls_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$AF,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HF))
    
    
    cot_2$W_Minus_Fouls_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HF,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$AF))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_Corners_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HC,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$AC))
    
    cot_2$W_Plus_Corners_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$AC,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HC))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {  
    
    cot_2$W_Minus_Corners_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$AC,
                                                     subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HC))
    
    cot_2$W_Minus_Corners_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HC,
                                                     subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$AC))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_YCards_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HY,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$AY))
    
    
    cot_2$W_Plus_YCards_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$AY,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HY))
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {  
    
    
    cot_2$W_Minus_YCards_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$AY,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HY))
    
    
    cot_2$W_Minus_YCards_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HY,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$AY))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    cot_2$W_Plus_RCards_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HR,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$AR))
    
    
    cot_2$W_Plus_RCards_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$AR,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HR))
    
  }
  
  for (j in 1:nrow(cot_2))
  {  
    
    
    cot_2$W_Minus_RCards_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$AR,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HR))
    
    
    cot_2$W_Minus_RCards_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HR,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$AR))
    
  }
  
  
  for (j in 1:nrow(cot_2))
  { 
    
    
    cot_2$Flag_Win_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Flag_Win==0))==0,
                                  ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                  nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                    max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Flag_Win==0)))
    
    cot_2$Flag_W1_2.5_More_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Flag_W1_2.5_More==0))==0,
                                          ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                          nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                            max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Flag_W1_2.5_More==0)))
    
    cot_2$Flag_W1_2.5_Less_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Flag_W1_2.5_Less==0))==0,
                                          ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                          nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                            max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Flag_W1_2.5_Less==0)))
    
    cot_2$Flag_Defeat_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Flag_Defeat==0))==0,
                                     ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                     nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                       max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Flag_Defeat==0)))
    
    cot_2$Flag_W2_2.5_More_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Flag_W2_2.5_More==0))==0,
                                          ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                          nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                            max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Flag_W2_2.5_More==0)))
    
    cot_2$Flag_W2_2.5_Less_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Flag_W2_2.5_Less==0))==0,
                                          ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                          nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                            max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Flag_W2_2.5_Less==0)))
    
    cot_2$Flag_1X_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Flag_1X==0))==0,
                                 ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                 nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                   max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Flag_1X==0)))
    
    cot_2$Flag_1X_2.5_More_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Flag_1X_2.5_More==0))==0,
                                          ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                          nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                            max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Flag_1X_2.5_More==0)))
    
    cot_2$Flag_1X_2.5_Less_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Flag_1X_2.5_Less==0))==0,
                                          ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                          nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                            max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Flag_1X_2.5_Less==0)))
    
    cot_2$Flag_2X_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Flag_2X==0))==0,
                                 ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                 nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                   max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Flag_2X==0)))
    
    cot_2$Flag_2X_2.5_More_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Flag_2X_2.5_More==0))==0,
                                          ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                          nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                            max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Flag_2X_2.5_More==0)))
    
    cot_2$Flag_2X_2.5_Less_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Flag_2X_2.5_Less==0))==0,
                                          ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                          nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                            max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Flag_2X_2.5_Less==0)))
    
    
    cot_2$Flag_2.5_More_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Total_more_2==0))==0,
                                       ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                       nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                         max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Total_more_2==0)))
    
    cot_2$Flag_2.5_Less_row[j]<-ifelse(nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]) & cot_2$Total_less_2==0))==0,
                                       ifelse(nrow(cot_2)==0,0,nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))),
                                       nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))-
                                         max(which(subset(cot_2, cot_2$Date<cot_2$Date[j] & as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))$Total_less_2==0)))
    
    
  
  }
  

  
  result<-rbind(result,cot_2) 

#}

  result$Month<-as.POSIXlt(result$Date)$mon+1
  
#write.csv(result, "Factors_Calculation_Data_SK_New.csv", row.names = FALSE)

#data<-read.csv("Factors_Calculation_Data_SK_New.csv",sep=",",h=T)
#data$Date<-as.Date(data$Date,"%Y-%m-%d")

#data$Month<-as.POSIXlt(data$Date)$mon+1
#colnames(data)[match("Flag_Defeat",colnames(data))]<-"Flag"

check<-function (League_list, Season_list, Flag_filter, Koef_filter, TS, pred_index, koef_index)

  
{

  # League_list<-c("SP1", "I1", "E1",  "F1", "D1", "E0")
  # season<-Season_list
  # Flag_filter<-"Flag_1X_2.5_Less"
  # Koef_filter<-"Koef_1X_2.5_Less"
  # TS<-0.84
  # pred_index<-0.55
  # koef_index<-1.0
  
  
data<-read.csv(paste(directory, "Factors_Calculation_Data_SK_New.csv", sep=""), sep=",",h=T)
data$Date<-as.Date(data$Date,"%Y-%m-%d")
data$Month<-as.POSIXlt(data$Date)$mon+1

data$Flag_1X_2_More<-ifelse(data$Goal_1-data$Goal_2>=0 & data$Goal_1+data$Goal_2>=2,1,0)
data$Flag_2X_2_More<-ifelse(data$Goal_2-data$Goal_1>=0 & data$Goal_1+data$Goal_2>=2,1,0)
data$Koef_1X_2_More<-1/(1/data$Win+1/data$Draw)*data$BbMx_More_2/1.6
data$Koef_2X_2_More<-1/(1/data$Defeat+1/data$Draw)*data$BbMx_More_2/1.6

data$Flag_1X_2.5_More<-ifelse(data$Goal_1-data$Goal_2>=0 & data$Goal_1+data$Goal_2>2,1,0)
data$Flag_2X_2.5_More<-ifelse(data$Goal_2-data$Goal_1>=0 & data$Goal_1+data$Goal_2>2,1,0)
data$Koef_1X_2.5_More<-1/(1/data$Win+1/data$Draw)*data$BbMx_More_2
data$Koef_2X_2.5_More<-1/(1/data$Defeat+1/data$Draw)*data$BbMx_More_2

data$Flag_1X_2.5_Less<-ifelse(data$Goal_1-data$Goal_2>=0 & data$Goal_1+data$Goal_2<=2,1,0)
data$Flag_2X_2.5_Less<-ifelse(data$Goal_2-data$Goal_1>=0 & data$Goal_1+data$Goal_2<=2,1,0)
data$Koef_1X_2.5_Less<-1/(1/data$Win+1/data$Draw)*data$BbMx_Less_2
data$Koef_2X_2.5_Less<-1/(1/data$Defeat+data$Draw)*data$BbMx_Less_2

data$Flag_W1_2.5_Less<-ifelse(data$Goal_1-data$Goal_2>0 & data$Goal_1+data$Goal_2<=2,1,0)
data$Flag_W2_2.5_Less<-ifelse(data$Goal_2-data$Goal_1>0 & data$Goal_1+data$Goal_2<=2,1,0)
data$Koef_W1_2.5_Less<-data$Win*data$BbMx_Less_2
data$Koef_W2_2.5_Less<-data$Defeat*data$BbMx_Less_2

data$Flag_W1_2.5_More<-ifelse(data$Goal_1-data$Goal_2>0 & data$Goal_1+data$Goal_2>2,1,0)
data$Flag_W2_2.5_More<-ifelse(data$Goal_2-data$Goal_1>0 & data$Goal_1+data$Goal_2>2,1,0)
data$Koef_W1_2.5_More<-data$Win*data$BbMx_More_2
data$Koef_W2_2.5_More<-data$Defeat*data$BbMx_More_2

data$Flag_W1_1.5_More<-ifelse(data$Goal_1-data$Goal_2>0 & data$Goal_1+data$Goal_2>=2,1,0)
data$Flag_W2_1.5_More<-ifelse(data$Goal_2-data$Goal_1>0 & data$Goal_1+data$Goal_2>=2,1,0)
data$Koef_W1_1.5_More<-data$Win*data$BbMx_More_2/1.6
data$Koef_W2_1.5_More<-data$Defeat*data$BbMx_More_2/1.6

data$Total_less_2<-ifelse(data$Goal_1+data$Goal_2<=2,1,0)

colnames(data)[match(Flag_filter ,colnames(data))]<-"Flag"

#validation<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Defeat>=1.0 & data$Season!=Season_list & data$Month!=5)
#control<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Defeat>=1.0 &  data$Season==Season_list & data$Month!=5 & data$League %in% League_list)

#validation<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Defeat>=1.0 & data$Season!=Season_list & data$Month!=5)
#control<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Defeat>=1.0 &  data$Season==Season_list & data$Month!=5 & data$League %in% League_list)

validation<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 & data$Defeat>=1.0 & data$Month!=5)
control<-subset(result, result$Match_T1_Home>=4 & result$Match_T2_Guest>=4 & result$Defeat>=1.0 & result$Month!=5 & result$League %in% League_list)
colnames(control)[match(Flag_filter ,colnames(control))]<-"Flag"

validation<-validation[with(validation,order(validation$Date)),]
control<-control[with(control,order(control$Date)),]

validation<-validation[,c("Flag",
                            "Win",
                            "Defeat",
                            "BbMx_Less_2",
                            "BbMx_More_2",
                            "W_Points_T1_All",
                            "W_Points_T2_All",
                            "W_Total_Win_T1_All",
                            "W_Total_Win_T2_All",
                            "W_Total_Defeat_T1_All",
                            "W_Total_Defeat_T2_All",
                            "W_Plus_Goals_T1_All",
                            "W_Plus_Goals_T2_All",
                            "W_Minus_Goals_T1_All",
                            "W_Minus_Goals_T2_All",
                            "W_Plus_Shots_T1_All",
                            "W_Plus_Shots_T2_All",
                            "W_Minus_Shots_T1_All",
                            "W_Minus_Shots_T2_All",
                            "W_Plus_TShots_T1_All",
                            "W_Plus_TShots_T2_All",
                            "W_Minus_TShots_T1_All",
                            "W_Minus_TShots_T2_All",
                            "W_Plus_Corners_T1_All",
                            "W_Plus_Corners_T2_All",
                            "W_Minus_Corners_T1_All",
                            "W_Minus_Corners_T2_All",
                            "W_Plus_HGoals_T1_All",
                            "W_Plus_HGoals_T2_All",
                            "W_Minus_HGoals_T1_All",
                            "W_Minus_HGoals_T2_All",
                            "W_Plus_YCards_T1_All",
                            "W_Plus_YCards_T2_All",
                            "W_Minus_YCards_T1_All",
                            "W_Minus_YCards_T2_All",
                            "W_Plus_RCards_T1_All",
                            "W_Plus_RCards_T2_All",
                            "W_Minus_RCards_T1_All",
                            "W_Minus_RCards_T2_All",
                            "W_Plus_Fouls_T1_All",
                            "W_Plus_Fouls_T2_All",
                            "W_Minus_Fouls_T1_All",
                            "W_Minus_Fouls_T2_All",
                            "W_Total_More_2_T1_All",
                            "W_Total_More_2_T2_All")]

control_League<-control$League
control_Date<-control$Date
control_Team_1<-control$Team_1
control_Team_2<-control$Team_2
control_Goal_1<-control$Goal_1
control_Goal_2<-control$Goal_2
control_Flag<-control$Flag
control_Koef<-control[,Koef_filter]
control_1x<-1/(1/control$Win+1/control$Draw)
control_2x<-1/(1/control$Defeat+1/control$Draw)

control_Win<-control$Win
control_Draw<-control$Draw
control_Defeat<-control$Defeat

control_BbMx_More_2<-control$BbMx_More_2
control_BbMx_More_1.5<-control$BbMx_More_2/1.6
control_BbMx_Less_2<-control$BbMx_Less_2
control_BbMx_Less_1.5<-control$BbMx_Less_2/1.6

control<-control[,c(  "Win",
                        "Defeat",
                        "BbMx_Less_2",
                        "BbMx_More_2",
                        "W_Points_T1_All",
                        "W_Points_T2_All",
                        "W_Total_Win_T1_All",
                        "W_Total_Win_T2_All",
                        "W_Total_Defeat_T1_All",
                        "W_Total_Defeat_T2_All",
                        "W_Plus_Goals_T1_All",
                        "W_Plus_Goals_T2_All",
                        "W_Minus_Goals_T1_All",
                        "W_Minus_Goals_T2_All",
                        "W_Plus_Shots_T1_All",
                        "W_Plus_Shots_T2_All",
                        "W_Minus_Shots_T1_All",
                        "W_Minus_Shots_T2_All",
                        "W_Plus_TShots_T1_All",
                        "W_Plus_TShots_T2_All",
                        "W_Minus_TShots_T1_All",
                        "W_Minus_TShots_T2_All",
                        "W_Plus_Corners_T1_All",
                        "W_Plus_Corners_T2_All",
                        "W_Minus_Corners_T1_All",
                        "W_Minus_Corners_T2_All",
                        "W_Plus_HGoals_T1_All",
                        "W_Plus_HGoals_T2_All",
                        "W_Minus_HGoals_T1_All",
                        "W_Minus_HGoals_T2_All",
                        "W_Plus_YCards_T1_All",
                        "W_Plus_YCards_T2_All",
                        "W_Minus_YCards_T1_All",
                        "W_Minus_YCards_T2_All",
                        "W_Plus_RCards_T1_All",
                        "W_Plus_RCards_T2_All",
                        "W_Minus_RCards_T1_All",
                        "W_Minus_RCards_T2_All",
                        "W_Plus_Fouls_T1_All",
                        "W_Plus_Fouls_T2_All",
                        "W_Minus_Fouls_T1_All",
                        "W_Minus_Fouls_T2_All",
                        "W_Total_More_2_T1_All",
                        "W_Total_More_2_T2_All")]

bst <- xgb.load(paste(directory, 'xgb.model_', Koef_filter, sep=""))

sample=data.frame(control_League,
                  control_Date,
                  control_Team_1,
                  control_Team_2,
                  control_Goal_1,
                  control_Goal_2,
                  control_Flag,
                  predict(bst, data.matrix(control)),
                  control_Koef,
                  control_1x,
                  control_2x,
                  control_Win,
                  control_Draw,
                  control_Defeat,
                  control_BbMx_More_1.5,
                  control_BbMx_More_2,
                  control_BbMx_Less_1.5,
                  control_BbMx_Less_2,
                  c(rep(Koef_filter, length(control_Koef))))

names(sample)<-c("League", "Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef",
                 "1_X", "2_X", "Win", "Draw", "Defeat", "1.5_More", "2.5_More", "1.5_Less", "2.5_Less", "Exodus")
sample<-sample[with(sample,order(sample$Date)),]
#plot.roc(sample$Flag, sample$Pred,print.auc=T)

#sample_Win<-subset(sample, sample$Pred>=TS)
#sample_Win<-sample_Win[with(sample_Win,order(sample_Win$Date)),]
#nrow(subset(sample_Win, sample_Win$Flag==1))/nrow(sample_Win)

#profit(sample_Win)
#profit(subset(sample_Win, sample_Win$League %in% c("SC0")))

#data_12<-find_best_strategy(sample,10,10,30)
#sample_Win_F<-strategy(trend(subset(data_12, data_12$Count>=10),5), subset(data_12, data_12$Count>=5), sample)
#sample<-subset(sample, sample$League %in% c("E0", "D1", "I1", "SP1", "F1"))

#sample_Win_F<-strategy(subset(data_12, data_12$Count>=10), sample)
#profit(sample_Win_F)

sample_Win<-subset(sample, sample$Pred>=pred_index & sample$Koef>=koef_index )

return (sample_Win)


}


sample_Win_F_overall<-data.frame()

sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_1X_2.5_Less",    "Koef_1X_2.5_Less",    0.84, 0.55, 1.0))
sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_2X_2.5_Less",    "Koef_2X_2.5_Less",    0.84, 0.64, 1.0))

sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_1X_2.5_More",    "Koef_1X_2.5_More",    0.84, 0.83, 1.0))
sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_2X_2.5_More",    "Koef_2X_2.5_More",    0.84, 0.58, 1.0))

sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_W1_2.5_More",    "Koef_W1_2.5_More",    0.84, 0.64, 1.0))
sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_W2_2.5_More",    "Koef_W2_2.5_More",    0.84, 0.53, 1.0))

sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_Win",            "Win",                 0.84, 0.85, 1.0))
sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_Defeat",         "Defeat",              0.84, 0.80, 1.0))

sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("F1", "E0"),                           NA, "Total_more_2",        "BbMx_More_2",         0.84, 0.69, 1.0))
sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("E1", "F1"),                           NA, "Total_less_2",        "BbMx_Less_2",         0.84, 0.69, 1.0))

sample_Win_F_overall<-sample_Win_F_overall[with(sample_Win_F_overall,order(sample_Win_F_overall$Date)),]
sample_Win_F_overall<-subset(sample_Win_F_overall, is.na(sample_Win_F_overall$Goal_1)==FALSE | (is.na(sample_Win_F_overall$Goal_1)==TRUE & as.Date(sample_Win_F_overall$Date)==Sys.Date()))

exit_my_model<-data.frame()

for (i in 1:length(unique(sample_Win_F_overall$Date)))
  
{
  
  list<-c("")
  
  
  for (j in 1:length(unique(sample_Win_F_overall$Exodus)))
    
  {
    
    
    section<-subset(sample_Win_F_overall, sample_Win_F_overall$Date<unique(sample_Win_F_overall$Date)[i] & sample_Win_F_overall$Exodus==unique(sample_Win_F_overall$Exodus)[j])
    
    if (nrow(section)>0)
    {
      
      
      if (profit(section)[length(profit(section))]>=1.0)
        
      {
        list<-c(list, as.character(unique(sample_Win_F_overall$Exodus)[j]))
      }
      
    }
    
  }
  exit_my_model<-rbind(exit_my_model, subset(sample_Win_F_overall, sample_Win_F_overall$Date==unique(sample_Win_F_overall$Date)[i] & sample_Win_F_overall$Exodus %in% list))
  
}

#profit(exit_my_model)

#############################################################################################################################################

# test_3_Less<-subset(result, result$Flag_2.5_Less_row>=2 &
#                       result$League %in% c("E0"))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "BbMx_Less_2", "Total_less_2")]
# names(test_3_Less)<-c("League", "Date", "Team_1", "Team_2",  "Goal_1", "Goal_2", "Koef", "Flag")
# test_3_Less$Exodus<-"BbMx_Less_2"
# profit(test_3_Less)

result<-subset(result, is.na(result$Goal_1)==FALSE | (is.na(result$Goal_1)==TRUE & as.Date(result$Date)==Sys.Date()))
data<-result

colnames(result)[match("Total_less_2" ,colnames(result))]<-"Flag"
colnames(result)[match("BbMx_Less_2" ,colnames(result))]<-"Koef"

test_3_Less<-data.frame()
list<-c("E0", "E1", "D1", "SC0", "SP1",  "F1", "I1")

    for (i in 1:length(unique(result$Date)))
      
    {
      
      max<-0
      lmax<-0
      mmax<-0
      short_list<-""
      
       for (l in 1:5)
      {
        
        for (k in 1:7)
          
        {
          
          
        section<-subset(result, result$Date<unique(result$Date)[i] & result$Koef<=2.00 &  result$Flag_2.5_Less_row>=l & result$League %in% c(short_list, list[k]))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef", "Flag")]
        
        if (nrow(section)>0)
        {
          
          
          if (profit(section)[length(profit(section))]>=max)
            
          {
            short_list<-c(short_list, list[k])
            max<-profit(section)[length(profit(section))]
            lmax<-l
            #mmax<-m
            
          }
          
        }
        
        }
        
       }
       
      test_3_Less<-rbind(test_3_Less, subset(result, result$Date==unique(result$Date)[i] & result$Koef<=2.00 & result$Flag_2.5_Less_row>=lmax & result$League %in% short_list)[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef", "Flag")])
     # print(paste("Date:", unique(result$Date)[i],  "L:", lmax, "short_list:", short_list, sep=" "))
}

test_3_Less$Exodus<-"BbMx_Less_2"

#############################################################################################################################################

# test_3_More<-subset(result, result$Flag_2.5_More_row>=2 & result$BbMx_More_2<=1.9 &
#                       result$League %in% c("E0", "SP1", "D1", "E1"))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "BbMx_More_2", "Total_more_2")]
# names(test_3_More)<-c("League", "Date", "Team_1", "Team_2",  "Goal_1", "Goal_2", "Koef", "Flag")
# test_3_More$Exodus<-"BbMx_More_2"
# profit(test_3_More)

result<-data
data<-result
colnames(result)[match("Total_more_2" ,colnames(result))]<-"Flag"
colnames(result)[match("BbMx_More_2" ,colnames(result))]<-"Koef"

test_3_More<-data.frame()
list<-c("E0", "E1", "D1", "SP1", "SC0", "F1", "I1")

for (i in 1:length(unique(result$Date)))
  
{
  
  max<-0
  lmax<-0
  mmax<-0
  short_list<-""
  
  for (l in 1:5)
  {
    
    for (k in 1:7)
      
    {
      
      
      section<-subset(result, result$Date<unique(result$Date)[i] & result$Koef<=1.80 &  result$Flag_2.5_More_row>=l & result$League %in% c(short_list, list[k]))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef", "Flag")]
      
      if (nrow(section)>0)
      {
        
        
        if (profit(section)[length(profit(section))]>=max)
          
        {
          short_list<-c(short_list, list[k])
          max<-profit(section)[length(profit(section))]
          lmax<-l
          #mmax<-m
          
        }
        
      }
      
    }
    
  }
  
  test_3_More<-rbind(test_3_More, subset(result, result$Date==unique(result$Date)[i] & result$Koef<=1.80 & result$Flag_2.5_More_row>=lmax & result$League %in% short_list)[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef", "Flag")])
  # print(paste("Date:", unique(result$Date)[i],  "L:", lmax, "short_list:", short_list, sep=" "))
}

test_3_More$Exodus<-"BbMx_More_2"

################################################################################# Win ###################################################################

# test_Win<-subset(result, result$Flag_Win_row>=4 &
#                    result$League %in% c("I1", "D1", "E0", "E1"))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Win", "Flag_Win")]
# names(test_Win)<-c("League", "Date", "Team_1", "Team_2",  "Goal_1", "Goal_2", "Koef", "Flag")
# test_Win$Exodus<-"Win"
#profit(test_Win)

result<-data
data<-result

colnames(result)[match("Flag_Win" ,colnames(result))]<-"Flag"
colnames(result)[match("Win" ,colnames(result))]<-"Koef"

test_Win<-data.frame()
list<-c("E0", "E1", "D1", "SP1", "SC0", "F1", "I1")

for (i in 1:length(unique(result$Date)))
  
{
  
  max<-0
  lmax<-0
  mmax<-0
  short_list<-""
  
  for (l in 2:6)
  {
    
    for (k in 1:7)
      
    {
      
      
      section<-subset(result, result$Date<unique(result$Date)[i] & result$Koef<=2.10 &  result$Flag_Win_row>=l & result$League %in% c(short_list, list[k]))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef", "Flag")]
      
      if (nrow(section)>0)
      {
        
        
        if (profit(section)[length(profit(section))]>=max)
          
        {
          short_list<-c(short_list, list[k])
          max<-profit(section)[length(profit(section))]
          lmax<-l
          #mmax<-m
          
        }
        
      }
      
    }
    
  }
  
  test_Win<-rbind(test_Win, subset(result, result$Date==unique(result$Date)[i] & result$Koef<=2.10 & result$Flag_Win_row>=lmax & result$League %in% short_list)[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef", "Flag")])
  # print(paste("Date:", unique(result$Date)[i],  "L:", lmax, "short_list:", short_list, sep=" "))
}

test_Win$Exodus<-"Win"

################################################################################# Defeat ###################################################################

# test_Defeat<-subset(result, result$Flag_Defeat_row>=2 & result$Flag_Defeat_row<=7  & result$Defeat <=2.5 & 
#                       result$League %in% c("F1", "SP1", "I1", "E0", "D1", "SC0", "E1"))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Defeat", "Flag_Defeat")]
# names(test_Defeat)<-c("League", "Date", "Team_1", "Team_2",  "Goal_1", "Goal_2", "Koef", "Flag")
# test_Defeat$Exodus<-"Defeat"
#profit(test_Defeat)

result<-data
data<-result

colnames(result)[match("Flag_Defeat" ,colnames(result))]<-"Flag"
colnames(result)[match("Defeat" ,colnames(result))]<-"Koef"

test_Defeat<-data.frame()
list<-c("E0", "E1", "D1", "SP1", "SC0", "F1", "I1")

for (i in 1:length(unique(result$Date)))
  
{
  
  max<-0
  lmax<-0
  mmax<-0
  short_list<-""
  
  for (l in 4:9)
  {
    
    for (k in 1:7)
      
    {
      
      
      section<-subset(result, result$Date<unique(result$Date)[i] & result$Koef<=2.50 & result$Flag_Defeat_row>=2 & result$Flag_Defeat_row<=l & result$League %in% c(short_list, list[k]))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef", "Flag")]
      
      if (nrow(section)>0)
      {
        
        
        if (profit(section)[length(profit(section))]>=max)
          
        {
          short_list<-c(short_list, list[k])
          max<-profit(section)[length(profit(section))]
          lmax<-l
          #mmax<-m
          
        }
        
      }
      
    }
    
  }
  
  test_Defeat<-rbind(test_Defeat, subset(result, result$Date==unique(result$Date)[i] & result$Koef<=2.50 & result$Flag_Defeat_row>=2 & result$Flag_Defeat_row<=lmax & result$League %in% short_list)[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef", "Flag")])
  # print(paste("Date:", unique(result$Date)[i],  "L:", lmax, "short_list:", short_list, sep=" "))
}

test_Defeat$Exodus<-"Defeat"

############################################################################### 1X 2.5 More ###################################################################
# 
# result<-data
# data<-result
# 
# test_1X_2.5_More<-subset(result, result$Flag_1X_2.5_More_row>=3  & result$Koef_1X_2.5_More <=2.2 &
#                            result$League %in% c("D1", "I1", "E1"))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef_1X_2.5_More", "Flag_1X_2.5_More")]
# names(test_1X_2.5_More)<-c("League", "Date", "Team_1", "Team_2",  "Goal_1", "Goal_2", "Koef", "Flag")
# test_1X_2.5_More$Exodus<-"Koef_1X_2.5_More"
# profit(test_1X_2.5_More)

# result<-data
# data<-result

# colnames(result)[match("Flag_1X_2.5_More" ,colnames(result))]<-"Flag"
# colnames(result)[match("Koef_1X_2.5_More" ,colnames(result))]<-"Koef"
# 
# test_1X_2.5_More<-data.frame()
# list<-c("E0", "E1", "D1", "SP1", "SC0", "F1", "I1")
# 
# for (i in 1:length(unique(result$Date)))
#   
# {
#   
#   max<-0
#   lmax<-0
#   mmax<-0
#   short_list<-""
#   
#   for (l in 4:9)
#   {
#     
#     for (k in 1:7)
#       
#     {
#       
#       
#       section<-subset(result, result$Date<unique(result$Date)[i] & result$Koef<=2.20 & result$Flag_1X_2.5_More_row<=l & result$League %in% c(short_list, list[k]))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef", "Flag")]
#       
#       if (nrow(section)>0)
#       {
#         
#         
#         if (profit(section)[length(profit(section))]>=max)
#           
#         {
#           short_list<-c(short_list, list[k])
#           max<-profit(section)[length(profit(section))]
#           lmax<-l
#           #mmax<-m
#           
#         }
#         
#       }
#       
#     }
#     
#   }
#   
#   test_1X_2.5_More<-rbind(test_1X_2.5_More, subset(result, result$Date==unique(result$Date)[i] & result$Koef<=2.20 & result$Flag_1X_2.5_More_row<=lmax & result$League %in% short_list)[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef", "Flag")])
#   # print(paste("Date:", unique(result$Date)[i],  "L:", lmax, "short_list:", short_list, sep=" "))
# }
# 
# short_list
# 
# profit(test_1X_2.5_More)


############################################################################### 1X 2.5 Less ###################################################################

# test_1X_2.5_Less<-subset(result, result$Flag_1X_2.5_Less_row>=3  & 
#                            result$Koef_1X_2.5_Less <=2.2 & 
#                            result$League %in% c("D1", "SP1"))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef_1X_2.5_Less", "Flag_1X_2.5_Less")]
# names(test_1X_2.5_Less)<-c("League", "Date", "Team_1", "Team_2",  "Goal_1", "Goal_2", "Koef", "Flag")
# test_1X_2.5_Less$Exodus<-"Koef_1X_2.5_Less"
# profit(test_1X_2.5_Less)

############################################################################### 2X 2.5 Less ###################################################################

# test_2X_2.5_Less<-subset(result, result$Flag_2X_2.5_More_row>=2  & 
#                            result$Koef_2X_2.5_More <=2.3 & result$League %in% c("SP1", "I1"))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef_2X_2.5_Less", "Flag_2X_2.5_Less")]
# names(test_2X_2.5_Less)<-c("League", "Date", "Team_1", "Team_2",  "Goal_1", "Goal_2", "Koef", "Flag")
# test_2X_2.5_Less$Exodus<-"Koef_2X_2.5_Less"

############################################################################### 1X  ###################################################################

# test_1X<-subset(result, result$Flag_1X_row>=9  & 
#                            result$Koef_1X <=2.0 & result$League %in% c("E0", "I1"))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef_1X", "Flag_1X")]
# names(test_1X)<-c("League", "Date", "Team_1", "Team_2",  "Goal_1", "Goal_2", "Koef", "Flag")
# test_1X$Exodus<-"Koef_1X"

############################################################################### 2X  ###################################################################

# test_2X<-subset(result, result$Flag_1X_row>=5  & 
#                   result$Koef_2X <=2.0 & result$League %in% c("F1", "I1"))[,c("League", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Koef_2X", "Flag_2X")]
# names(test_2X)<-c("League", "Date", "Team_1", "Team_2",  "Goal_1", "Goal_2", "Koef", "Flag")
# test_2X$Exodus<-"Koef_2X"

test<-rbind(rbind(test_3_Less, test_3_More), rbind(test_Win, test_Defeat))
test<-test[with(test,order(test$Date)),]
#plot(profit(test))

exit<-rbind(exit_my_model[,c(colnames(test))], test)
#exit<-subset(exit,exit$Koef<=1.83)
exit<-exit[with(exit,order(exit$Date)),]
exit<-exit[!duplicated(exit), ]
  
##########################################################################################################################################

write.csv(merge(x = subset(exit, is.na(exit$Flag)==TRUE & as.Date(exit$Date)==Sys.Date())[,c("Date", "Team_1", "Team_2", "Exodus", "Koef")], 
                y = time, by = c("Team_1", "Team_2"), all = FALSE),
          paste(directory, "Football_Forecasts.csv", sep=""), row.names = FALSE)

write.csv(exit[,c("Date", "Team_1", "Team_2", "Exodus", "Koef")], paste(directory, "For_Me.csv", sep=""), row.names = FALSE)

# sample_Win_F_overall<-data.frame()
# 
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_1X_2.5_Less",    "Koef_1X_2.5_Less",    0.84, 0.00, 1.0))
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_2X_2.5_Less",    "Koef_2X_2.5_Less",    0.84, 0.00, 1.0))
# 
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_1X_2.5_More",    "Koef_1X_2.5_More",    0.84, 0.00, 1.0))
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_2X_2.5_More",    "Koef_2X_2.5_More",    0.84, 0.00, 1.0))
# 
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_W1_2.5_More",    "Koef_W1_2.5_More",    0.84, 0.00, 1.0))
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_W2_2.5_More",    "Koef_W2_2.5_More",    0.84, 0.00, 1.0))
# 
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_Win",            "Win",                 0.84, 0.00, 1.0))
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1",  "F1", "D1", "E0"), NA, "Flag_Defeat",         "Defeat",              0.84, 0.00, 1.0))
# 
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("F1", "E0"),                           NA, "Total_more_2",        "BbMx_More_2",         0.84, 0.00, 1.0))
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("E1", "F1"),                           NA, "Total_less_2",        "BbMx_Less_2",         0.84, 0.00, 1.0))
# 
# sample_Win_F_overall<-sample_Win_F_overall[with(sample_Win_F_overall,order(sample_Win_F_overall$Date)),]
# 
# write.csv(sample_Win_F_overall, 
#           paste(directory, "For_me.csv", sep=""), row.names = FALSE)

# profit(sample_Win_F_overall)
# 
# exit<-data.frame()
# 
# for (i in 1:length(unique(sample_Win_F_overall$Date)))
#   
# {
#   list<-c("")
#   
#   
#     for (j in 1:length(unique(sample_Win_F_overall$Exodus)))
#          
#          {
#       
#            test<-subset(sample_Win_F_overall, sample_Win_F_overall$Date<unique(sample_Win_F_overall$Date)[i] & sample_Win_F_overall$Exodus==unique(sample_Win_F_overall$Exodus)[j])
#            
#            if (nrow(test)>0)
#            {
#              if (profit(test)[length(profit(test))]>0.7)
#                
#              {
#                
#                list<-c(list, as.character(unique(sample_Win_F_overall$Exodus)[j]))
#              }
#            }
#     }
#   
#     exit<-rbind(exit, subset(sample_Win_F_overall, sample_Win_F_overall$Date==unique(sample_Win_F_overall$Date)[i] & sample_Win_F_overall$Exodus %in% list))
# }
# 
# profit(exit)

# sample_Win_F_overall<-data.frame()
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1", "F1", "D1", "E0"), 13, "Flag_Win",        "Win",    0.83))
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1", "F1", "D1", "E0"), 13, "Flag_Defeat",     "Defeat", 0.7))
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check(c("SP1", "I1", "E1", "F1", "D1", "E0")                              , 13, "Total_more_2",    "BbMx_More_2",    0.65))
# sample_Win_F_overall<-sample_Win_F_overall[with(sample_Win_F_overall,order(sample_Win_F_overall$Date)),]
# profit(sample_Win_F_overall)
# 
# profit(subset(sample_Win_F_overall, sample_Win_F_overall$League %in% c("E0") & sample_Win_F_overall$Pred>=0.83))
# profit(sample_Win_F_overall)
# 
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check("E0",  12))
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check("E1",  12))
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check("D1",  12))
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check("SC0", 12))
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check("I1",  12))
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check("SP1", 12))
# sample_Win_F_overall<-rbind(sample_Win_F_overall, check("F1",  12))
# 
# sample_Win_F_overall<-subset(sample_Win_F_overall, sample_Win_F_overall$League=="E0")
# sample_Win_F_overall<-sample_Win_F_overall[with(sample_Win_F_overall,order(sample_Win_F_overall$Date)),]
# 
# profit(subset(sample_Win_F_overall, sample_Win_F_overall$League%in% c("E1")))
# 
# sample_SP1<-sample
# 
# data_12<-find_best_strategy(sample,30)
# data_win_SP1<-data
# data<-
# sample_Win_F<-rbind(strategy(trend(subset(data_win_E0, data_win_E0$Count>=10),5), subset(data_win_E0, data_win_E0$Count>=10), sample_E0),
#                     strategy(trend(subset(data_win_SP1, data_win_SP1$Count>=10),5), subset(data_win_SP1, data_win_SP1$Count>=10), sample_SP1))
# 
# sample_Win_F<-rbind(sample_Win_F,
#                     strategy(trend(subset(data_win_SP1, data_win_SP1$Count>=10),5), subset(data_win_SP1, data_win_SP1$Count>=10), sample_SP1))
# 
# 
# sample_Win_F<-strategy(trend(subset(data, data$Count>=10),5), subset(data, data$Count>=10), sample)
# sample_Win_F<-strategy(data, sample)
# sample_Win_F<-sample_Win_F[with(sample_Win_F,order(sample_Win_F$Date)),]
# profit(sample_Win_F)
# 
# #sample_Win_F<-sample_Win_F %>% distinct(Team_1, Team_2, .keep_all = TRUE)
# sample_Win_F<-rbind(strategy(data_win_I1, sample_I1),
#                     strategy(data_win_SP1, sample_SP1))
# 
# sample_Win_F<-sample_Win_F[with(sample_Win_F,order(sample_Win_F$Date)),]
# profit(sample_Win_F)
# 
# sample_Win<-rbind(sample_Win_F, sample_Win_D)
# sample_Win<-sample_Win[with(sample_Win,order(sample_Win$Date)),]
# profit(sample_Win)
# 
# data_13<-data

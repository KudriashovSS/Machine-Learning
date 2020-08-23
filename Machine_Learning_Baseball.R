

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
  
  xgb <- xgboost(data = data.matrix(train_sample[,!names(train_sample) %in% "Flag"]), label = train_sample$Flag, eta = 0.01,
                 max_depth =20, gamma = 10, subsample = 0.6, colsample_bytree =0.5, min_child_weight = 5,
                 nthread = 2,  nrounds = 1000, eval_metric = "auc",objective='binary:logistic') 
  
  
  #xgb.save(bst, paste(directory, 'xgb.model_', string, sep=""))
  
  return (predict(xgb, data.matrix(control_sample)))
  #return(xgb)
  
  #return ()
}


#directory<-"C:/Users/mb29945/Desktop/SK/Machine_Learning/Machine_Learning/SPORTS/Baseball/"
directory<-"C:/Users/Сергей/Desktop/НЕ ЗАХЛАМЛЯЙ РАБОЧИЙ СТОЛ, СКЛАДЫВАТЬ ВСЕ СЮДА/Сергей/Machine_Learning/Machine_Learning/Machine_Learning/SPORTS/Baseball/"
data_teams<-read_excel(paste(directory, "Baseball_history_data_team_stats.xlsx", sep=""), 
                       col_names = c("Season", "Dataset", "Game-id", "Date", "Team", "Venue", "I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", 
                                     "I11", "I12", "I13", "I14", "I15", "I16", "I17", "I18", "I19", "I20", "I21", "I22", "I23", "I24", "I25", "R", "H", "E",
                                     "Umpires_H_1_B", "Umpires_2_3_B", "Temperature", "Wind", "Duration", "AB", "R_B", "H_B", "RBI", "BB_B", "1B", "2B", "3B", "HR",
                                     "TB", "SB", "HBP", "SO", "PA", "P_B", "S_B", "PO", "A_B", "AVG_B", "Starting_pitcher", "W_L_pitcher", "W", "L", "IP", "H_P",
                                     "R_P", "ER_P", "ERA_P", "P_P", "S_P", "BB_P", "SO_P", "SB_P", "HR_P", "QS_P", "HB_P", "CG_P", "CGSHO_P", "NH_P", "BF_P", "GB_P",
                                     "FB_P", "LD_P", "Opening Odds", "Live_movements_opened", "Live_movements_1", "Live_movements_2", "Live_movements_3", 
                                     "Live_movements_closed", "Closing_odds", "Runline", "1H_Total_1H_ML", "1H_runline"),
                       skip=1)

  
data_teams$I1<-ifelse(is.na(data_teams$I1)==TRUE,0,data_teams$I1)
data_teams$I2<-ifelse(is.na(data_teams$I2)==TRUE,0,data_teams$I2)
data_teams$I3<-ifelse(is.na(data_teams$I3)==TRUE,0,data_teams$I3)
data_teams$I4<-ifelse(is.na(data_teams$I4)==TRUE,0,data_teams$I4)
data_teams$I5<-ifelse(is.na(data_teams$I5)==TRUE,0,data_teams$I5)
data_teams$I6<-ifelse(is.na(data_teams$I6)==TRUE,0,data_teams$I6)
data_teams$I7<-ifelse(is.na(data_teams$I7)==TRUE,0,data_teams$I7)
data_teams$I8<-ifelse(is.na(data_teams$I8)==TRUE,0,data_teams$I8)
data_teams$I9<-ifelse(is.na(data_teams$I9)==TRUE,0,data_teams$I9)
data_teams$I10<-ifelse(is.na(data_teams$I10)==TRUE,0,data_teams$I10)
data_teams$I11<-ifelse(is.na(data_teams$I11)==TRUE,0,data_teams$I11)
data_teams$I12<-ifelse(is.na(data_teams$I12)==TRUE,0,data_teams$I12)
data_teams$I13<-ifelse(is.na(data_teams$I13)==TRUE,0,data_teams$I13)
data_teams$I14<-ifelse(is.na(data_teams$I14)==TRUE,0,data_teams$I14)
data_teams$I15<-ifelse(is.na(data_teams$I15)==TRUE,0,data_teams$I15)
data_teams$I16<-ifelse(is.na(data_teams$I16)==TRUE,0,data_teams$I16)
data_teams$I17<-ifelse(is.na(data_teams$I17)==TRUE,0,data_teams$I17)
data_teams$I18<-ifelse(is.na(data_teams$I18)==TRUE,0,data_teams$I18)
data_teams$I19<-ifelse(is.na(data_teams$I19)==TRUE,0,data_teams$I19)
data_teams$I20<-ifelse(is.na(data_teams$I20)==TRUE,0,data_teams$I20)
data_teams$I21<-ifelse(is.na(data_teams$I21)==TRUE,0,data_teams$I21)
data_teams$I22<-ifelse(is.na(data_teams$I22)==TRUE,0,data_teams$I22)
data_teams$I23<-ifelse(is.na(data_teams$I23)==TRUE,0,data_teams$I23)
data_teams$I24<-ifelse(is.na(data_teams$I24)==TRUE,0,data_teams$I24)
data_teams$I25<-ifelse(is.na(data_teams$I25)==TRUE,0,data_teams$I25)

data<-data.frame(matrix(data=NA, nrow=nrow(data_teams)/2, ncol=94))

colnames(data)<-c("Season", "REG_PO", "Date", "Game", "Team_1", "Team_2", "Goal_1", "Goal_2", "Win_NBA", "Defeat_NBA", "Wind", "Duration", "R_1", "R_2",
                  "H_1", "H_2", "E_1", "E_2", "AB_1", "AB_2", "R_B_1", "R_B_2", "H_B_1", "H_B_2", "RBI_1", "RBI_2", "BB_B_1", "BB_B_2", "B_1_1", "B_1_2",
                  "B_2_1", "B_2_2", "B_3_1", "B_3_2", "HR_1", "HR_2", "TB_1", "TB_2", "SB_1", "SB_2", "HBP_1", "HBP_2", "SO_1", "SO_2", 
                  "PA_1", "PA_2", "PB_1", "PB_2","PO_1", "PO_2", "A_B_1", "A_B_2", "Flag_Win", "Flag_Defeat", "IP_1", "IP_2", "H_P_1", "H_P_2", "R_P_1",
                  "R_P_1", "ER_P_1", "ER_P_2", "ERA_P_1", "ERA_P_2", "P_P_1", "P_P_2", "S_P_1", "S_P_2", "BB_P_1", "BB_P_2", "SO_P_1",
                  "SO_P_2", "SB_P_1", "SB_P_2", "HR_P_1", "HR_P_2", "QS_P_1", "QS_P_2", "HB_P_1", "HB_P_2", "CG_P_1", "CG_P_2", "CGSHO_P_1", 
                  "CGSHO_P_2", "NH_P_1", "NH_P_2", "BF_P_1", "BF_P_1", "GB_P_1", "GB_P_2", "FB_P_1", "FB_P_2","LD_P_1", "LD_P_2")

data$Date<-as.Date("2019-01-01")

for (i in 1:nrow(data))
  
{
  data$Season[i]    <-              data_teams$Season[2*i]
  data$REG_PO[i]    <-              ifelse(grepl("Regular", data_teams$Dataset[2*i])==TRUE, "Regular", "Playoff")
  
  data$Date[i]      <-              as.Date(data_teams$Date)[2*i]
  data$Game[i]      <-              data_teams$`Game-id`[2*i]
  
  data$Team_1[i]    <-              data_teams$Team[2*i]
  data$Team_2[i]    <-              data_teams$Team[2*i-1]
  
  data$Goal_1[i]    <-              data_teams$I1[2*i]+data_teams$I2[2*i]+data_teams$I3[2*i]+data_teams$I4[2*i]+data_teams$I5[2*i]+data_teams$I6[2*i]+
                                    data_teams$I7[2*i]+data_teams$I8[2*i]+data_teams$I9[2*i]+data_teams$I10[2*i]+data_teams$I11[2*i]+data_teams$I12[2*i]+
                                    data_teams$I13[2*i]+data_teams$I14[2*i]+data_teams$I15[2*i]+data_teams$I16[2*i]+data_teams$I17[2*i]+data_teams$I18[2*i]+
                                    data_teams$I19[2*i]+data_teams$I20[2*i]+data_teams$I21[2*i]+data_teams$I22[2*i]+data_teams$I23[2*i]+data_teams$I24[2*i]+
                                    data_teams$I24[2*i]+data_teams$I25[2*i]
  
  data$Goal_2[i]    <-              data_teams$I1[2*i-1]+data_teams$I2[2*i-1]+data_teams$I3[2*i-1]+data_teams$I4[2*i-1]+data_teams$I5[2*i-1]+data_teams$I6[2*i-1]+
                                    data_teams$I7[2*i-1]+data_teams$I8[2*i-1]+data_teams$I9[2*i-1]+data_teams$I10[2*i-1]+data_teams$I11[2*i-1]+data_teams$I12[2*i-1]+
                                    data_teams$I13[2*i-1]+data_teams$I14[2*i-1]+data_teams$I15[2*i-1]+data_teams$I16[2*i-1]+data_teams$I17[2*i-1]+data_teams$I18[2*i-1]+
                                    data_teams$I19[2*i-1]+data_teams$I20[2*i-1]+data_teams$I21[2*i-1]+data_teams$I22[2*i-1]+data_teams$I23[2*i-1]+data_teams$I24[2*i-1]+
                                    data_teams$I24[2*i-1]+data_teams$I25[2*i-1]
  
  data$Win_NBA[i]   <-              ifelse(abs(data_teams$`Opening Odds`[2*i])>=100,
                                    ifelse(as.numeric(gsub("+", "", data_teams$`Opening Odds`[2*i]))<0,100/abs(as.numeric(gsub("+", "", data_teams$`Opening Odds`[2*i])))+1,
                                    ifelse(as.numeric(gsub("+", "", data_teams$`Opening Odds`[2*i]))==0,1.95,as.numeric(gsub("+", "",   data_teams$`Opening Odds`[2*i]))/100+1)),1)
                                    
                                    
  
  data$Defeat_NBA[i]<-              ifelse(abs(data_teams$`Opening Odds`[2*i-1])>=100,
                                    ifelse(as.numeric(gsub("+", "", data_teams$`Opening Odds`[2*i-1]))<0,100/abs(as.numeric(gsub("+", "", data_teams$`Opening Odds`[2*i-1])))+1,
                                    ifelse(as.numeric(gsub("+", "", data_teams$`Opening Odds`[2*i-1]))==0,1.95,as.numeric(gsub("+", "",   data_teams$`Opening Odds`[2*i-1]))/100+1)),1)
  
  data$Win_NBA[i]<-                 ifelse(data$Win_NBA[i]>1,data$Win_NBA[i], 1/(1.00-1/data$Defeat_NBA[i]))
  
  data$Defeat_NBA[i]<-              ifelse(data$Defeat_NBA[i]>1,data$Defeat_NBA[i], 1/(1.00-1/data$Win_NBA[i]))
  
  
  data$Wind[i]<-                    as.numeric(data_teams$Wind[2*i-1])
  data$Temperature[i]<-             as.numeric(data_teams$Temperature[2*i-1])
  
  data$Duration[i]<-                as.numeric(data_teams$Duration[2*i-1]) 
  
  data$R_1[i]<-                     data_teams$R[2*i]
  data$R_2[i]<-                     data_teams$R[2*i-1]
  data$H_1[i]<-                     data_teams$H[2*i]
  data$H_2[i]<-                     data_teams$H[2*i-1]
  data$E_1[i]<-                     data_teams$E[2*i]
  data$E_2[i]<-                     data_teams$E[2*i-1]
  
  data$AB_1[i]<-                    data_teams$AB[2*i]
  data$AB_2[i]<-                    data_teams$AB[2*i-1]
  data$R_B_1[i]<-                   data_teams$R_B[2*i]
  data$R_B_2[i]<-                   data_teams$R_B[2*i-1]
  
  data$H_B_1[i]<-                   data_teams$H_B[2*i]
  data$H_B_2[i]<-                   data_teams$H_B[2*i-1]
  
  data$RBI_1[i]<-                   data_teams$RBI[2*i]
  data$RBI_2[i]<-                   data_teams$RBI[2*i-1]
  
  data$BB_B_1[i]<-                  data_teams$BB_B[2*i]
  data$BB_B_2[i]<-                  data_teams$BB_B[2*i-1]
  
  data$B_1_1[i]<-                   data_teams$`1B`[2*i]
  data$B_1_2[i]<-                   data_teams$`1B`[2*i-1]
  
  data$B_2_1[i]<-                   data_teams$`2B`[2*i]
  data$B_2_2[i]<-                   data_teams$`2B`[2*i-1]
  
  data$B_3_1[i]<-                   data_teams$`3B`[2*i]
  data$B_3_2[i]<-                   data_teams$`3B`[2*i-1]
  
  data$HR_1[i]<-                    data_teams$HR[2*i]
  data$HR_2[i]<-                    data_teams$HR[2*i-1]
  
  data$TB_1[i]<-                    data_teams$TB[2*i]
  data$TB_2[i]<-                    data_teams$TB[2*i-1]
  
  data$SB_1[i]<-                    data_teams$SB[2*i]
  data$SB_2[i]<-                    data_teams$SB[2*i-1]
  
  data$HBP_1[i]<-                   data_teams$HBP[2*i]
  data$HBP_2[i]<-                   data_teams$HBP[2*i-1]
  
  data$SO_1[i]<-                    data_teams$SO[2*i]
  data$SO_2[i]<-                    data_teams$SO[2*i-1]
  
  data$PA_1[i]<-                    data_teams$PA[2*i]
  data$PA_2[i]<-                    data_teams$PA[2*i-1]
  
  data$PB_1[i]<-                    data_teams$P_B[2*i]
  data$PB_2[i]<-                    data_teams$P_B[2*i-1]
  
  data$PO_1[i]<-                    data_teams$PO[2*i]
  data$PO_2[i]<-                    data_teams$PO[2*i-1]
  
  data$A_B_1[i]<-                   data_teams$A_B[2*i]
  data$A_B_2[i]<-                   data_teams$A_B[2*i-1]
  
  data$Flag_Win[i]<-                ifelse(is.na(data_teams$W[2*i])==TRUE,0,data_teams$W[2*i])
  data$Flag_Defeat[i]<-             1-data$Flag_Win[i]
  
  data$IP_1[i]<-                    data_teams$IP[2*i]
  data$IP_2[i]<-                    data_teams$IP[2*i-1]
  
  data$H_P_1[i]<-                   data_teams$H_P[2*i]
  data$H_P_2[i]<-                   data_teams$H_P[2*i-1]
  
  data$R_P_1[i]<-                   data_teams$R_P[2*i]
  data$R_P_2[i]<-                   data_teams$R_P[2*i-1]
  
  data$ER_P_1[i]<-                  data_teams$ER_P[2*i]
  data$ER_P_2[i]<-                  data_teams$ER_P[2*i-1]
  
  data$ER_P_1[i]<-                  data_teams$ER_P[2*i]
  data$ER_P_2[i]<-                  data_teams$ER_P[2*i-1]
  
  data$ERA_P_1[i]<-                 data_teams$ERA_P[2*i]
  data$ERA_P_2[i]<-                 data_teams$ERA_P[2*i-1]
  
  data$P_P_1[i]<-                   data_teams$P_P[2*i]
  data$P_P_2[i]<-                   data_teams$P_P[2*i-1]
  
  data$S_P_1[i]<-                   data_teams$S_P[2*i]
  data$S_P_2[i]<-                   data_teams$S_P[2*i-1]
  
  data$BB_P_1[i]<-                  data_teams$BB_P[2*i]
  data$BB_P_2[i]<-                  data_teams$BB_P[2*i-1]
  
  data$SO_P_1[i]<-                  data_teams$SO_P[2*i]
  data$SO_P_2[i]<-                  data_teams$SO_P[2*i-1]
  
  data$SB_P_1[i]<-                  data_teams$SB_P[2*i]
  data$SB_P_2[i]<-                  data_teams$SB_P[2*i-1]
  
  data$HR_P_1[i]<-                  data_teams$HR_P[2*i]
  data$HR_P_2[i]<-                  data_teams$HR_P[2*i-1]
  
  data$QS_P_1[i]<-                  data_teams$QS_P[2*i]
  data$QS_P_2[i]<-                  data_teams$QS_P[2*i-1]
  
  data$HB_P_1[i]<-                  data_teams$HB_P[2*i]
  data$HB_P_2[i]<-                  data_teams$HB_P[2*i-1]
  
  data$CG_P_1[i]<-                  data_teams$CG_P[2*i]
  data$CG_P_2[i]<-                  data_teams$CG_P[2*i-1]
  
  data$CGSHO_P_1[i]<-               data_teams$CGSHO_P[2*i]
  data$CGSHO_P_2[i]<-               data_teams$CGSHO_P[2*i-1]
  
  data$NH_P_1[i]<-                  data_teams$NH_P[2*i]
  data$NH_P_2[i]<-                  data_teams$NH_P[2*i-1]
  
  data$BF_P_1[i]<-                  data_teams$BF_P[2*i]
  data$BF_P_2[i]<-                  data_teams$BF_P[2*i-1]
  
  data$GB_P_1[i]<-                  data_teams$GB_P[2*i]
  data$GB_P_2[i]<-                  data_teams$GB_P[2*i-1]
  
  data$FB_P_1[i]<-                  data_teams$FB_P[2*i]
  data$FB_P_2[i]<-                  data_teams$FB_P[2*i-1]
  
  data$LD_P_1[i]<-                  data_teams$LD_P[2*i]
  data$LD_P_2[i]<-                  data_teams$LD_P[2*i-1]
  
}

data$Win_NBA<-ifelse(is.na(data$Win_NBA)==TRUE,1,data$Win_NBA)
data$Defeat_NBA<-ifelse(is.na(data$Defeat_NBA)==TRUE,1,data$Defeat_NBA)


for (i in 1:nrow(data))
  
{
  if (data$Win_NBA[i]<2.0)
    
  {
    data$Win_NBA[i]<-data$Win_NBA[i]*1.00
    data$Defeat_NBA[i]<-1/(1.035-1/data$Win_NBA[i])
  }
  
  if (data$Defeat_NBA[i]<2.0)
    
  {
    data$Defeat_NBA[i]<-data$Defeat_NBA[i]*1.00
    data$Win_NBA[i]<-1/(1.035-1/data$Defeat_NBA[i])
  }
  
}

n<-5
cot<-data
result<-data.frame()

for (i in 1:length(unique(cot$Season)))
  
{
  
  cot_2<-subset(cot, cot$Season==unique(cot$Season)[i])
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$Match_T1_All[j]<-nrow(rbind(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j]))))),
                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))))
    
    
    
    cot_2$Match_T2_All[j]<-nrow(rbind(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j]))))),
                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))))
    
    
    
    
    cot_2$Match_T1_Home[j]<-nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & cot_2$Team_1==cot_2$Team_1[j]))
    
    
    
    
    cot_2$Match_T2_Guest[j]<-nrow(subset(cot_2, cot_2$Date<cot_2$Date[j] & cot_2$Team_2==cot_2$Team_2[j]))
    
  }
  
  
  
  ########################################################################## ???????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  
  {
    
    
    cot_2$W_Total_Win_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_Win,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_Defeat))
    
    
    cot_2$W_Total_Win_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Defeat,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_Win))
    
    
    
  }
  
  
  ##################################################################### ?????????????????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Total_Defeat_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_Defeat,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_Win))
    
    
    cot_2$W_Total_Defeat_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Win,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_Defeat))
    
    
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
  
  

  ##################################################################### Ind Total More 110 ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_R_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$R_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$R_2))
    
    cot_2$W_Plus_R_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$R_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$R_1))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_R_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$R_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$R_1))
    
    
    cot_2$W_Minus_R_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$R_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$R_2))
    
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_H_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$H_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$H_2))
    
    cot_2$W_Plus_H_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$H_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$H_1))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_H_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$H_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$H_1))
    
    
    cot_2$W_Minus_H_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$H_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$H_2))
    
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_E_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$E_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$E_2))
    
    cot_2$W_Plus_E_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$E_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$E_1))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_E_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$E_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$E_1))
    
    
    cot_2$W_Minus_E_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$E_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$E_2))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_AB_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$AB_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$AB_2))
    
    cot_2$W_Plus_AB_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$AB_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$AB_1))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_AB_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$AB_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$AB_1))
    
    
    cot_2$W_Minus_AB_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$AB_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$AB_2))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_R_B_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$R_B_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$R_B_2))
    
    cot_2$W_Plus_R_B_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$R_B_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$R_B_1))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_R_B_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$R_B_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$R_B_1))
    
    
    cot_2$W_Minus_R_B_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$R_B_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$R_B_2))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_H_B_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$H_B_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$H_B_2))
    
    cot_2$W_Plus_H_B_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$H_B_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$H_B_1))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_H_B_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$H_B_2,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$H_B_1))
    
    
    cot_2$W_Minus_H_B_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$H_B_1,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$H_B_2))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_RBI_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$RBI_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$RBI_2))
    
    cot_2$W_Plus_RBI_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$RBI_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$RBI_1))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_RBI_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$RBI_2,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$RBI_1))
    
    
    cot_2$W_Minus_RBI_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$RBI_1,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$RBI_2))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_BB_B_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$BB_B_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$BB_B_2))
    
    cot_2$W_Plus_BB_B_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$BB_B_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$BB_B_1))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_BB_B_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$BB_B_2,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$BB_B_1))
    
    
    cot_2$W_Minus_BB_B_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$BB_B_1,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$BB_B_2))
    
    
    
  }
  

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_B_1_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$B_1_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$B_1_2))
  
  cot_2$W_Plus_B_1_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$B_1_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$B_1_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_B_1_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$B_1_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$B_1_1))
  
  
  cot_2$W_Minus_B_1_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$B_1_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$B_1_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_B_2_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$B_2_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$B_2_2))
  
  cot_2$W_Plus_B_2_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$B_2_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$B_2_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_B_2_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$B_2_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$B_2_1))
  
  
  cot_2$W_Minus_B_2_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$B_2_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$B_2_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_B_3_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$B_3_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$B_3_2))
  
  cot_2$W_Plus_B_3_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$B_3_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$B_3_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_B_3_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$B_3_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$B_3_1))
  
  
  cot_2$W_Minus_B_3_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$B_3_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$B_3_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_HR_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HR_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HR_2))
  
  cot_2$W_Plus_HR_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HR_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HR_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_HR_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HR_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HR_1))
  
  
  cot_2$W_Minus_HR_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HR_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HR_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_TB_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$TB_1,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$TB_2))
  
  cot_2$W_Plus_TB_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$TB_2,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$TB_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_TB_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$TB_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$TB_1))
  
  
  cot_2$W_Minus_TB_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$TB_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$TB_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_SB_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$SB_1,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$SB_2))
  
  cot_2$W_Plus_SB_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$SB_2,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$SB_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_SB_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$SB_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$SB_1))
  
  
  cot_2$W_Minus_SB_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$SB_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$SB_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_HBP_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HBP_1,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HBP_2))
  
  cot_2$W_Plus_HBP_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HBP_2,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HBP_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_HBP_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HBP_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HBP_1))
  
  
  cot_2$W_Minus_HBP_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HBP_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HBP_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_SO_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$SO_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$SO_2))
  
  cot_2$W_Plus_SO_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$SO_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$SO_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_SO_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$SO_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$SO_1))
  
  
  cot_2$W_Minus_SO_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$SO_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$SO_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_PA_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$PA_1,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$PA_2))
  
  cot_2$W_Plus_PA_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$PA_2,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$PA_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_PA_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$PA_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$PA_1))
  
  
  cot_2$W_Minus_PA_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$PA_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$PA_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_PB_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$PB_1,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$PB_2))
  
  cot_2$W_Plus_PB_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$PB_2,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$PB_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_PB_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$PB_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$PB_1))
  
  
  cot_2$W_Minus_PB_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$PB_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$PB_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_A_B_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$A_B_1,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$A_B_2))
  
  cot_2$W_Plus_A_B_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$A_B_2,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$A_B_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_A_B_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$A_B_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$A_B_1))
  
  
  cot_2$W_Minus_A_B_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$A_B_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$A_B_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_IP_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$IP_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$IP_2))
  
  cot_2$W_Plus_IP_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$IP_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$IP_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_IP_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$IP_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$IP_1))
  
  
  cot_2$W_Minus_IP_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$IP_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$IP_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_H_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$H_P_1,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$H_P_2))
  
  cot_2$W_Plus_H_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$H_P_2,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$H_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_H_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$H_P_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$H_P_1))
  
  
  cot_2$W_Minus_H_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$H_P_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$H_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_R_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$R_P_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$R_P_2))
  
  cot_2$W_Plus_R_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$R_P_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$R_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_R_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$R_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$R_P_1))
  
  
  cot_2$W_Minus_R_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$R_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$R_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_ER_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$ER_P_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$ER_P_2))
  
  cot_2$W_Plus_ER_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$ER_P_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$ER_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_ER_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$ER_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$ER_P_1))
  
  
  cot_2$W_Minus_ER_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$ER_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$ER_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_ERA_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$ERA_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$ERA_P_2))
  
  cot_2$W_Plus_ERA_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$ERA_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$ERA_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_ERA_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$ERA_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$ERA_P_1))
  
  
  cot_2$W_Minus_ERA_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$ERA_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$ERA_P_2))
  
  
  
}


for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_P_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$P_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$P_P_2))
  
  cot_2$W_Plus_P_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$P_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$P_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_P_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$P_P_2,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$P_P_1))
  
  
  cot_2$W_Minus_P_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$P_P_1,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$P_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_S_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$S_P_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$S_P_2))
  
  cot_2$W_Plus_S_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$S_P_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$S_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_S_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$S_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$S_P_1))
  
  
  cot_2$W_Minus_S_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$S_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$S_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_BB_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$BB_P_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$BB_P_2))
  
  cot_2$W_Plus_BB_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$BB_P_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$BB_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_BB_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$BB_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$BB_P_1))
  
  
  cot_2$W_Minus_BB_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$BB_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$BB_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_SO_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$SO_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$SO_P_2))
  
  cot_2$W_Plus_SO_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$SO_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$SO_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_SO_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$SO_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$SO_P_1))
  
  
  cot_2$W_Minus_SO_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$SO_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$SO_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_SB_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$SB_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$SB_P_2))
  
  cot_2$W_Plus_SB_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$SB_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$SB_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_SB_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$SB_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$SB_P_1))
  
  
  cot_2$W_Minus_SB_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$SB_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$SB_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_HR_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HR_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HR_P_2))
  
  cot_2$W_Plus_HR_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HR_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HR_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_HR_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HR_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HR_P_1))
  
  
  cot_2$W_Minus_HR_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HR_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HR_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_QS_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$QS_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$QS_P_2))
  
  cot_2$W_Plus_QS_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$QS_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$QS_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_QS_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$QS_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$QS_P_1))
  
  
  cot_2$W_Minus_QS_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$QS_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$QS_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_HB_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HB_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HB_P_2))
  
  cot_2$W_Plus_HB_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HB_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HB_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_HB_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$HB_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$HB_P_1))
  
  
  cot_2$W_Minus_HB_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$HB_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$HB_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_CG_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$CG_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$CG_P_2))
  
  cot_2$W_Plus_CG_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$CG_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$CG_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_CG_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$CG_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$CG_P_1))
  
  
  cot_2$W_Minus_CG_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$CG_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$CG_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_CGSHO_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$CGSHO_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$CGSHO_P_2))
  
  cot_2$W_Plus_CGSHO_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$CGSHO_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$CGSHO_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_CGSHO_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$CGSHO_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$CGSHO_P_1))
  
  
  cot_2$W_Minus_CGSHO_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$CGSHO_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$CGSHO_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_NH_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$NH_P_1,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$NH_P_2))
  
  cot_2$W_Plus_NH_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$NH_P_2,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$NH_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_NH_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$NH_P_2,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$NH_P_1))
  
  
  cot_2$W_Minus_NH_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$NH_P_1,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$NH_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_BF_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$BF_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$BF_P_2))
  
  cot_2$W_Plus_BF_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$BF_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$BF_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_BF_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$BF_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$BF_P_1))
  
  
  cot_2$W_Minus_BF_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$BF_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$BF_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_BF_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$BF_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$BF_P_2))
  
  cot_2$W_Plus_BF_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$BF_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$BF_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_BF_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$BF_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$BF_P_1))
  
  
  cot_2$W_Minus_BF_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$BF_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$BF_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_GB_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$GB_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$GB_P_2))
  
  cot_2$W_Plus_GB_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$GB_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$GB_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_GB_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$GB_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$GB_P_1))
  
  
  cot_2$W_Minus_GB_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$GB_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$GB_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_FB_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FB_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FB_P_2))
  
  cot_2$W_Plus_FB_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FB_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FB_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_FB_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FB_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FB_P_1))
  
  
  cot_2$W_Minus_FB_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FB_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FB_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_FB_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FB_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FB_P_2))
  
  cot_2$W_Plus_FB_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FB_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FB_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_FB_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FB_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FB_P_1))
  
  
  cot_2$W_Minus_FB_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FB_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FB_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_LD_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$LD_P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$LD_P_2))
  
  cot_2$W_Plus_LD_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$LD_P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$LD_P_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_LD_P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$LD_P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$LD_P_1))
  
  
  cot_2$W_Minus_LD_P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$LD_P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$LD_P_2))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Plus_PO_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$PO_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$PO_2))
  
  cot_2$W_Plus_PO_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$PO_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$PO_1))
  
  
  
}

for (j in 1:nrow(cot_2))
{
  
  
  cot_2$W_Minus_PO_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$PO_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$PO_1))
  
  
  cot_2$W_Minus_PO_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$PO_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$PO_2))
  
  
  
}

result<-rbind(result,cot_2)

}   

write.csv(result, paste(directory, "Factors_Calculation_Baseball_SK.csv", sep=""), row.names = FALSE)

# result$Rand<-runif(1, 0.0, 1.0)
# 
# result<-merge(x = result,
#               y = data[,c("Game", "Win_NBA", "Defeat_NBA")],
#               by = "Game", all.x = TRUE)
# 
# colnames(result)
# 
# drop <- c("Win_NBA.x","Defeat_NBA.x")
# result = result[,!(names(result) %in% drop)]
# 
# names(result)[names(result) == "Win_NBA.y"] <- "Win_NBA"
# names(result)[names(result) == "Defeat_NBA.y"] <- "Defeat_NBA"

check<-function (Season_list, Flag_filter, Koef_filter, pred_index, koef_index_low, koef_index_high)
  
{
  
  
  # Season_list<-14
  # Flag_filter<-"Flag_Win_Fora_Minus_5"
  # Koef_filter<-"Win_NBA"
  # pred_index<-0.6
  # koef_index_low<-1.2
  # koef_index_high<-2.0
  
  history<-read.csv(paste(directory, "Factors_Calculation_Baseball_SK.csv", sep=""), sep=",",h=T)
  history$Date<-as.Date(history$Date,"%Y-%m-%d")
  
for (j in 1:25)
  
{
   
  for (i in 1:nrow(history))
    
  {
  
  history$Rand[i]<-runif(1, 0.0, 1.0)
  
  }

  
  # control<-subset(history, history$Match_T1_Home>=5 & history$Match_T2_Guest>=5 & history$Rand>=0.95)
  # validation<-subset(history, history$Match_T1_Home>=5 & history$Match_T2_Guest>=5 & history$Rand<0.95)
  # 
  # 
  # if (j==1)
  #   
  # {
  #   control_overall<-control
  #   validation_overall<-validation
  # }
  # 
  # control_overall<-rbind( control_overall,  control)
  # validation_overall<-rbind( validation_overall,  validation)
  

  

  colnames(history)[match("Flag_Defeat",colnames(history))]<-"Flag"
  
  validation<-subset(history, history$Match_T1_Home>=5 & history$Match_T2_Guest>=5 & history$Rand<0.95)
  
  # validation<-validation[,c("Flag",
  #                           "Win_NBA",
  #                           "Defeat_NBA",
  #                           colnames(history)[103:(ncol(history)-1)])]
                          
  
  validation<-validation[,c("Flag",
                            "Defeat_NBA",
                            "Win_NBA",
                            "W_Plus_SB_P_T1_All",
                            "W_Plus_B_3_T1_All",
                            "W_Plus_FB_P_T2_All",
                            "W_Plus_FB_P_T1_All",
                            "W_Minus_SO_T2_All", 
                            "W_Minus_BB_B_T2_All",
                            "W_Plus_SB_T2_All",
                            "W_Minus_E_T1_All",
                            "W_Plus_QS_P_T1_All",
                            "W_Plus_SB_T2_All",
                            "W_Minus_B_3_T1_All")]
                            
  control<-subset(history, history$Match_T1_Home>=5 & history$Match_T2_Guest>=5 & history$Rand>=0.95)
  colnames(control)[match("Flag_Defeat",colnames(control))]<-"Flag"
  
  control_Date<-control$Date
  control_Team_1<-control$Team_1
  control_Team_2<-control$Team_2
  control_Goal_1<-control$Goal_1
  control_Goal_2<-control$Goal_2
  control_Flag<-control$Flag
  control_Win<-control$Win_NBA
  control_Defeat<-control$Defeat_NBA
  control_Koef<-control$Defeat_NBA
  control_Exodus<-"2st Team Win"
  
  # control<-control[,c("Win_NBA",
  #                           "Defeat_NBA",
  #                           colnames(history)[103:(ncol(history)-1)])]
  
  control<-control[,c( "Defeat_NBA",
                       "Win_NBA",
                       "W_Plus_SB_P_T1_All",
                       "W_Plus_B_3_T1_All",
                       "W_Plus_FB_P_T2_All",
                       "W_Plus_FB_P_T1_All",
                       "W_Minus_SO_T2_All",
                       "W_Minus_BB_B_T2_All",
                       "W_Plus_SB_T2_All",
                       "W_Minus_E_T1_All",
                       "W_Plus_QS_P_T1_All",
                       "W_Plus_SB_T2_All",
                       "W_Minus_B_3_T1_All")]
  
  
   #bst <- xgb.load(paste(directory, 'xgb.model_', Koef_filter, sep=""))
  
  
  # library(CreditRisk)
  # library(creditR)
  # library(caTools)
  # library(caret)
  # library(woeBinning)
  # 
  # woerules <- woe.binning(df = train_sample,target.var = "Flag",pred.var = train_sample,event.class = 1)
  # train_woe <- woe.binning.deploy(train_sample, woerules, add.woe.or.dum.var='woe')
  # 
  # #Creating a dataset with the transformed variables and default flag
  # 
  # train_woe <- train_woe[ , grepl( "woe" , names( train_woe ) ) ]
  # train_woe$Flag<-train_sample$Flag
  # 
  # #Applying the WOE rules used on the train data to the test data
  # 
  # test_woe <- woe.binning.deploy(control_sample, woerules, add.woe.or.dum.var='woe')
  # test_woe <- test_woe[ , grepl( "woe" , names( test_woe ) ) ]
  # test_woe$Flag<-control_sample$Flag
  # 
  # # Information value and univariate gini can be used as variable selection methods. Generally, a threshold value of 0.30 is used for IV and 0.10 is used for univariate Gini
  # 
  # IV.calc.data(train_woe,"Flag")
  # 
  # Gini.univariate.data(train_woe,"Flag")
  # 
  # # Creating a new dataset by Gini elimination. IV elimination is also possible
  # 
  # eliminated_data <- IV_elimination(train_woe,"Flag",0.03)
  # 
  # # Correlation matrix constructure
  # 
  # corr_matrix = cor(eliminated_data[,-ncol(eliminated_data)])
  # hc = findCorrelation(corr_matrix, cutoff=0.7) 
  # hc = sort(hc)
  # selected_data = eliminated_data[,-c(hc)]
  # 
  # # Creating a logistic (target: 0 or 1) regression model of the data
  # 
  # model= glm(formula = Flag ~ ., family = binomial(link = "logit"),  data = selected_data)
  # summary(model)
  # woe.glm.feature.importance(selected_data, model,"Flag")
  # 
  # # Exclude variables with low feauture importance: 
  # # woe.X30_60DelinquencyLast3Mon.binned, woe.X60_90DelinquencyLast12Mon.binned, woe.X60_90DelinquencyLast3Mon.binned 
  # # and recontstruct our model 
  # 
  # # selected_data<-selected_data[,!colnames(selected_data) %in% c("woe.X30_60DelinquencyLast3Mon.binned", 
  # #                                                               "woe.X60_90DelinquencyLast12Mon.binned", 
  # #                                                               "woe.X60_90DelinquencyLast3Mon.binned")]
  # 
  # model= glm(formula = Flag ~ ., family = binomial(link = "logit"),  data = selected_data)
  # summary(model)
  # woe.glm.feature.importance(selected_data,model,"Flag")
  # 
  # ms_train_data <- cbind(selected_data,model$fitted.values)
  # ms_test_data <- cbind(test_woe[,colnames(selected_data)], predict(model,type = "response", newdata = test_woe))
  # 
  # plot.roc(ms_train_data$Flag, ms_train_data[,ncol(ms_train_data)],print.auc=T) 
  # plot.roc(ms_test_data$Flag, ms_test_data[,ncol(ms_test_data)],print.auc=T) 
  
  
  train_sample<-validation
  control_sample<-control
  
  xgb <- xgboost(data = data.matrix(train_sample[,!names(train_sample) %in% "Flag"]), label = train_sample$Flag, eta = 0.01,
                 max_depth =20, gamma = 10, subsample = 0.6, colsample_bytree =0.5, min_child_weight = 5,
                 nthread = 2,  nrounds = 1000, eval_metric = "auc",objective='binary:logistic') 
  
  #xgb.save(bst, paste(directory, 'xgb.model_', string, sep=""))
  
  #a<-xgb.importance(model=xgb)
  
  sample=data.frame(control_Date,  
                    control_Team_1, 
                    control_Team_2, 
                    control_Goal_1, 
                    control_Goal_2, 
                    control_Flag,
                    # boost(validation,
                    #       control,
                    #       Koef_filter),
                    predict(xgb, data.matrix(control_sample)),
                    control_Koef,
                    control_Win,
                    control_Defeat,
                    c(rep(control_Exodus, length(control_Koef))))
  
  names(sample)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef", "Koef_Win", "Koef_Defeat", "Exodus")
  
  plot.roc(sample$Flag, sample$Koef,print.auc=T)
  plot.roc(sample$Flag, sample$Pred,print.auc=T)
  
  sample$Date<-as.Date(sample$Date,"%Y/%m/%d")
  sample<-sample[with(sample,order(sample$Date)),]
  plot.roc(sample$Flag, sample$Pred,print.auc=T) 
  
  sample_Win<-subset(sample, sample$Pred>=0.0)
  profit(sample_Win)

  if (j==1)
    
  {
    sample_overall<-sample_Win
  }
  
  else
  
    {
    
    sample_overall<-rbind(sample_overall, sample_Win)
  
    }
  
  
}
  sample_overall<-sample_overall[with(sample_overall,order(sample_overall$Date)),]
  
  profit(subset(sample_overall, sample_overall$Pred>=0.70))
  
  nrow(subset(sample_overall, sample_overall$Pred>=0.70))
  
  sample_overall_Win <- subset(sample_overall, sample_overall$Pred>=0.73)
  
  sample_overall_Defeat <- subset(sample_overall, sample_overall$Pred>=0.73)
  
  sample_overall_overall<-rbind(sample_overall_Win, sample_overall_Defeat)
  sample_overall_overall<-sample_overall_overall[with(sample_overall_overall,order(sample_overall_overall$Date)),]
  profit(sample_overall_overall)
  
  #write.table(subset(sample, is.na(sample$Flag)==TRUE), paste(directory, "For_Me_Basketball.csv", sep=""), sep=",", col.names = !file.exists(paste(directory, "For_Me_Basketball.csv", sep="")), append = T, row.names = FALSE)
  
  # sample_Win<-subset(sample, sample$Pred>=pred_index & sample$Koef>=koef_index_low & sample$Koef<=koef_index_high)
  # #sample_Win<-subset(sample, sample$Pred>=0.65 & sample$Koef>=1.2 & sample$Koef<=2.0)
  # sample_Win$Koef<-sample_Win$Koef*1.2
  
  # ifelse(Flag_filter=="Flag_Win_Fora_Minus_5",
  #        sample$Koef<-ifelse(sample$Goal_1-sample$Goal_2==5,1,sample$Koef),
  #        sample$Koef<-ifelse(sample$Goal_2-sample$Goal_2==1,1,sample$Koef))
  #sample_Win$Flag<-ifelse(sample_Win$Goal_1-sample_Win$Goal_2>=5,1,0)
  #sample_Win<-sample_Win[with(sample_Win, order(sample_Win$Date)),]
  
  # profit(sample_Win)
  # 
  sample_test=data.frame(sample$Date,  sample$Team_1, sample$Team_2, sample$Goal_1, sample$Goal_2, sample$Flag,
                         sample$Pred, sample$Koef, sample$Exodus)
  
  names(sample_test)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef", "Exodus")
  #sample_test$Flag<-ifelse(sample_test$Goal_1-sample_test$Goal_2>=5,1,0)
  sample_test$Koef<-sample_test$Koef*1.2
  
  ifelse(Flag_filter=="Flag_Win_Fora_Minus_5",
         sample_test$Koef<-ifelse(is.na(sample_test$Flag)==FALSE,
                                  ifelse(sample_test$Goal_1-sample_test$Goal_2==5,1,sample_test$Koef),
                                  sample_test$Koef),
         sample_test$Koef<-ifelse(is.na(sample_test$Flag)==FALSE,
                                  ifelse(sample_test$Goal_2-sample_test$Goal_1==5,1,sample_test$Koef),
                                  sample_test$Koef))
  
  sample_test<-sample_test[with(sample_test, order(sample_test$Date)),]
  sample_Win<-                     find_best_strategy(sample_test, 1.2, pred_index, koef_index_low, koef_index_high)
  #profit(sample_Win)
  
  return (sample_Win)
  
}

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
#library(RCurl)
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

find_best_strategy<-function(sample, multiplier, ts)
  
{  
  
  
  for (i in 1:length(unique(sample$Date)))
    
  {
    
    
    max<-0
    kmaxup<-0
    
    for (j in 1:25)
      
    {
      
      
      if (nrow(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Koef>=1.2*multiplier & sample$Koef<=2.0*multiplier & sample$Pred>=ts+j/100))>0)
      {
        
        if(profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Koef>=1.2*multiplier & sample$Koef<=2.0*multiplier & sample$Pred>=ts+j/100))[length(profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Koef>=1.2*multiplier & sample$Koef<=2.0*multiplier & sample$Pred>=ts+j/100)))]>max)    
          
        {
          
          max<-profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Koef>=1.2*multiplier & sample$Koef<=2.0*multiplier & sample$Pred>=ts+j/100))[length(profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Koef>=1.2*multiplier & sample$Koef<=2.0*multiplier & sample$Pred>=ts+j/100)))]
          kmaxup<-j
        }
        
        
      } 
      
      
    }
    
    if (exists("sample_overall")==TRUE)
      
    {
      
      sample_overall<-rbind(sample_overall, subset(sample, sample$Date==unique(sample$Date)[i] & sample$Koef>=1.2*multiplier & sample$Koef<=2.0*multiplier & sample$Pred>=ts+kmaxup/100))
      
      
    }
    
    
    if (exists("sample_overall")==FALSE)
    {
      
      sample_overall<-subset(sample, sample$Date==unique(sample$Date)[i] & sample$Date==unique(sample$Date)[i] & sample$Koef>=1.2*multiplier & sample$Koef<=2.0*multiplier & sample$Pred>=ts+kmaxup/100)
      
    }
    
  }
  
  return(sample_overall)
  
}

directory<-"/root/"

directory<-"C:/Users/Сергей/Desktop/НЕ ЗАХЛАМЛЯЙ РАБОЧИЙ СТОЛ, СКЛАДЫВАТЬ ВСЕ СЮДА/Сергей/Machine_Learning/Machine_Learning/Machine_Learning/SPORTS/Basketball/"

# download.file("https://www.dropbox.com/sh/tnptj57ppsrh9jg/AAB1aLCt9MYv52OBQ6YWoSe9a?dl=0",
#               paste(directory, "past.xlsx", sep=""), mode = "wb", extra='curl')

data<-read_excel(paste(directory,  
                       paste(substr(as.character(Sys.Date()-1), 6,7),"-",  substr(as.character(Sys.Date()-1), 9,10), "-",substr(as.character(Sys.Date()-1), 1,4), sep=""),
                       "-nba-season-team-feed.xlsx", sep=""), sheet="NBA-TEAM-FEED")

file.remove(dir(  
  directory, 
  pattern = "feed", 
  full.names = TRUE
))

colnames(data)[30]<-"TO..TO"

data$Win<-1.2
data$Defeat<-6.0
data$REG_PO<-ifelse(grepl("Regular", data$DATASET)==TRUE, "Regular", "Playoff")

past<-data.frame(matrix(data=NA, nrow=nrow(data)/2, ncol=57))

names(past)         <-              c("Season","REG_PO", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Win_NBA", "Defeat_NBA", 
                                    "X1Q_1", "X2Q_1", "X3Q_1", "X4Q_1", "OT_1","X1Q_2", "X2Q_2", "X3Q_2", "X4Q_2", "OT_2", 
                                   "FG_1",	"FGA_1",	"X3P_1",	"X3PA_1",	"FT_1",	"FTA_1",	"OR_1",	"DR_1",	"TOT_1",	
                                   "A_1",	"PF_1",	"ST_1",	"TO_1",	"TO.TO_1",	"BL_1", "POSS_1",	"PACE_1",	"OEFF_1",	"DEFF_1",
                                   "FG_2",	"FGA_2",	"X3P_2",	"X3PA_2",	"FT_2",	"FTA_2",	"OR_2",	"DR_2",	"TOT_2",	
                                   "A_2",	"PF_2",	"ST_2",	"TO_2",	"TO.TO_2",	"BL_2", "POSS_2",	"PACE_2",	"OEFF_2",	"DEFF_2")

for (i in 1:nrow(past))
  
{
  past$Season[i]    <-              14
  past$REG_PO[i]    <-              ifelse(grepl("Regular", data$DATASET[2*i])==TRUE, "Regular", "Playoff")
  
  past$Date[i]      <-              as.Date(data$DATE,"%m/%d/%Y")[2*i]
  
  past$Team_1[i]    <-              data$TEAM[2*i]
  past$Team_2[i]    <-              data$TEAM[2*i-1]
  
  past$Goal_1[i]    <-              data$PTS[2*i]
  past$Goal_2[i]    <-              data$PTS[2*i-1]
  
  past$Win_NBA[i]   <-              ifelse(as.numeric(gsub("+", "", data$MONEYLINE[2*i]))<0,100/abs(as.numeric(gsub("+", "", data$MONEYLINE[2*i])))+1,
                                   ifelse(as.numeric(gsub("+", "", data$MONEYLINE[2*i]))==0,1.95,as.numeric(gsub("+", "", data$MONEYLINE[2*i]))/100+1))
  
  past$Defeat_NBA[i]<-              ifelse(as.numeric(gsub("+", "", data$MONEYLINE[2*i-1]))<0,100/abs(as.numeric(gsub("+", "", data$MONEYLINE[2*i-1])))+1,
                                   ifelse(as.numeric(gsub("+", "", data$MONEYLINE[2*i-1]))==0,1.95,as.numeric(gsub("+", "", data$MONEYLINE[2*i-1]))/100+1))

  past$X1Q_1[i]     <-              data$`1Q`[2*i]
  past$X2Q_1[i]     <-              data$`1Q`[2*i]
  past$X3Q_1[i]     <-              data$`1Q`[2*i]
  past$X4Q_1[i]     <-              data$`1Q`[2*i]
  
  past$OT_1[i]      <-              ifelse(is.na(data$OT1[2*i])==TRUE,0,data$OT1[2*i])+
                                   ifelse(is.na(data$OT2[2*i])==TRUE,0,data$OT2[2*i])+
                                   ifelse(is.na(data$OT3[2*i])==TRUE,0,data$OT3[2*i])+
                                   ifelse(is.na(data$OT4[2*i])==TRUE,0,data$OT4[2*i])+
                                   ifelse(is.na(data$OT5[2*i])==TRUE,0,data$OT5[2*i])
  
  past$X1Q_2[i]     <-              data$`1Q`[2*i-1]
  past$X2Q_2[i]     <-              data$`1Q`[2*i-1]
  past$X3Q_2[i]     <-              data$`1Q`[2*i-1]
  past$X4Q_2[i]     <-              data$`1Q`[2*i-1]
  
  past$OT_2[i]      <-              ifelse(is.na(data$OT1[2*i-1])==TRUE,0,data$OT1[2*i-1])+
                                   ifelse(is.na(data$OT2[2*i-1])==TRUE,0,data$OT2[2*i-1])+
                                   ifelse(is.na(data$OT3[2*i-1])==TRUE,0,data$OT3[2*i-1])+
                                   ifelse(is.na(data$OT4[2*i-1])==TRUE,0,data$OT4[2*i-1])+
                                   ifelse(is.na(data$OT5[2*i-1])==TRUE,0,data$OT5[2*i-1])
  
  past$FG_1[i]      <-              data$FG[2*i] 
  past$FGA_1[i]     <-              data$FGA[2*i]
  past$X3P_1[i]     <-              data$`3P`[2*i]
  past$X3PA_1[i]    <-              data$`3PA`[2*i]
  past$FT_1[i]      <-              data$FT[2*i]
  past$FTA_1[i]     <-              data$FTA[2*i]
  past$OR_1[i]      <-              data$OR[2*i]
  past$DR_1[i]      <-              data$DR[2*i]
  past$TOT_1[i]     <-              data$TOT[2*i]
  past$A_1[i]       <-              data$A[2*i]
  past$PF_1[i]      <-              data$PF[2*i]
  past$ST_1[i]      <-              data$ST[2*i]
  past$TO_1[i]      <-              data$TO[2*i]
  past$TO.TO_1[i]   <-              data$TO..TO[2*i]
  past$BL_1[i]      <-              data$BL[2*i]
  past$POSS_1[i]    <-              data$POSS[2*i]
  past$PACE_1[i]    <-              data$PACE[2*i]
  past$OEFF_1[i]    <-              data$OEFF[2*i]
  past$DEFF_1[i]    <-              data$DEFF[2*i]
  
  past$FG_2[i]      <-              data$FG[2*i-1]
  past$FGA_2[i]     <-              data$FGA[2*i-1]
  past$X3P_2[i]     <-              data$`3P`[2*i-1]
  past$X3PA_2[i]    <-              data$`3PA`[2*i-1]
  past$FT_2[i]      <-              data$FT[2*i-1]
  past$FTA_2[i]     <-              data$FTA[2*i-1]
  past$OR_2[i]      <-              data$OR[2*i-1]
  past$DR_2[i]      <-              data$DR[2*i-1]
  past$TOT_2[i]     <-              data$TOT[2*i-1]
  past$A_2[i]       <-              data$A[2*i-1]
  past$PF_2[i]      <-              data$PF[2*i-1]
  past$ST_2[i]      <-              data$ST[2*i-1]
  past$TO_2[i]      <-              data$TO[2*i-1]
  past$TO.TO_2[i]   <-              data$TO..TO[2*i-1]
  past$BL_2[i]      <-              data$BL[2*i-1]
  past$POSS_2[i]    <-              data$POSS[2*i-1]
  past$PACE_2[i]    <-              data$PACE[2*i-1]
  past$OEFF_2[i]    <-              data$OEFF[2*i-1]
  past$DEFF_2[i]    <-              data$DEFF[2*i-1]
  
  }

past$Date<-as.Date(past$Date, origin = "1970-01-01")


future<-read_excel(paste(directory, "basketball_future.xlsx", sep=""))
future$Date<-as.Date(future$Date,"%y-%m-%d")
future$Win<-as.numeric(future$Win)
future$Defeat<-as.numeric(future$Defeat)

time<-data.frame(future[,c("Team_1", "Team_2", "Time")])
time$Time<-substr(as.character(time$Time), 12, 20)
colnames(time)<-c("Team_1", "Team_2", "Time")

future$REG_PO    <- "Regular"
future$Season<-14

future<-data.frame(future$Season,
                   future$REG_PO,
                   future$Date,
                   future$Team_1,
                   future$Team_2,
                   c(rep(NA,nrow(future))),
                   c(rep(NA,nrow(future))),
                   future$Win,
                   future$Defeat,
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
                   c(rep(NA,nrow(future))))  

names(future)         <-              c("Season","REG_PO", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Win_NBA", "Defeat_NBA", 
                                      "X1Q_1", "X2Q_1", "X3Q_1", "X4Q_1", "OT_1","X1Q_2", "X2Q_2", "X3Q_2", "X4Q_2", "OT_2", 
                                      "FG_1",	"FGA_1",	"X3P_1",	"X3PA_1",	"FT_1",	"FTA_1",	"OR_1",	"DR_1",	"TOT_1",	
                                      "A_1",	"PF_1",	"ST_1",	"TO_1",	"TO.TO_1",	"BL_1", "POSS_1",	"PACE_1",	"OEFF_1",	"DEFF_1",
                                      "FG_2",	"FGA_2",	"X3P_2",	"X3PA_2",	"FT_2",	"FTA_2",	"OR_2",	"DR_2",	"TOT_2",	
                                      "A_2",	"PF_2",	"ST_2",	"TO_2",	"TO.TO_2",	"BL_2", "POSS_2",	"PACE_2",	"OEFF_2",	"DEFF_2")

cot<-rbind(past, future)

####################################################################################################################################################################

cot$Date<-as.Date(cot$Date, origin = "1970-01-01")
cot$Flag_Win<-ifelse(cot$Goal_1>cot$Goal_2,1,0)
cot$Flag_Defeat<-ifelse(cot$Goal_1<cot$Goal_2,1,0)

cot$Flag_Win_Fora_Minus_5<-ifelse(cot$Goal_1-cot$Goal_2>=5,1,0)
cot$Flag_Defeat_Fora_Minus_5<-ifelse(cot$Goal_2-cot$Goal_1>=5,1,0)

cot$Total_more_170<-ifelse(cot$Goal_1+cot$Goal_2>170,1,0)
cot$Total_more_180<-ifelse(cot$Goal_1+cot$Goal_2>180,1,0)
cot$Total_more_190<-ifelse(cot$Goal_1+cot$Goal_2>190,1,0)
cot$Total_more_200<-ifelse(cot$Goal_1+cot$Goal_2>200,1,0)
cot$Total_more_210<-ifelse(cot$Goal_1+cot$Goal_2>210,1,0)
cot$Total_more_220<-ifelse(cot$Goal_1+cot$Goal_2>220,1,0)

cot$Flag_Ind_total_Team1_more_80<-ifelse(cot$Goal_1>80,1,0)
cot$Flag_Ind_total_Team1_more_90<-ifelse(cot$Goal_1>90,1,0)
cot$Flag_Ind_total_Team1_more_100<-ifelse(cot$Goal_1>100,1,0)
cot$Flag_Ind_total_Team1_more_110<-ifelse(cot$Goal_1>110,1,0)
cot$Flag_Ind_total_Team1_more_120<-ifelse(cot$Goal_1>120,1,0)

cot$Flag_Ind_total_Team2_more_80<-ifelse(cot$Goal_2>80,1,0)
cot$Flag_Ind_total_Team2_more_90<-ifelse(cot$Goal_2>90,1,0)
cot$Flag_Ind_total_Team2_more_100<-ifelse(cot$Goal_2>100,1,0)
cot$Flag_Ind_total_Team2_more_110<-ifelse(cot$Goal_2>110,1,0)
cot$Flag_Ind_total_Team2_more_120<-ifelse(cot$Goal_2>120,1,0)

n<-5

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
  
  
  ########################################################################## ???????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Total_Win_Fora_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_Win_Fora_Minus_5,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_Defeat_Fora_Minus_5))
    
    
    cot_2$W_Total_Win_Fora_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Defeat_Fora_Minus_5,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_Win_Fora_Minus_5))
    
    
    
  }
  
  
  ##################################################################### ?????????????????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Total_Defeat_Fora_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_Defeat_Fora_Minus_5,
                                                         subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_Win_Fora_Minus_5))
    
    
    cot_2$W_Total_Defeat_Fora_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Win_Fora_Minus_5,
                                                         subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_Defeat_Fora_Minus_5))
    
    
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
  
  
  ##################################################################### ?????????????? ???????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_Goals_1Q_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$X1Q_1,
                                                     subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$X1Q_2))
    
    cot_2$W_Plus_Goals_1Q_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$X1Q_2,
                                                     subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$X1Q_1))
    
    
  }
  
  ##################################################################### ?????????????????????? ???????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_Goals_1Q_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$X1Q_2,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$X1Q_1))
    
    
    cot_2$W_Minus_Goals_1Q_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$X1Q_1,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$X1Q_2))
    
    
    
  }
  
  
  ##################################################################### ?????????????? ???????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_Goals_2Q_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$X2Q_1,
                                                     subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$X2Q_2))
    
    cot_2$W_Plus_Goals_2Q_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$X2Q_2,
                                                     subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$X2Q_1))
    
    
  }
  ##################################################################### ?????????????????????? ???????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_Goals_2Q_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$X2Q_2,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$X2Q_1))
    
    
    cot_2$W_Minus_Goals_2Q_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$X2Q_1,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$X2Q_2))
    
    
    
  }
  
  ##################################################################### ?????????????? ???????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_Goals_3Q_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$X3Q_1,
                                                     subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$X3Q_2))
    
    cot_2$W_Plus_Goals_3Q_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$X3Q_2,
                                                     subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$X3Q_1))
    
    
  }
  
  ##################################################################### ?????????????????????? ???????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_Goals_3Q_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$X3Q_2,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$X3Q_1))
    
    
    cot_2$W_Minus_Goals_3Q_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$X3Q_1,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$X3Q_2))
    
    
    
  }
  
  ##################################################################### ?????????????? ???????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_Goals_4Q_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$X4Q_1,
                                                     subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$X4Q_2))
    
    cot_2$W_Plus_Goals_4Q_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$X4Q_2,
                                                     subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$X4Q_1))
    
    
  }
  ##################################################################### ?????????????????????? ???????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_Goals_4Q_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$X4Q_2,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$X4Q_1))
    
    
    cot_2$W_Minus_Goals_4Q_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$X4Q_1,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$X4Q_2))
    
    
    
  }
  
  
  ##################################################################### ?????????????? ???????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_Goals_OT_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$OT_1,
                                                     subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$OT_2))
    
    cot_2$W_Plus_Goals_OT_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$OT_2,
                                                     subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$OT_1))
    
    
  }
  
  ##################################################################### ?????????????????????? ???????? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_Goals_OT_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$OT_2,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$OT_1))
    
    
    cot_2$W_Minus_Goals_OT_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$OT_1,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$OT_2))
    
    
    
  }
  
  
  ##################################################################### ???????????? ####################################################################
  
  ##################################################################### Total More 170 ?? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$W_Total_More_170_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_170,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_170))
    
    
    cot_2$W_Total_More_170_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_170,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_170))
    
    
  }
  
  
  ##################################################################### Total More 180 ?? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$W_Total_More_180_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_180,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_180))
    
    
    cot_2$W_Total_More_180_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_180,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_180))
    
    
  }
  
  ##################################################################### Total More 190 ?? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$W_Total_More_190_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_190,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_190))
    
    
    cot_2$W_Total_More_190_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_190,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_190))
    
    
  }
  
  ##################################################################### Total More 200 ?? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$W_Total_More_200_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_200,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_200))
    
    
    cot_2$W_Total_More_200_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_200,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_200))
    
    
  }
  
  ##################################################################### Total More 210 ?? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$W_Total_More_210_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_210,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_210))
    
    
    cot_2$W_Total_More_210_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_210,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_210))
    
    
  }
  
  ##################################################################### Total More 220 ?? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$W_Total_More_220_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_220,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_220))
    
    
    cot_2$W_Total_More_220_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_220,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_220))
    
    
  }
  
  
  
  ##################################################################### Total More 190 ?? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$W_Total_More_190_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_190,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_190))
    
    
    cot_2$W_Total_More_190_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_190,
                                                      subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_190))
    
    
  }
  
  
  ##################################################################### Ind Total More 80 ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Ind_Total_more_80_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_Ind_total_Team1_more_80,
                                                         subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_Ind_total_Team2_more_80))
    
    
    cot_2$W_Ind_Total_more_80_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Ind_total_Team2_more_80,
                                                         subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_Ind_total_Team1_more_80))
    
    
  }
  
  
  ##################################################################### Ind Total More 90 ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Ind_Total_more_90_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_Ind_total_Team1_more_90,
                                                         subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_Ind_total_Team2_more_90))
    
    
    cot_2$W_Ind_Total_more_90_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Ind_total_Team2_more_90,
                                                         subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_Ind_total_Team1_more_90))
    
    
  }
  
  ##################################################################### Ind Total More 100 ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Ind_Total_more_100_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_Ind_total_Team1_more_100,
                                                          subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_Ind_total_Team2_more_100))
    
    
    cot_2$W_Ind_Total_more_100_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Ind_total_Team2_more_100,
                                                          subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_Ind_total_Team1_more_100))
    
    
  }
  
  ##################################################################### Ind Total More 110 ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Ind_Total_more_110_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_Ind_total_Team1_more_110,
                                                          subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_Ind_total_Team2_more_110))
    
    
    cot_2$W_Ind_Total_more_110_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Ind_total_Team2_more_110,
                                                          subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_Ind_total_Team1_more_110))
    
    
  }
  
  ##################################################################### Ind Total More 110 ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_FG_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FG_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FG_2))
    
    cot_2$W_Plus_FG_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FG_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FG_1))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_FG_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FG_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FG_1))
    
    
    cot_2$W_Minus_FG_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FG_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FG_2))
    
    
    
  }
  
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_FGA_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FGA_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FGA_2))
    
    
    cot_2$W_Plus_FGA_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FGA_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FGA_1))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_FGA_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FGA_2,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FGA_1))
    
    
    cot_2$W_Minus_FGA_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FGA_1,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FGA_2))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$W_Plus_3P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$X3P_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$X3P_2))
    
    
    
    cot_2$W_Plus_3P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$X3P_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$X3P_1))
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_3P_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$X3P_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$X3P_1))
    
    
    cot_2$W_Minus_3P_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$X3P_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$X3P_2))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$W_Plus_3PA_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$X3PA_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$X3PA_2))
    
    
    
    cot_2$W_Plus_3PA_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$X3PA_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$X3PA_1))
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_3PA_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$X3PA_2,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$X3PA_1))
    
    
    cot_2$W_Minus_3PA_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$X3PA_1,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$X3PA_2))
    
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_FT_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FT_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FT_2))
    
    
    cot_2$W_Plus_FT_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FT_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FT_1))
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_FT_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FT_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FT_1))
    
    
    cot_2$W_Minus_FT_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FT_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FT_2))
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_FTA_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FTA_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FTA_2))
    
    
    cot_2$W_Plus_FTA_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FTA_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FTA_1))
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_FTA_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FTA_2,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FTA_1))
    
    
    cot_2$W_Minus_FTA_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FTA_1,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FTA_2))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$W_Plus_OR_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$OR_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$OR_2))
    
    
    cot_2$W_Plus_OR_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$OR_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$OR_1))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_OR_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$OR_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$OR_1))
    
    cot_2$W_Minus_OR_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$OR_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$OR_2))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_DR_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$DR_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$DR_2))
    
    
    cot_2$W_Plus_DR_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$DR_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$DR_1))
    
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_DR_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$DR_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$DR_1))
    
    
    cot_2$W_Minus_DR_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$DR_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$DR_2))
    
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_A_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$A_1,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$A_2))
    
    
    cot_2$W_Plus_A_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$A_2,
                                              subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$A_1))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_A_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$A_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$A_1))
    
    
    cot_2$W_Minus_A_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$A_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$A_2))
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_PF_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$PF_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$PF_2))
    
    
    cot_2$W_Plus_PF_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$PF_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$PF_1))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_PF_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$PF_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$PF_1))
    
    
    cot_2$W_Minus_PF_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$PF_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$PF_2))
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_ST_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$ST_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$ST_2))
    
    
    cot_2$W_Plus_ST_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$ST_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$ST_1))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_ST_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$ST_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$ST_1))
    
    
    cot_2$W_Minus_ST_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$ST_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$ST_2))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_TO_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$TO_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$TO_2))
    
    
    cot_2$W_Plus_TO_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$TO_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$TO_1))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_TO_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$TO_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$TO_1))
    
    
    cot_2$W_Minus_TO_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$TO_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$TO_2))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_TO_TO_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$TO.TO_1,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$TO.TO_2))
    
    
    cot_2$W_Plus_T0_TO_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$TO.TO_2,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$TO.TO_1))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_TO_TO_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$TO.TO_2,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$TO.TO_1))
    
    
    cot_2$W_Minus_T0_TO_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$TO.TO_1,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$TO.TO_2))
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_BL_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$BL_1,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$BL_2))
    
    
    cot_2$W_Plus_BL_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$BL_2,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$BL_1))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_BL_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$BL_2,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$BL_1))
    
    
    cot_2$W_Minus_BL_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$BL_1,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$BL_2))
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_POSS_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$POSS_1,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$POSS_2))
    
    
    cot_2$W_Plus_POSS_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$POSS_2,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$POSS_1))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_POSS_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$POSS_2,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$POSS_1))
    
    
    cot_2$W_Minus_POSS_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$POSS_1,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$POSS_2))
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_PACE_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$PACE_1,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$PACE_2))
    
    
    cot_2$W_Plus_PACE_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$PACE_2,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$PACE_1))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_PACE_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$PACE_2,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$PACE_1))
    
    
    cot_2$W_Minus_PACE_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$PACE_1,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$PACE_2))
    
    
  }
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_OEFF_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$OEFF_1,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$OEFF_2))
    
    
    cot_2$W_Plus_OEFF_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$OEFF_2,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$OEFF_1))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_OEFF_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$OEFF_2,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$OEFF_1))
    
    
    cot_2$W_Minus_OEFF_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$OEFF_1,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$OEFF_2))
    
    
  }
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_DEFF_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$DEFF_1,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$DEFF_2))
    
    
    cot_2$W_Plus_DEFF_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$DEFF_2,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$DEF_1))
    
    
  }
  
  for (j in 1:nrow(cot_2))
    
  {
    
    
    cot_2$W_Minus_DEFF_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$DEFF_2,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$DEFF_1))
    
    
    cot_2$W_Minus_DEFF_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$DEFF_1,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$DEFF_2))
    
    
  }
  
  result<-rbind(result,cot_2)
  
}

#write.csv(result, "Factors_Calculation_Additional_Data_New_Info.csv", row.names=FALSE)

check<-function (Season_list, Flag_filter, Koef_filter, pred_index, koef_index_low, koef_index_high)

  {
    

    # Season_list<-14
    # Flag_filter<-"Flag_Win_Fora_Minus_5"
    # Koef_filter<-"Win_NBA"
    # pred_index<-0.6
    # koef_index_low<-1.2
    # koef_index_high<-2.0

    history<-read.csv(paste(directory, "Factors_Calculation_Additional_Data_New_Info.csv", sep=""), sep=",",h=T)
    history$Date<-as.Date(history$Date,"%Y-%m-%d")
    history<-subset(history, history$Season>=1)

    
    colnames(history)[match(Flag_filter,colnames(history))]<-"Flag"

    validation<-subset(history, history$Match_T1_Home>=5 & history$Match_T2_Guest>=5 & history$Season!=Season_list)
    
    validation<-validation[,c("Flag",
                              "Win_NBA",
                              "Defeat_NBA",
                              "W_Total_Win_T1_All",
                              "W_Total_Win_Fora_T2_All",
                              "W_Total_Defeat_Fora_T1_All",
                              "W_Total_Win_Fora_T1_All",
                              "W_Total_Defeat_T1_All",
                              "W_Total_Win_T2_All",
                              "W_Total_Defeat_T2_All",
                              "W_Total_Defeat_Fora_T2_All",
                              "W_Minus_BL_T2_All",
                              "W_Plus_Goals_1Q_T2_All",
                              "W_Minus_DR_T1_All",
                              "W_Minus_A_T1_All",
                              "W_Plus_Goals_1Q_T1_All",
                              "W_Minus_A_T2_All",
                              "W_Plus_DEFF_T2_All",
                              "W_Minus_Goals_1Q_T1_All",
                              "W_Plus_OR_T2_All",
                              "W_Minus_Goals_1Q_T2_All",
                              "W_Minus_OEFF_T1_All",
                              "W_Plus_BL_T1_All",
                              "W_Minus_Goals_2Q_T2_All",
                              "W_Plus_OEFF_T1_All",
                              "W_Minus_OR_T2_All",
                              "W_Plus_ST_T1_All",
                              "W_Plus_A_T1_All",
                              "W_Plus_Goals_2Q_T1_All",
                              "W_Minus_BL_T1_All",
                              "W_Plus_PF_T2_All",
                              "W_Ind_Total_more_110_T1_All",
                              "W_Plus_A_T2_All",
                              "W_Plus_3P_T2_All",
                              "W_Plus_3P_T1_All",
                              "W_Minus_Goals_OT_T1_All",
                              "W_Plus_FT_T1_All",
                              "W_Plus_3PA_T2_All",
                              "W_Minus_Goals_3Q_T1_All",
                              "W_Plus_Goals_OT_T1_All",
                              "W_Plus_OR_T1_All",
                              "W_Plus_DR_T2_All",
                              "W_Plus_PACE_T1_All",
                              "W_Ind_Total_more_80_T1_All",
                              "W_Minus_OR_T1_All",
                              "W_Minus_FT_T1_All",
                              "W_Minus_Goals_4Q_T2_All"
                              
    )]
    
    control<-subset(result, result$Match_T1_Home>=5 & result$Match_T2_Guest>=5 & result$Season==Season_list)
    colnames(control)[match(Flag_filter,colnames(control))]<-"Flag"
    
    control_Date<-control$Date
    control_Team_1<-control$Team_1
    control_Team_2<-control$Team_2
    control_Goal_1<-control$Goal_1
    control_Goal_2<-control$Goal_2
    control_Flag<-control$Flag
    control_Win<-control$Win_NBA
    control_Defeat<-control$Defeat_NBA
    control_Koef<-control[,Koef_filter]
    control_Exodus<-ifelse(Flag_filter=="Flag_Win_Fora_Minus_5",
                    c(rep("Fora -5 Team_1", length(control_Date))),
                    c(rep("Fora -5 Team_2", length(control_Date))))

    control<-control[,c("Win_NBA",
                        "Defeat_NBA",
                        "W_Total_Win_T1_All",
                        "W_Total_Win_Fora_T2_All",
                        "W_Total_Defeat_Fora_T1_All",
                        "W_Total_Win_Fora_T1_All",
                        "W_Total_Defeat_T1_All",
                        "W_Total_Win_T2_All",
                        "W_Total_Defeat_T2_All",
                        "W_Total_Defeat_Fora_T2_All",
                        "W_Minus_BL_T2_All",
                        "W_Plus_Goals_1Q_T2_All",
                        "W_Minus_DR_T1_All",
                        "W_Minus_A_T1_All",
                        "W_Plus_Goals_1Q_T1_All",
                        "W_Minus_A_T2_All",
                        "W_Plus_DEFF_T2_All",
                        "W_Minus_Goals_1Q_T1_All",
                        "W_Plus_OR_T2_All",
                        "W_Minus_Goals_1Q_T2_All",
                        "W_Minus_OEFF_T1_All",
                        "W_Plus_BL_T1_All",
                        "W_Minus_Goals_2Q_T2_All",
                        "W_Plus_OEFF_T1_All",
                        "W_Minus_OR_T2_All",
                        "W_Plus_ST_T1_All",
                        "W_Plus_A_T1_All",
                        "W_Plus_Goals_2Q_T1_All",
                        "W_Minus_BL_T1_All",
                        "W_Plus_PF_T2_All",
                        "W_Ind_Total_more_110_T1_All",
                        "W_Plus_A_T2_All",
                        "W_Plus_3P_T2_All",
                        "W_Plus_3P_T1_All",
                        "W_Minus_Goals_OT_T1_All",
                        "W_Plus_FT_T1_All",
                        "W_Plus_3PA_T2_All",
                        "W_Minus_Goals_3Q_T1_All",
                        "W_Plus_Goals_OT_T1_All",
                        "W_Plus_OR_T1_All",
                        "W_Plus_DR_T2_All",
                        "W_Plus_PACE_T1_All",
                        "W_Ind_Total_more_80_T1_All",
                        "W_Minus_OR_T1_All",
                        "W_Minus_FT_T1_All",
                        "W_Minus_Goals_4Q_T2_All"
    )]
   
    bst <- xgb.load(paste(directory, 'xgb.model_', Koef_filter, sep=""))
     
    sample=data.frame(control_Date,  
                      control_Team_1, 
                      control_Team_2, 
                      control_Goal_1, 
                      control_Goal_2, 
                      control_Flag,
                      # boost(validation,
                      #       control,
                      #       Koef_filter),
                      predict(bst, data.matrix(control)),
                      control_Koef,
                      control_Win,
                      control_Defeat,
                      c(rep(control_Exodus, length(control_Koef))))
    
    names(sample)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef", "Koef_Win", "Koef_Defeat", "Exodus")
    sample$Date<-as.Date(sample$Date,"%Y/%m/%d")
    sample<-sample[with(sample,order(sample$Date)),]
    #plot.roc(sample$Flag, sample$Koef,print.auc=T) 

    write.table(subset(sample, is.na(sample$Flag)==TRUE), paste(directory, "For_Me_Basketball.csv", sep=""), sep=",", col.names = !file.exists(paste(directory, "For_Me_Basketball.csv", sep="")), append = T, row.names = FALSE)
    
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
    sample_Win<-                     find_best_strategy(sample_test, 1.2, pred_index)
    #profit(sample_Win)
    
return (sample_Win)

}

for (season in 14:14)
  
{
  
  sample_Win_F_overall<-data.frame()
  sample_Win_F_overall<-rbind(sample_Win_F_overall, check(season , "Flag_Win_Fora_Minus_5",      "Win_NBA",     0.60, 1.2, 2.0))
  sample_Win_F_overall<-rbind(sample_Win_F_overall, check(season , "Flag_Defeat_Fora_Minus_5",   "Defeat_NBA",  0.60, 1.2, 2.0))
  sample_Win_F_overall<-sample_Win_F_overall[with(sample_Win_F_overall,order(sample_Win_F_overall$Date)),]
  
}

sample_Basketball<-sample_Win_F_overall
#profit(sample_Basketball)
#profit(subset(sample_Basketball, sample_Basketball$Exodus=="Fora -5 Team_1"))

sample_Basketball<-rbind(subset(sample_Basketball, is.na(sample_Basketball$Flag)==FALSE & as.Date(sample_Basketball$Date)<Sys.Date()),
                       subset(sample_Basketball, as.Date(sample_Basketball$Date)==Sys.Date()+1))

write.csv(merge(x = subset(sample_Basketball, is.na(sample_Basketball$Flag)==TRUE & as.Date(sample_Basketball$Date)==Sys.Date()+1)[,c("Date", "Team_1", "Team_2", "Exodus", "Koef")], 
                y = time, by = c("Team_1", "Team_2"), all = FALSE),
                paste(directory, "Basketball_Forecasts.csv", sep=""), row.names = FALSE)

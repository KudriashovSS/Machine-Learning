

library(pROC)
library(ROCR)
library(xgboost)
library(caTools)
library(magrittr)
library(dplyr)
library(Ckmeans.1d.dp)
library(readxl)
library(RCurl)
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


find_best_strategy<-function(sample)
  
{  
  
  
  for (i in 1:length(unique(sample$Date)))
    
  {
    
    
    max<-0
    kmaxup<-0
    
    for (j in 1:25)
      
    {
      
      
      if (nrow(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Pred>=0.50+j/100))>0)
      {
        
        if(profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Pred>=0.50+j/100))[length(profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Pred>=0.50+j/100)))]>max)    
          
        {
          
          max<-profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Pred>=0.50+j/100))[length(profit(subset(sample, sample$Date<unique(sample$Date)[i] & sample$Pred>=0.50+j/100)))]
          kmaxup<-j
        }
        
        
      } 
      
      
    }
    
    if (exists("sample_overall")==TRUE)
      
    {
      
      sample_overall<-rbind(sample_overall, subset(sample, sample$Date==unique(sample$Date)[i] & sample$Pred>=0.50+kmaxup/100))
      
      
    }
    
    
    if (exists("sample_overall")==FALSE)
    {
      
      sample_overall<-subset(sample, sample$Date==unique(sample$Date)[i] & sample$Date==unique(sample$Date)[i] & sample$Pred>=0.50+kmaxup/100)
      
    }
    
  }
  
  return(sample_overall)
  
}

boost<-function(train_sample, control_sample, string)
  
{
  
  set.seed(189)
  
  xgb <- xgboost(data = data.matrix(train_sample[,!names(train_sample) %in% "Flag"]), label = train_sample$Flag, eta = 0.01,
                 max_depth =20, gamma = 10, subsample = 0.6, colsample_bytree =0.5, min_child_weight = 5,
                 nthread = 2,  nrounds = 1000, eval_metric = "auc",objective='binary:logistic') 
  
  
  #xgb.save(xgb, paste(directory, 'xgb.model_hockey_', string, sep=""))
  
  return (predict(xgb, data.matrix(control_sample)))
  #return(xgb)
  
  #return ()
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

directory<-"/root/"
#directory<-"C:/Users/Сергей/Desktop/НЕ ЗАХЛАМЛЯЙ РАБОЧИЙ СТОЛ, СКЛАДЫВАТЬ ВСЕ СЮДА/Сергей/Machine_Learning/Machine_Learning/SPORTS/Hockey/"
data<-read_excel(paste(directory, "hockey_past.xlsx", sep=""))

past<-data.frame(matrix(NA,nrow(data)/24,24))
colnames(past)<-c("Row", "Team_1", "Game", "GP", "Flag_Win", "Flag_Defeat", "T", "OT", "P", "Goal_1", "Goal_2", "S.O_Win",
                 "S.O_Loss", "SF", "SA", "PPG", "Opp", "PP.",  "TS",    "PPGA",      "PK.",    "FOW",   "FOL",     "FOW.")

past$Date<-Sys.Date()

for (i in 1:nrow(past))
  
{
  past$Row[i]<-as.numeric(data$Column[24*(i-1)+1])
  past$Team_1[i]<-ifelse(length(grep("Canadiens", data$Column[24*(i-1)+2]))==0, data$Column[24*(i-1)+2], "Montreal Canadiens")
  past$Team_2[i]<-ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="NYR", "New York Rangers",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="WSH", "Washington Capitals",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="OTT", "Ottawa Senators",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="DAL", "Dallas Stars",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="CHI", "Chicago Blackhawks",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="VAN", "Vancouver Canucks",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="SJS", "San Jose Sharks",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="DET", "Detroit Red Wings",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="EDM", "Edmonton Oilers",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="TBL", "Tampa Bay Lightning",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="MIN", "Minnesota Wild",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="CGY", "Calgary Flames",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="STL", "St. Louis Blues",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="PIT", "Pittsburgh Penguins",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="BOS", "Boston Bruins",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="VGK", "Vegas Golden Knights",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="BUF", "Buffalo Sabres",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="NSH", "Nashville Predators",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="WPG", "Winnipeg Jets",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="NJD", "New Jersey Devils",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="FLA", "Florida Panthers",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="ANA", "Anaheim Ducks",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="LAK", "Los Angeles Kings",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="CBJ", "Columbus Blue Jackets",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="TOR", "Toronto Maple Leafs",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="PHI", "Philadelphia Flyers",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="ARI", "Arizona Coyotes", 
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="CAR", "Carolina Hurricanes",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="COL", "Colorado Avalanche",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="NYI", "New York Islanders",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="MTL", "Montreal Canadiens",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="PHX", "Phoenix Coyotes",
                 ifelse(substr(data$Column[24*(i-1)+3],nchar(data$Column[24*(i-1)+3])-2, nchar(data$Column[3]))=="ATL", "Atlanta Thrashers",NA)))))))))))))))))))))))))))))))))
  past$Date[i]<-  as.Date(substr(data$Column[24*(i-1)+3], 1,10), format="%Y/%m/%d")
  past$GP[i]<-1
  past$Flag_Win[i]<-as.numeric(data$Column[24*(i-1)+5])
  past$Flag_Defeat[i]<-as.numeric(data$Column[24*(i-1)+6])
  past$T[i]<-as.numeric(data$Column[24*(i-1)+7])
  past$OT[i]<-as.numeric(data$Column[24*(i-1)+8])
  past$P[i]<-as.numeric(data$Column[24*(i-1)+9])
  past$Goal_1[i]<-as.numeric(data$Column[24*(i-1)+10])
  past$Goal_2[i]<-as.numeric(data$Column[24*(i-1)+11])
  past$S.O_Win[i]<-as.numeric(data$Column[24*(i-1)+12])
  past$S.O_Loss[i]<-as.numeric(data$Column[24*(i-1)+13])
  past$SF[i]<-as.numeric(data$Column[24*(i-1)+14])
  past$SA[i]<-as.numeric(data$Column[24*(i-1)+15])
  past$PPG[i]<-as.numeric(data$Column[24*(i-1)+16])
  past$Opp[i]<-as.numeric(data$Column[24*(i-1)+17])
  past$PP.[i]<-as.numeric(data$Column[24*(i-1)+18])
  past$TS[i]<-as.numeric(data$Column[24*(i-1)+19])
  past$PPGA[i]<-as.numeric(data$Column[24*(i-1)+20])
  past$PK.[i]<-as.numeric(data$Column[24*(i-1)+21])
  past$FOW[i]<-as.numeric(data$Column[24*(i-1)+22])
  past$FOL[i]<-as.numeric(data$Column[24*(i-1)+23])
  past$FOW.[i]<-as.numeric(data$Column[24*(i-1)+24])
  
} 

past<-past[with(past,order(past$Date)),]

past$Points_1<-ifelse(past$Goal_1>past$Goal_2         & past$OT==0, 3, 
                      ifelse(past$Goal_1>past$Goal_2  & past$OT==1, 2,
                      ifelse(past$Goal_1==past$Goal_2 & past$P==2, 2, 
                      ifelse(past$Goal_1==past$Goal_2 & past$P==1, 1,
                      ifelse(past$Goal_1<past$Goal_2  & past$OT==1, 1,0)))))

past$Points_2<-ifelse(past$Goal_1<past$Goal_2         & past$OT==0, 3, 
                      ifelse(past$Goal_1<past$Goal_2  & past$OT==1, 2,
                      ifelse(past$Goal_1==past$Goal_2 & past$P==1, 2, 
                      ifelse(past$Goal_1==past$Goal_2 & past$P==2, 1,
                      ifelse(past$Goal_1>past$Goal_2  & past$OT==1, 1,0)))))

sum(past$Flag_Win)

past$Flag_Win<-ifelse(past$Points_1>past$Points_2,1,0)
past$Flag_Defeat<-ifelse(past$Points_1<past$Points_2,1,0)

past$OT<-ifelse(past$Points_1==1 | past$Points_1==2 | past$Points_2==1 | past$Points_2==2,1 ,0)

past$Goal_1<-ifelse(past$Goal_1==past$Goal_2, 
                    ifelse(past$Flag_Win==1, past$Goal_1+1, past$Goal_1),
                    past$Goal_1)

past$Goal_2<-ifelse(past$Goal_1==past$Goal_2, 
                    ifelse(past$Flag_Defeat==1, past$Goal_2+1, past$Goal_2),
                    past$Goal_2)

past$Flag_Win_without_OT<-ifelse(past$Points_1==3,1,0)
past$Flag_Defeat_without_OT<-ifelse(past$Points_2==3,1,0)

past<-past[with(past,order(past$Date)),]

past$Total_more_5<-ifelse(past$Goal_1+past$Goal_2>5,1,0)
past$Total_more_6<-ifelse(past$Goal_1+past$Goal_2>6,1,0)
past$Total_more_7<-ifelse(past$Goal_1+past$Goal_2>7,1,0)

past$Flag_Ind_total_Team1_more_2<-ifelse(past$Goal_1>2,1,0)
past$Flag_Ind_total_Team2_more_2<-ifelse(past$Goal_2>2,1,0)

past$Flag_Ind_total_Team1_more_3<-ifelse(past$Goal_1>3,1,0)
past$Flag_Ind_total_Team2_more_3<-ifelse(past$Goal_2>3,1,0)

past$Flag_Ind_total_Team1_more_4<-ifelse(past$Goal_1>4,1,0)
past$Flag_Ind_total_Team2_more_4<-ifelse(past$Goal_2>4,1,0)

past$Flag_Ind_total_Team1_more_5<-ifelse(past$Goal_1>5,1,0)
past$Flag_Ind_total_Team2_more_5<-ifelse(past$Goal_2>5,1,0)

past$Season_Flag<-19
# 
# for (i in 2:nrow(past))
# 
# {
#   if (abs(past$Date[i]-past$Date[i-1])>90)
#   
#     {
#     past$Season_Flag[i]<-past$Season_Flag[i-1]+1
#   
#     }
# 
#   else {past$Season_Flag[i]<-past$Season_Flag[i-1]}
# 
#   }

past<-past[,c("Date", "Team_1", "Team_2", "T", "OT", "P", "Goal_1", "Goal_2",
              "S.O_Win", "S.O_Loss", "SF", "SA", "PPG", "Opp", "PP.","TS", "PPGA", "PK.", "FOW", "FOL", "FOW.", "Season_Flag")]

##################################################################################################################################################################

data<-read_excel(paste(directory, "hockey_future.xlsx",sep=""))
future<-data.frame(matrix(NA,nrow(data)/3,3))
colnames(future)<-c("Date", "Team_1", "Team_2")
future$Date<-Sys.Date()

for (i in 1:nrow(future))
  
{
  future$Date[i]<-as.Date(paste(substr(data$Column[3*(i-1)+1],1,6), "2019", sep=""), "%d.%m.%y")
  
  if (as.POSIXlt(future$Date[i])$mon+1 %in% c(10,11,12))
    
  {
    future$Date[i]<-future$Date[i]-366 
  }
  
  future$Time[i]<-substr(data$Column[3*(i-1)+1],7,nchar(data$Column[3*(i-1)+1]))
  future$Team_1[i]<-data$Column[3*(i-1)+2]
  future$Team_2[i]<-data$Column[3*(i-1)+3]
}

time<-subset(future, future$Date==Sys.Date() | future$Date==Sys.Date()+1)[,c("Team_1", "Team_2", "Time")]

future<-data.frame(future$Date,
                   future$Team_1,
                   future$Team_2,
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
                   c(rep(19,nrow(future))),
                   future$Time)  

colnames(future)<-c("Date", "Team_1", "Team_2", "T", "OT", "P", "Goal_1", "Goal_2",                          
                    "S.O_Win", "S.O_Loss", "SF", "SA", "PPG", "Opp", "PP.","TS", "PPGA", "PK.", "FOW", "FOL", "FOW.", "Season_Flag", "Time")

future<-subset(future, (as.Date(future$Date)==Sys.Date()+1 & as.numeric(substr(future$Time,2,3))<=10) | (as.Date(future$Date)==Sys.Date() & as.numeric(substr(future$Time,2,3))>10))
future<-future[,c("Date", "Team_1", "Team_2", "T", "OT", "P", "Goal_1", "Goal_2",                          
                   "S.O_Win", "S.O_Loss", "SF", "SA", "PPG", "Opp", "PP.","TS", "PPGA", "PK.", "FOW", "FOL", "FOW.", "Season_Flag")]
cot<-rbind(past, future)

cot<-cot[with(cot,order(cot$Date)),]

cot$Points_1<-ifelse(cot$Goal_1>cot$Goal_2         & cot$OT==0, 3, 
                     ifelse(cot$Goal_1>cot$Goal_2  & cot$OT==1, 2,
                            ifelse(cot$Goal_1==cot$Goal_2 & cot$P==2, 2, 
                                   ifelse(cot$Goal_1==cot$Goal_2 & cot$P==1, 1,
                                          ifelse(cot$Goal_1<cot$Goal_2  & cot$OT==1, 1,0)))))

cot$Points_2<-ifelse(cot$Goal_1<cot$Goal_2         & cot$OT==0, 3, 
                     ifelse(cot$Goal_1<cot$Goal_2  & cot$OT==1, 2,
                            ifelse(cot$Goal_1==cot$Goal_2 & cot$P==1, 2, 
                                   ifelse(cot$Goal_1==cot$Goal_2 & cot$P==2, 1,
                                          ifelse(cot$Goal_1>cot$Goal_2  & cot$OT==1, 1,0)))))

cot$Flag_Win<-ifelse(cot$Points_1>cot$Points_2,1,0)
cot$Flag_Defeat<-ifelse(cot$Points_1<cot$Points_2,1,0)

cot$OT<-ifelse(cot$Points_1==1 | cot$Points_1==2 | cot$Points_2==1 | cot$Points_2==2,1 ,0)

cot$Goal_1<-ifelse(cot$Goal_1==cot$Goal_2, 
                   ifelse(cot$Flag_Win==1, cot$Goal_1+1, cot$Goal_1),
                   cot$Goal_1)

cot$Goal_2<-ifelse(cot$Goal_1==cot$Goal_2, 
                   ifelse(cot$Flag_Defeat==1, cot$Goal_2+1, cot$Goal_2),
                   cot$Goal_2)

cot$Flag_Win_without_OT<-ifelse(cot$Points_1==3,1,0)
cot$Flag_Defeat_without_OT<-ifelse(cot$Points_2==3,1,0)

cot<-cot[with(cot,order(cot$Date)),]

cot$Total_more_5<-ifelse(cot$Goal_1+cot$Goal_2>5,1,0)
cot$Total_more_6<-ifelse(cot$Goal_1+cot$Goal_2>6,1,0)
cot$Total_more_7<-ifelse(cot$Goal_1+cot$Goal_2>7,1,0)

cot$Flag_Ind_total_Team1_more_2<-ifelse(cot$Goal_1>2,1,0)
cot$Flag_Ind_total_Team2_more_2<-ifelse(cot$Goal_2>2,1,0)

cot$Flag_Ind_total_Team1_more_3<-ifelse(cot$Goal_1>3,1,0)
cot$Flag_Ind_total_Team2_more_3<-ifelse(cot$Goal_2>3,1,0)

cot$Flag_Ind_total_Team1_more_4<-ifelse(cot$Goal_1>4,1,0)
cot$Flag_Ind_total_Team2_more_4<-ifelse(cot$Goal_2>4,1,0)

cot$Flag_Ind_total_Team1_more_5<-ifelse(cot$Goal_1>5,1,0)
cot$Flag_Ind_total_Team2_more_5<-ifelse(cot$Goal_2>5,1,0)

for (i in 1:nrow(cot))
  
{
  if (is.na(cot$Flag_Defeat[i])==FALSE)
  {
  if (cot$Goal_1[i]-cot$Goal_2[i]==1 & paste(cot$Team_1[i], cot$Team_2[i], sep = "") %in% 
      c("Winnipeg JetsCalgary Flames",
        "Calgary FlamesArizona Coyotes",
        "Toronto Maple LeafsVegas Golden Knights",
        "Toronto Maple LeafsVegas Golden Knights",
        "Philadelphia FlyersMontreal Canadiens",
        "Ottawa SenatorsLos Angeles Kings",
        "Pittsburgh PenguinsChicago Blackhawks",
        "Winnipeg JetsDallas Stars",
        "Winnipeg JetsDallas Stars",
        "Montreal CanadiensColumbus Blue Jackets",
        "New York RangersPittsburgh Penguins",
        "Los Angeles KingsDetroit Red Wings",
        "Columbus Blue JacketsSt. Louis Blues",
        "San Jose SharksDetroit Red Wings"))
  {
    cot$Flag_Win_without_OT[i]<-0
    cot$Flag_Defeat_without_OT[i]<-0
  }
  }
   
}

#################################################################################################################################################################

result<-data.frame()

for (i in 1:length(unique(cot$Season_Flag)))
  
{
  
  cot_2<-subset(cot, cot$Season_Flag==unique(cot$Season_Flag)[i])
  
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
  
  ########################################################################## ???????? ####################################################################
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Total_Win_without_OT_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_Win_without_OT,
                                                            subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_Defeat_without_OT))
    
    
    cot_2$W_Total_Win_without_OT_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Defeat_without_OT,
                                                            subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_Win_without_OT))
    
    
    
  }
  
  ########################################################################## ???????? ####################################################################
  
  
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
    
    
    cot_2$W_Total_Defeat_without_OT_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Flag_Defeat_without_OT,
                                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Flag_Win_without_OT))
    
    
    cot_2$W_Total_Defeat_without_OT_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Win_without_OT,
                                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Flag_Defeat_without_OT))
    
    
    
  }
  
  ########################################################################## ???????? ####################################################################
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_OT_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$OT,
                                          subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$OT))
    
    
    cot_2$W_OT_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$OT,
                                          subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$OT))
    
    
    
  }
  
  ########################################################################## ???????? ####################################################################
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Points_T1_Al[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Points_1,
                                             subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Points_2))
    
    
    cot_2$W_Points_T2_All_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Points_2,
                                                     subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Points_1))
    
    
    
  }
  
  ########################################################################## ???????? ####################################################################
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_Goals_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Goal_1,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Goal_2))
    
    
    cot_2$W_Plus_Goals_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Goal_2,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Goal_1))
    
    
    
  }
  
  
  ########################################################################## ???????? ####################################################################
  
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_Goals_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Goal_2,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Goal_1))
    
    
    cot_2$W_Minus_Goals_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Goal_1,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Goal_2))
    
    
    
  }
  
  
  ##################################################################### Total More 5 ?? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$W_Total_More_5_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_5,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_5))
    
    
    cot_2$W_Total_More_5_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_5,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_5))
    
    
  }
  
  
  ##################################################################### Total More 6 ?? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$W_Total_More_6_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_6,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_6))
    
    
    cot_2$W_Total_More_6_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_6,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_6))
    
    
  }
  
  ##################################################################### Total More 7 ?? ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    
    cot_2$W_Total_More_7_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Total_more_7,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Total_more_7))
    
    
    cot_2$W_Total_More_7_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Total_more_7,
                                                    subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Total_more_7))
    
    
  }
  
  
  ##################################################################### Ind Total More 2 ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Ind_Total_more_2_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Ind_total_Team1_more_2,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Ind_total_Team2_more_2))
    
    
    cot_2$W_Ind_Total_more_2_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Ind_total_Team2_more_2,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Ind_total_Team1_more_2))
    
    
  }
  
  
  ##################################################################### Ind Total More 2 ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Ind_Total_more_2_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Ind_total_Team1_more_2,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Ind_total_Team2_more_2))
    
    
    cot_2$W_Ind_Total_more_2_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Ind_total_Team2_more_2,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Ind_total_Team1_more_2))
    
    
  }
  
  ##################################################################### Ind Total More 3 ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Ind_Total_more_3_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Ind_total_Team1_more_3,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Ind_total_Team2_more_3))
    
    
    cot_2$W_Ind_Total_more_3_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Ind_total_Team2_more_3,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Ind_total_Team1_more_3))
    
    
  }
  
  ##################################################################### Ind Total More 4 ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Ind_Total_more_4_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Ind_total_Team1_more_4,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Ind_total_Team2_more_4))
    
    
    cot_2$W_Ind_Total_more_4_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Ind_total_Team2_more_4,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Ind_total_Team1_more_4))
    
    
  }
  
  ##################################################################### Ind Total More 5 ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Ind_Total_more_5_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Ind_total_Team1_more_5,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Ind_total_Team2_more_5))
    
    
    cot_2$W_Ind_Total_more_5_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Ind_total_Team2_more_5,
                                                        subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Ind_total_Team1_more_5))
    
    
  }
  
  
  #####################################################################   ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_S_O_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$S.O_Win,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$S.O_Loss))
    
    
    cot_2$W_Plus_S_O_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$S.O_Loss,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$S.O_Win))
    
    
  }
  
  
  #####################################################################   ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_S_O_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$S.O_Loss,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$S.O_Win))
    
    
    cot_2$W_Minus_S_O_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$S.O_Win,
                                                 subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$S.O_Loss))
    
    
  }
  
  #####################################################################   ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_Shots_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$SF,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$SA))
    
    
    cot_2$W_Plus_Shots_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$SA,
                                                  subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$SF))
    
    
  }
  
  
  #####################################################################   ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_Shots_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$SA,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$SF))
    
    
    cot_2$W_Minus_Shots_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$SF,
                                                   subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$SA))
    
    
  }
  
  
  #####################################################################   ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_PP_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$PPG,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$PPA))
    
    
    cot_2$W_Plus_PP_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$PPA,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$PPG))
    
    
  }
  
  
  #####################################################################   ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_PP_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$PPA,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$PPG))
    
    
    cot_2$W_Minus_PP_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$PPG,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$PPA))
    
    
  }
  
  #####################################################################   ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_PPO_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$Opp,
                                           subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$Opp))
    
    
    cot_2$W_PPO_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Opp,
                                           subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$Opp))
    
    
  }
  
  
  
  #####################################################################   ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Plus_FO_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FOW,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FOL))
    
    
    cot_2$W_Plus_FO_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FOL,
                                               subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FOW))
    
    
  }
  
  
  #####################################################################   ####################################################################
  
  for (j in 1:nrow(cot_2))
  {
    
    
    cot_2$W_Minus_FO_T1_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_1[j])))))$FOL,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_1[j])))))$FOW))
    
    
    cot_2$W_Minus_FO_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$FOW,
                                                subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_1)==as.character(cot_2$Team_2[j])))))$FOL))
    
    
  }
  
  result<-rbind(result,cot_2) 
  
}

#write.csv(result, "Factors_Calculation_NHL_SK.csv", row.names = FALSE)

  Season_list<-19
  Flag_filter<-"Flag_Win_without_OT"
  Koef_filter<-"1st_Team_Win_without OT"
  TS<-0.84
  pred_index<-0.55
  koef_index<-1.0
  
  data<-read.csv(paste(directory, "Factors_Calculation_NHL_SK.csv", sep=""), sep=",",h=T)
  data$Date<-as.Date(data$Date,"%Y-%m-%d")
  
  colnames(data)[match(Flag_filter ,colnames(data))]<-"Flag"
  
  validation<-subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 &  data$Season_Flag!=Season_list)
  control<-   subset(result, result$Match_T1_Home>=5 & result$Match_T2_Guest>=5 &  result$Season_Flag==Season_list)
  #control<-   subset(data, data$Match_T1_Home>=5 & data$Match_T2_Guest>=5 &  data$Season_Flag==Season_list)
  
  validation<-validation[with(validation,order(validation$Date)),]
  control<-control[with(control,order(control$Date)),]
  
  colnames(control)[match(Flag_filter ,colnames(control))]<-"Flag"
  
  validation<-validation[,c("Flag",
                            "W_Minus_Shots_T2_All",
                            "W_Total_Win_without_OT_T1_All",
                            "W_Minus_Shots_T1_All",
                            "W_Plus_Shots_T2_All",
                            "W_Plus_FO_T2_All",
                            "W_Plus_Shots_T1_All",
                            "W_Total_More_7_T2_All",
                            "W_Minus_Goals_T1_All",
                            "W_Points_T1_Al",
                            "W_Plus_PP_T1_All",
                            "W_Total_Win_without_OT_T2_All",
                            "W_Minus_Goals_T2_All",
                            "W_Total_Defeat_without_OT_T2_All",
                            "W_Total_More_6_T2_All",
                            "W_Plus_FO_T1_All",
                            "W_Minus_FO_T1_All",
                            "W_Plus_PP_T2_All",
                            "W_OT_T1_All",
                            "W_Total_Defeat_without_OT_T1_All",
                            "W_Minus_FO_T2_All",
                            "W_Minus_PP_T1_All",
                            "W_OT_T2_All",
                            "W_Total_More_5_T1_All",
                            "W_Plus_Goals_T1_All",
                            "W_Plus_Goals_T2_All",
                            "W_Minus_PP_T2_All",
                            "W_PPO_T2_All",
                            "W_Total_More_6_T1_All",
                            "W_Total_Win_T1_All",
                            "W_PPO_T1_All",
                            "W_Total_Win_T2_All",
                            "W_Points_T2_All_T2_All",
                            "W_Minus_S_O_T2_All",
                            "W_Minus_S_O_T1_All",
                            "W_Total_Defeat_T1_All",
                            "W_Plus_S_O_T1_All",
                            "W_Plus_S_O_T2_All",
                            "W_Total_Defeat_T2_All",
                            "W_Ind_Total_more_4_T1_All",
                            "W_Ind_Total_more_4_T2_All")]
  
  control_League<-control$League
  control_Date<-control$Date
  control_Team_1<-control$Team_1
  control_Team_2<-control$Team_2
  control_Goal_1<-control$Goal_1
  control_Goal_2<-control$Goal_2
  control_Flag<-control$Flag
  
  control<-control[,c(  "W_Minus_Shots_T2_All",
                        "W_Total_Win_without_OT_T1_All",
                        "W_Minus_Shots_T1_All",
                        "W_Plus_Shots_T2_All",
                        "W_Plus_FO_T2_All",
                        "W_Plus_Shots_T1_All",
                        "W_Total_More_7_T2_All",
                        "W_Minus_Goals_T1_All",
                        "W_Points_T1_Al",
                        "W_Plus_PP_T1_All",
                        "W_Total_Win_without_OT_T2_All",
                        "W_Minus_Goals_T2_All",
                        "W_Total_Defeat_without_OT_T2_All",
                        "W_Total_More_6_T2_All",
                        "W_Plus_FO_T1_All",
                        "W_Minus_FO_T1_All",
                        "W_Plus_PP_T2_All",
                        "W_OT_T1_All",
                        "W_Total_Defeat_without_OT_T1_All",
                        "W_Minus_FO_T2_All",
                        "W_Minus_PP_T1_All",
                        "W_OT_T2_All",
                        "W_Total_More_5_T1_All",
                        "W_Plus_Goals_T1_All",
                        "W_Plus_Goals_T2_All",
                        "W_Minus_PP_T2_All",
                        "W_PPO_T2_All",
                        "W_Total_More_6_T1_All",
                        "W_Total_Win_T1_All",
                        "W_PPO_T1_All",
                        "W_Total_Win_T2_All",
                        "W_Points_T2_All_T2_All",
                        "W_Minus_S_O_T2_All",
                        "W_Minus_S_O_T1_All",
                        "W_Total_Defeat_T1_All",
                        "W_Plus_S_O_T1_All",
                        "W_Plus_S_O_T2_All",
                        "W_Total_Defeat_T2_All",
                        "W_Ind_Total_more_4_T1_All",
                        "W_Ind_Total_more_4_T2_All"
  )]
  
  
  #matrix<-xgb.importance(model=bst)
  
  bst <- xgb.load(paste(directory, 'xgb.model_hockey_', Koef_filter, sep=""))
  
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
                    1.9)
  
  names(sample)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef")
  sample<-sample[with(sample,order(sample$Date)),]
  plot.roc(sample$Flag, sample$Pred,print.auc=T)
  
  #as.numeric(auc(sample$Flag,sample$Pred))
  
  # if (as.numeric(auc(sample$Flag,sample$Pred))>max)
  # {
  #   short_list<-c(short_list, list[i])
  #   max<-as.numeric(auc(sample$Flag,sample$Pred))
  # }
  # 
  # }  
  
  # sample_Win<-subset(sample, sample$Pred>=0.64)
  # sample_Win<-sample_Win[with(sample_Win,order(sample_Win$Date)),]
  # nrow(subset(sample_Win, sample_Win$Flag==1))/nrow(sample_Win)
  # profit(sample_Win)
  
  sample_test=data.frame(sample$Date,  sample$Team_1, sample$Team_2, sample$Goal_1, sample$Goal_2, sample$Flag, 
                         sample$Pred, sample$Koef)
  
  names(sample_test)<-c("Date","Team_1", "Team_2", "Goal_1", "Goal_2", "Flag", "Pred", "Koef")
  #sample_test$Flag<-ifelse(sample_test$Goal_2-sample_test$Goal_1>=5,1,0)
  
  sample_test$Date<-as.Date(ifelse(is.na(sample_test$Flag)==TRUE, Sys.Date()+1, sample_test$Date), origin="1970-01-01")
  
  sample_test<-sample_test[with(sample_test, order(sample_test$Date)),]
  sample_Win<-                     find_best_strategy(sample_test)
  profit(sample_Win)
  
  sample_Win$Exodus<-"1st Team Win without OT"
  
  write.csv(merge(x = subset(sample_Win, is.na(sample_Win$Flag)==TRUE)[,c("Date", "Team_1", "Team_2", "Exodus")], 
                  y = time, by = c("Team_1", "Team_2"), all = FALSE),
            paste(directory, "Hockey_Forecasts.csv", sep=""), row.names = FALSE)
  



weighted_mean<-function(x)
{
  a<-c(rep(0,length(x)))
  
  for(i in 1:length(x))
  {
    a[i]<-i/length(x)
  }
  
  return (sum(a*x)/length(x))
}

setwd("C:/Users/mb29945/Desktop/SK/Machine_Learning/Machine_Learning/SPORTS/Basketball")
cot<-read.csv("Basketball_All_Seasons.csv",sep=";",h=T)
cot$DATE_1<-as.Date(cot$DATE_1,"%m/%d/%Y")  
cot<-cot[with(cot,order(cot$DATE_1)),]
cot$Win<-as.numeric(sub(",", '', as.character(cot$Win)))/10000000
cot$Defeat<-as.numeric(sub(",", '', as.character(cot$Defeat)))/10000000

cot$OT1_1<-ifelse(is.na(cot$OT1_1)==TRUE,0,cot$OT1_1)
cot$OT2_1<-ifelse(is.na(cot$OT2_1)==TRUE,0,cot$OT2_1)
cot$OT3_1<-ifelse(is.na(cot$OT3_1)==TRUE,0,cot$OT3_1)
cot$OT4_1<-ifelse(is.na(cot$OT4_1)==TRUE,0,cot$OT4_1)
cot$OT5_1<-ifelse(is.na(cot$OT5_1)==TRUE,0,cot$OT5_1)

cot$OT1_2<-ifelse(is.na(cot$OT1_2)==TRUE,0,cot$OT1_2)
cot$OT2_2<-ifelse(is.na(cot$OT2_2)==TRUE,0,cot$OT2_2)
cot$OT3_2<-ifelse(is.na(cot$OT3_2)==TRUE,0,cot$OT3_2)
cot$OT4_2<-ifelse(is.na(cot$OT4_2)==TRUE,0,cot$OT4_2)
cot$OT5_2<-ifelse(is.na(cot$OT5_2)==TRUE,0,cot$OT5_2)

cot$OT_1<-cot$OT1_1+cot$OT2_1+cot$OT3_1+cot$OT4_1+cot$OT4_1+cot$OT5_1
cot$OT_2<-cot$OT1_2+cot$OT2_2+cot$OT3_2+cot$OT4_2+cot$OT4_2+cot$OT5_2

cot$REG_PO<-ifelse(grepl("Regular", cot$DATASET_1)==TRUE, "Regular", "Playoff")

cot$Win<-ifelse(is.na(cot$Win)==TRUE, 1.2, cot$Win) 
cot$Defeat<-ifelse(is.na(cot$Defeat)==TRUE, 1/(1-1/cot$Win), cot$Defeat)
           
        

cot<-cot[,c("Season", "REG_PO", "DATE_1", "TEAM_1", "TEAM_2", "PTS_1", "PTS_2", "Win", "Defeat", "X1Q_1", "X2Q_1", "X3Q_1", "X4Q_1", "OT_1", 
            "X1Q_2", "X2Q_2", "X3Q_2", "X4Q_2", "OT_2", 
            "FG_1",	"FGA_1",	"X3P_1",	"X3PA_1",	"FT_1",	"FTA_1",	"OR_1",	"DR_1",	"TOT_1",	
            "A_1",	"PF_1",	"ST_1",	"TO_1",	"TO.TO_1",	"BL_1", "POSS_1",	"PACE_1",	"OEFF_1",	"DEFF_1",
            "FG_2",	"FGA_2",	"X3P_2",	"X3PA_2",	"FT_2",	"FTA_2",	"OR_2",	"DR_2",	"TOT_2",	
            "A_2",	"PF_2",	"ST_2",	"TO_2",	"TO.TO_2",	"BL_2", "POSS_2",	"PACE_2",	"OEFF_2",	"DEFF_2")]


names(cot)<-c("Season","REG_PO", "Date", "Team_1", "Team_2", "Goal_1", "Goal_2", "Win", "Defeat", "X1Q_1", "X2Q_1", "X3Q_1", "X4Q_1", "OT_1", 
            "X1Q_2", "X2Q_2", "X3Q_2", "X4Q_2", "OT_2", 
            "FG_1",	"FGA_1",	"X3P_1",	"X3PA_1",	"FT_1",	"FTA_1",	"OR_1",	"DR_1",	"TOT_1",	
            "A_1",	"PF_1",	"ST_1",	"TO_1",	"TO.TO_1",	"BL_1", "POSS_1",	"PACE_1",	"OEFF_1",	"DEFF_1",
            "FG_2",	"FGA_2",	"X3P_2",	"X3PA_2",	"FT_2",	"FTA_2",	"OR_2",	"DR_2",	"TOT_2",	
            "A_2",	"PF_2",	"ST_2",	"TO_2",	"TO.TO_2",	"BL_2", "POSS_2",	"PACE_2",	"OEFF_2",	"DEFF_2")


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
    
    
    cot_2$W_TotaL_Win_T2_All[j]<-weighted_mean(c(subset(cot_2, (cot_2$Date<cot_2$Date[j] & ((as.character(cot_2$Team_2)==as.character(cot_2$Team_2[j])))))$Flag_Defeat,
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


a<-result

result$POSS_1<-ifelse(result$POSS_1=="",NA,result$POSS_1)
result$PACE_1<-ifelse(result$PACE_1=="",NA,result$PACE_1)
result$OEFF_1<-ifelse(result$OEFF_1=="",NA,result$OEFF_1)
result$DEFF_1<-ifelse(result$DEFF_1=="",NA,result$DEFF_1)

result$POSS_2<-ifelse(result$POSS_2=="",NA,result$POSS_2)
result$PACE_2<-ifelse(result$PACE_2=="",NA,result$PACE_2)
result$OEFF_2<-ifelse(result$OEFF_2=="",NA,result$OEFF_2)
result$DEFF_2<-ifelse(result$DEFF_2=="",NA,result$DEFF_2)


write.csv(result, "Factors_Calculation_Additional_Data_New_Info.csv", row.names=FALSE)

data_set<-subset(result, is.na(result$W_Plus_DEFF_T2_All)==FALSE & is.na(result$W_Minus_DEFF_T1_All)==FALSE & is.na(result$W_Minus_DEFF_T2_All)==FALSE)
varlist<-c(colnames(result[,c(79:(ncol(result)))]))

fit <- lm(Flag_Win_Fora_Minus_5 ~  W_Total_Win_T1_All        +          W_Total_Defeat_T1_All        +     W_Plus_Goals_T2_All   +        
                                   W_Total_More_210_T2_All   +          W_Ind_Total_more_90_T2_All   +     W_Plus_FG_T2_All      +                    
                                   W_Plus_3P_T2_All          +          W_Plus_3PA_T1_All            +     W_Plus_3PA_T2_All     +
                                   W_Minus_3PA_T1_All        +          W_Minus_3PA_T2_All           +     W_Plus_FT_T2_All      +           
                                   W_Plus_FTA_T1_All         +          W_Plus_OR_T1_All             +     W_Plus_DR_T1_All      +
                                                           W_Plus_DR_T2_All          +          W_Minus_DR_T1_All            +     W_Minus_DR_T2_All     +
                                   W_Plus_A_T1_All           +          W_Plus_A_T2_All              +     W_Minus_A_T1_All      +
                                   W_Minus_A_T2_All          +          W_Plus_ST_T1_All             +     W_Minus_ST_T2_All     +
                                   W_Plus_TO_T1_All          +          W_Minus_TO_T2_All            +     W_Plus_TO_TO_T1_All   +
                                   W_Minus_T0_TO_T2_All,                                                   data=data_set)

summary(fit)

data_set$Pred <- predict(fit, data_set)
data<-data_set[,c("Season", "Date","Team_1", "Team_2", "Flag_Win_Fora_Minus_5", "Goal_1", "Goal_2", "Pred")]
colnames(result)
write.csv(result, "Factors_Calculation_Additional_Data_New_Info.csv", row.names=FALSE)

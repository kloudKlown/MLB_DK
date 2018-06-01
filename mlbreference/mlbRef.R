# ############ Load Library
# install.packages("corrplot")
# install.packages("brnn")
# install.packages("h2o")
# install.packages("randomForest")
# install.packages("Matrix")
# install.packages("xgboost")
# install.packages("stringdist")
# install.packages("varhandle")
# install.packages("mxnet")

library(Hmisc)
library(corrplot)
library(brnn)
library(h2o)
library(randomForest)
library(Matrix)
library(xgboost)
library(stringdist)
library(varhandle)
library(tidyr)
require(devtools)
#install_version("DiagrammeR", version = "0.9.0", repos = "http://cran.us.r-project.org")
require(DiagrammeR)

library(mxnet)
library(Hmisc)


cleanData <- function(data, lowerLimit, higherLimit){
  data[data == "NULL"] = 0
  data[is.na(data)] = 0
  
  for(i in lowerLimit: higherLimit){
    print(i)
    if (is.numeric(data[,i]) != TRUE){
      data[,i] = as.numeric(levels(data[,i]))[data[,i]]  
    }
    
    i = i + 1
  }
  
    return(data)
}

B2017 = read.csv('All_MLB_Batters_17.csv')
P2017 = read.csv('All_MLB_Pitchers_17.csv')


B2018 = read.csv('All_MLB_Batters_18.csv')
P2018 = read.csv('All_MLB_Pitchers_18.csv')
CumulativeBattingStats2017 = read.csv("CumBatting2017.csv", header = TRUE)
CumulativePitchingStats2017 = read.csv("CumPitching2017.csv", header = TRUE)
CumulativeBattingStats2018 = read.csv("CumBatting2018.csv", header = TRUE)
CumulativePitchingStats2018 = read.csv("CumPitching2018.csv", header = TRUE)
pitchingStats2017 = read.csv("pitchingStats2017.csv", header = TRUE)


MergeData2017 = read.csv("MergeData2017.csv")
# MergeData2017$Date = as.Date(MergeData2017$Date, "%m/%d/%Y")
MergeData2018 = read.csv("MergeData2018.csv")

colnames(B2017) = c('Name','Bats','','Position', 'Gcar','Gtm','Date','Tm','Home','Opp','Rslt','Inngs','PA','AB','R','H','2B','3B','HR','RBI','BB','IBB','SO',
                 'HBP','SH','SF','ROE','GDP','SB','CS','BA','OBP','SLG','OPS','BOP','aLI','WPA','RE24','DK','FD','Pos')

colnames(B2018) = c('Name','Bats','','Position', 'Gcar','Gtm','Date','Tm','Home','Opp','Rslt','Inngs','PA','AB','R','H','2B','3B','HR','RBI','BB','IBB','SO',
                    'HBP','SH','SF','ROE','GDP','SB','CS','BA','OBP','SLG','OPS','BOP','aLI','WPA','RE24','DK','FD','Pos','empty')


## Cleaning Batting 2017
B2017 = cleanData(B2017, 13, 38)
B2017$Home = as.numeric(levels(B2017$Home))[B2017$Home] 
B2017$Home[is.na(B2017$Home)] <- 0


B2017$DK = B2017$H*3 + B2017$`2B`*3 + B2017$`3B` * 8 + B2017$HR * 10 + B2017$R * 2 + B2017$BB * 2 + B2017$HBP * 2 + B2017$SB * 5

#Cleaning Batting 2018
B2018 = cleanData(B2018, 13, 38)
B2018$Home = as.numeric(levels(B2018$Home))[B2018$Home] 
B2018$Home[is.na(B2018$Home)] <- 0

B2018$DK = B2018$H*3 + B2018$`2B`*3 + B2018$`3B` * 8 + B2018$HR * 10 + B2018$R * 2 + B2018$BB * 2 + B2018$HBP * 2 + B2018$SB * 5

colnames(P2017) = c('Name','Pitches','','Position','Gcar','Gtm','Date','Tm','Home','Opp','Rslt','Inngs','Dec','DR','IP','H','R',
  'ER','BB','SO','HR','HBP','ERA','BF','Pit','Str','StL','StS','GB','FB','LD','PU','Unk',
  'GSc','IR','IS','SB','CS','PO','AB','2B','3B','IBB','GDP','SF','ROE','aLI','WPA','RE24','DFS(DK)','DFS(FD)','Entered','Exited','empty')

colnames(P2018) = c('Name','Pitches','','Position','Gcar','Gtm','Date','Tm','Home','Opp','Rslt','Inngs','Dec','DR','IP','H','R',
                    'ER','BB','SO','HR','HBP','ERA','BF','Pit','Str','StL','StS','GB','FB','LD','PU','Unk',
                    'GSc','IR','IS','SB','CS','PO','AB','2B','3B','IBB','GDP','SF','ROE','aLI','WPA','RE24','DFS(DK)','DFS(FD)','Entered','Exited','empty')



#Cleaning Pitching 2017
P2017 = cleanData(P2017, 13, 50)
P2017$Home = as.numeric(levels(P2017$Home))[P2017$Home] 
P2017$Home[is.na(P2017$Home)] <- 0

#Cleaning Pitching 2017
P2018 = cleanData(P2018, 13, 50)
P2018$Home = as.numeric(levels(P2018$Home))[P2018$Home] 
P2018$Home[is.na(P2018$Home)] <- 0

P2017$DK = P2017$IP*2.25 + P2017$SO * 2 + P2017$ER * -2 + P2017$H *  -.6 + P2017$BB * -.6 
P2018$DK = P2018$IP*2.25 + P2018$SO * 2 + P2018$ER * -2 + P2018$H *  -.6 + P2018$BB * -.6 

B2018$Date =  as.Date(B2018$Date, "%b %d %Y")
B2017$Date =  as.Date(B2017$Date, "%b %d %Y")
tempB2018 = B2018[1,]
tempB2018$HRA = 0
tempB2018$SBA = 0
tempB2018$`2BA` = 0


CumulativeBattingStats2018 = tempB2018[0,]
rm(tempB2018)

PlayerList = unique(B2018$Name)

## Calculate Cumulative stats for each player in 2017/2018 season
for(i in 1:length(PlayerList)){
  
  subData = subset(B2018, B2018$Name == PlayerList[i])
  print(i)
  subData =  subData[ order(as.Date(subData$Date), decreasing = FALSE ), ]  
  for(j in 1:nrow(subData)){
    if ( j < 3){
      NewRow = subData[j,]
      NewRow$`2BA` = 0
      NewRow$HRA = 0
      NewRow$SBA = 0
      
      CumulativeBattingStats2018 = rbind(CumulativeBattingStats2018, NewRow)
    }
    else{
    
      NewRow = subData[j,]
      NewRow$PA = mean(subData[(j-3):(j-1),]$PA)
      NewRow$AB = mean(subData[(j-3):(j-1),]$AB)
      NewRow$R = mean(subData[(j-3):(j-1),]$R)
      NewRow$`2B` = mean(subData[(j-3):(j-1),]$`2B`)
      NewRow$`3B` = mean(subData[(j-3):(j-1),]$`3B`)
      NewRow$HR = mean(subData[(j-3):(j-1),]$HR)
      NewRow$RBI = mean(subData[(j-3):(j-1),]$RBI)
      NewRow$BB = mean(subData[(j-3):(j-1),]$BB)
      NewRow$IBB = mean(subData[(j-3):(j-1),]$IBB)
      NewRow$SO = mean(subData[(j-3):(j-1),]$SO)
      NewRow$HBP = mean(subData[(j-3):(j-1),]$HBP)
      NewRow$SH = mean(subData[(j-3):(j-1),]$SH)
      NewRow$SF = mean(subData[(j-3):(j-1),]$SF)
      NewRow$ROE = mean(subData[(j-3):(j-1),]$ROE)
      NewRow$GDP = mean(subData[(j-3):(j-1),]$GDP)
      NewRow$SB = mean(subData[(j-3):(j-1),]$SB)
      NewRow$CS = mean(subData[(j-3):(j-1),]$CS)
      NewRow$BA = mean(subData[(j-3):(j-1),]$BA)
      NewRow$OBP = mean(subData[(j-3):(j-1),]$OBP)
      NewRow$SLG = mean(subData[(j-3):(j-1),]$SLG)
      NewRow$OPS = mean(subData[(j-3):(j-1),]$OPS)
      NewRow$BOP = mean(subData[(j-3):(j-1),]$BOP)
      NewRow$aLI = mean(subData[(j-3):(j-1),]$aLI)
      NewRow$WPA = mean(subData[(j-3):(j-1),]$WPA)
      NewRow$RE24 = mean(subData[(j-3):(j-1),]$RE24)
      NewRow$`2BA` = subData[j,]$`2B`
      NewRow$HRA = subData[j,]$HR
      NewRow$SBA = subData[j,]$SB
      
      CumulativeBattingStats2018 = rbind(CumulativeBattingStats2018, NewRow)
    }
    
  }
}


P2018$Date =  as.Date(P2018$Date, "%b %d %Y")
CumulativePitchingStats2018 = P2018[0,]
PlayerList = unique(P2018$Name)

## Calculate Cumulative stats for each player in 2017/2018 season
for(i in 1:length(PlayerList)){
  
  subData = subset(P2018, P2018$Name == PlayerList[i])
  print(i)
  subData =  subData[ order(as.Date(subData$Date), decreasing = FALSE ), ]  
  for(j in 1:nrow(subData)){
    if ( j < 3){
      CumulativePitchingStats2018 = rbind(CumulativePitchingStats2018, subData[j,])
    }
    else{
    
      NewRow = subData[j,]
      NewRow$DR = mean(subData[(j-3):(j-1),]$DR)
      NewRow$IP = mean(subData[(j-3):(j-1),]$IP)
      NewRow$H = mean(subData[(j-3):(j-1),]$H)
      NewRow$`R` = mean(subData[(j-3):(j-1),]$`R`)
      NewRow$`ER` = mean(subData[(j-3):(j-1),]$`ER`)
      NewRow$BB = mean(subData[(j-3):(j-1),]$BB)
      NewRow$SO = mean(subData[(j-3):(j-1),]$SO)
      NewRow$HR = mean(subData[(j-3):(j-1),]$HR)
      NewRow$HBP = mean(subData[(j-3):(j-1),]$HBP)
      NewRow$ERA = mean(subData[(j-3):(j-1),]$ERA)
      NewRow$BF = mean(subData[(j-3):(j-1),]$BF)
      NewRow$Pit = mean(subData[(j-3):(j-1),]$Pit)
      NewRow$Str = mean(subData[(j-3):(j-1),]$Str)
      NewRow$StL = mean(subData[(j-3):(j-1),]$StL)
      NewRow$StS = mean(subData[(j-3):(j-1),]$StS)
      NewRow$GB = mean(subData[(j-3):(j-1),]$GB)
      NewRow$FB = mean(subData[(j-3):(j-1),]$FB)
      NewRow$LD = mean(subData[(j-3):(j-1),]$LD)
      NewRow$PU = mean(subData[(j-3):(j-1),]$PU)
      NewRow$Unk = mean(subData[(j-3):(j-1),]$Unk)
      NewRow$GSc = mean(subData[(j-3):(j-1),]$GSc)
      NewRow$IR = mean(subData[(j-3):(j-1),]$IR)
      NewRow$IS = mean(subData[(j-3):(j-1),]$IS)
      NewRow$SB = mean(subData[(j-3):(j-1),]$SB)
      NewRow$CS = mean(subData[(j-3):(j-1),]$CS)
      NewRow$PO = mean(subData[(j-3):(j-1),]$PO)
      NewRow$AB = mean(subData[(j-3):(j-1),]$AB)
      NewRow$`2B` = mean(subData[(j-3):(j-1),]$`2B`)
      NewRow$`3B` = mean(subData[(j-3):(j-1),]$`3B`)
      NewRow$IBB = mean(subData[(j-3):(j-1),]$IBB)
      NewRow$GDP = mean(subData[(j-3):(j-1),]$GDP)
      NewRow$SF = mean(subData[(j-3):(j-1),]$SF)
      NewRow$ROE = mean(subData[(j-3):(j-1),]$ROE)
      NewRow$aLI = mean(subData[(j-3):(j-1),]$aLI)
      NewRow$WPA = mean(subData[(j-3):(j-1),]$WPA)
      NewRow$RE24 = mean(subData[(j-3):(j-1),]$RE24)
      
      CumulativePitchingStats2018 = rbind(CumulativePitchingStats2018, NewRow)
    }
    
  }
}

write.csv(CumulativeBattingStats2018, file = "CumBatting2018.csv")
write.csv(CumulativePitchingStats2018, file = "CumPitching2018.csv")
CumulativeBattingStats2018 = read.csv("CumBatting2018.csv", header = TRUE)
CumulativePitchingStats2018 = read.csv("CumPitching2018.csv", header = TRUE)

B2018 = subset(B2018, as.character(B2018$Bats) == "B" | as.character(B2018$Bats) == "L" | 
  as.character(B2018$Bats) == "R")

B2017 = subset(B2017, as.character(B2017$Bats) == "B" | as.character(B2017$Bats) == "L" | 
                 as.character(B2017$Bats) == "R")


# testBatting = subset(CumulativeBattingStats, CumulativeBattingStats$Tm == "CLE")
testPitching = subset(CumulativePitchingStats2018, grepl("GS[0-9]", CumulativePitchingStats2018$Inngs))
names(testPitching)[names(testPitching) == 'Opp'] <- 'Opp2'
names(testPitching)[names(testPitching) == 'Tm'] <- 'Opp'


MergeData2018 = merge(CumulativeBattingStats2018, testPitching, by = c("Date","Opp"))
write.csv(MergeData2018, file = "MergeData2018.csv")
MergeData2018 = read.csv("MergeData2018.csv")

MergeData2018 = subset(MergeData2018,  !is.na(MergeData2018$Date))
names(MergeData2018)[names(MergeData2018) %nin% names(MergeData2017)]

MergeData2018 = subset(MergeData2018, select = -empty.x )
MergeData2018 = subset(MergeData2018, select = -empty.y )


# names(MergeData2018)[names(MergeData2018) == 'X2B.x'] <- '2B.x'
# names(MergeData2018)[names(MergeData2018) == 'X3B.x'] <- '3B.x'
# names(MergeData2018)[names(MergeData2018) == 'X2B.y'] <- '2B.y'
# names(MergeData2018)[names(MergeData2018) == 'X3B.y'] <- '3B.y'

names(MergeData2017)[names(MergeData2017) %nin% names(MergeData2018)]
MergeData2017 = subset(MergeData2017, select = -`NA.` )
MergeData2017 = subset(MergeData2017, select = -empty )

names(MergeData2017)[names(MergeData2017) %nin% names(MergeData2018)]
names(MergeData2018)[names(MergeData2018) %nin% names(MergeData2017)]

AllData = rbind(MergeData2017, MergeData2018)
AllData = unique(AllData)
################################## PITCHERS

# write.csv(MergeData2018, file = "MergeData2018.csv")

# spearmanP = varclus(as.matrix(AllData[,c( "AB.x","R.x","H.x","2B.x","3B.x","HR.x","RBI","BB.x",
#                                           "IBB.x","SO.x","HBP.x","SH","SF.x","ROE.x","GDP.x","SB.x","CS.x","BA",
#                                           "OBP","SLG","BOP","aLI.x","WPA.x","RE24.x",
#                                           "IP","H.y","ER","BB.y",
#                                           "SO.y","HR.y","HBP.y","ERA","Str","StL","StS","GB","FB",
#                                           "LD","PU","Unk","GSc","IR","IS","SB.y","CS.y","PO","2B.y",
#                                           "3B.y","IBB.y","GDP.y","SF.y","ROE.y","aLI.y","WPA.y")], similarity = "spearman"))
# plot(spearmanP)
# abline(h=0.3)

####### Pitching stats
DateCheck = "2018-5-17"
# testPitching = subset(CumulativePitchingStats2018, grepl("GS[0-9]", CumulativePitchingStats2018$Inngs))
testPitching = CumulativePitchingStats2018

PlayerList = testPitching$Name
testPitching$OppTeamH = 0
testPitching$OppTeamHR = 0
testPitching$OppTeamR =0
testPitching$OppTeam2B = 0
testPitching$OppTeam3B = 0
pitchingStats2018 = testPitching[0,]

for(each in 1:nrow(testPitching)){
  print(each)
  pitichingDate = as.Date(testPitching[each,]$Date)
  oppTeam =as.character(testPitching[each,]$Opp)
  OpposingTeam = subset(B2018, as.Date(B2018$Date) < pitichingDate & 
                          as.character(B2018$Tm) == oppTeam &
                          as.Date(B2018$Date) > (pitichingDate - 7))
  
  testPitching[each,]$OppTeamH = mean(OpposingTeam$H)
  testPitching[each,]$OppTeamHR = mean(OpposingTeam$HR)
  testPitching[each,]$OppTeamR = mean(OpposingTeam$R)
  testPitching[each,]$OppTeam2B = mean(OpposingTeam$`2B`)
  testPitching[each,]$OppTeam3B = mean(OpposingTeam$`3B`)
  
  pitchingStats2018 = rbind(pitchingStats2018, testPitching[each,])
                        
  
}

write.csv(pitchingStats2018, file = "pitchingStats2018.csv")
pitchingStats2018 = read.csv("pitchingStats2018.csv" , header = TRUE)
pitchingStats2017 = subset(pitchingStats2017, select = -X.2 )
pitchingStats2018 = subset(pitchingStats2018, select = -X.2 )

names(pitchingStats2018)[names(pitchingStats2018) %nin% names(pitchingStats2017)]
names(pitchingStats2017)[names(pitchingStats2017) %nin% names(pitchingStats2018)]

pitchingAll = rbind(pitchingStats2018, pitchingStats2017)

pitchingToday = pitchingAll


################## TOday data
DateCheck = "2018-6-1"

TodayData <- function(data, team, opp, pitcher){
  
  TodayBatting = subset(B2018, B2018$Tm == team | B2018$Tm == opp )
  TodayPitching = subset(P2018, P2018$Tm == team | P2018$Tm == opp )
  TodayPitching = subset(TodayPitching, TodayPitching$Tm == opp & TodayPitching$Name == pitcher)
  
  if(nrow(TodayPitching) == 0){
    TodayPitching = subset(P2017, P2017$Tm == team | P2017$Tm == opp )
    TodayPitching = subset(TodayPitching, TodayPitching$Name == pitcher)
  }
  
  TodayBatting =  TodayBatting[ order(TodayBatting$Date), ]
  TodayBatting = subset(TodayBatting, TodayBatting$Tm == team)
  
  ### Today's batting
  TodayCumBat = TodayBatting[0,]
  PlayerList = unique(TodayBatting$Name)
  
  for(i in 1:length(PlayerList)){
    
    subData = subset(TodayBatting, TodayBatting$Name == PlayerList[i])
    print(i)
    subData =  subData[ order(subData$Date), ]
    if (nrow(subData) > 0 && nrow(subData) < 3){
      subData = rbind(subData, subData)
      subData = rbind(subData, subData)
    }
    if (nrow(subData) > 2){
      for(j in nrow(subData):nrow(subData)){
        
        NewRow = subData[j,]
        NewRow$Date = format(Sys.time(), "%Y-%m-%d")
        NewRow$PA = mean(subData[(j-3):(j-1),]$PA)
        NewRow$AB = mean(subData[(j-3):(j-1),]$AB)
        NewRow$R = mean(subData[(j-3):(j-1),]$R)
        NewRow$`2B` = mean(subData[(j-3):(j-1),]$`2B`)
        NewRow$`3B` = mean(subData[(j-3):(j-1),]$`3B`)
        NewRow$HR = mean(subData[(j-3):(j-1),]$HR)
        NewRow$RBI = mean(subData[(j-3):(j-1),]$RBI)
        NewRow$BB = mean(subData[(j-3):(j-1),]$BB)
        NewRow$IBB = mean(subData[(j-3):(j-1),]$IBB)
        NewRow$SO = mean(subData[(j-3):(j-1),]$SO)
        NewRow$HBP = mean(subData[(j-3):(j-1),]$HBP)
        NewRow$SH = mean(subData[(j-3):(j-1),]$SH)
        NewRow$SF = mean(subData[(j-3):(j-1),]$SF)
        NewRow$ROE = mean(subData[(j-3):(j-1),]$ROE)
        NewRow$GDP = mean(subData[(j-3):(j-1),]$GDP)
        NewRow$SB = mean(subData[(j-3):(j-1),]$SB)
        NewRow$CS = mean(subData[(j-3):(j-1),]$CS)
        NewRow$BA = mean(subData[(j-3):(j-1),]$BA)
        NewRow$OBP = mean(subData[(j-3):(j-1),]$OBP)
        NewRow$SLG = mean(subData[(j-3):(j-1),]$SLG)
        NewRow$OPS = mean(subData[(j-3):(j-1),]$OPS)
        NewRow$BOP = mean(subData[(j-3):(j-1),]$BOP)
        NewRow$aLI = mean(subData[(j-3):(j-1),]$aLI)
        NewRow$WPA = mean(subData[(j-3):(j-1),]$WPA)
        NewRow$RE24 = mean(subData[(j-3):(j-1),]$RE24)
        
        TodayCumBat = rbind(TodayCumBat, NewRow)
      }
    }
    
  }
  
  ### Today's pitching
  
  TodayCumPitch = P2018[0,]
  PlayerList = unique(TodayPitching$Name)
  
  for(i in 1:length(PlayerList)){
    
    subData = subset(TodayPitching, TodayPitching$Name == PlayerList[i])
    print(i)
    subData =  subData[ order(subData$Date), ]
    if (nrow(subData) > 0 && nrow(subData) < 3){
      subData = rbind(subData, subData)
      subData = rbind(subData, subData)
    }
    
    if (nrow(subData) > 2){
      
      for(j in nrow(subData):nrow(subData)){
        
        NewRow = subData[j,]
        NewRow$Date = format(Sys.time(),"%Y-%m-%d")
        NewRow$DR = mean(subData[(j-3):(j-1),]$DR)
        NewRow$IP = mean(subData[(j-3):(j-1),]$IP)
        NewRow$H = mean(subData[(j-3):(j-1),]$H)
        NewRow$`R` = mean(subData[(j-3):(j-1),]$`R`)
        NewRow$`ER` = mean(subData[(j-3):(j-1),]$`ER`)
        NewRow$BB = mean(subData[(j-3):(j-1),]$BB)
        NewRow$SO = mean(subData[(j-3):(j-1),]$SO)
        NewRow$HR = mean(subData[(j-3):(j-1),]$HR)
        NewRow$HBP = mean(subData[(j-3):(j-1),]$HBP)
        NewRow$ERA = mean(subData[(j-3):(j-1),]$ERA)
        NewRow$BF = mean(subData[(j-3):(j-1),]$BF)
        NewRow$Pit = mean(subData[(j-3):(j-1),]$Pit)
        NewRow$Str = mean(subData[(j-3):(j-1),]$Str)
        NewRow$StL = mean(subData[(j-3):(j-1),]$StL)
        NewRow$StS = mean(subData[(j-3):(j-1),]$StS)
        NewRow$GB = mean(subData[(j-3):(j-1),]$GB)
        NewRow$FB = mean(subData[(j-3):(j-1),]$FB)
        NewRow$LD = mean(subData[(j-3):(j-1),]$LD)
        NewRow$PU = mean(subData[(j-3):(j-1),]$PU)
        NewRow$Unk = mean(subData[(j-3):(j-1),]$Unk)
        NewRow$GSc = mean(subData[(j-3):(j-1),]$GSc)
        NewRow$IR = mean(subData[(j-3):(j-1),]$IR)
        NewRow$IS = mean(subData[(j-3):(j-1),]$IS)
        NewRow$SB = mean(subData[(j-3):(j-1),]$SB)
        NewRow$CS = mean(subData[(j-3):(j-1),]$CS)
        NewRow$PO = mean(subData[(j-3):(j-1),]$PO)
        NewRow$AB = mean(subData[(j-3):(j-1),]$AB)
        NewRow$`2B` = mean(subData[(j-3):(j-1),]$`2B`)
        NewRow$`3B` = mean(subData[(j-3):(j-1),]$`3B`)
        NewRow$IBB = mean(subData[(j-3):(j-1),]$IBB)
        NewRow$GDP = mean(subData[(j-3):(j-1),]$GDP)
        NewRow$SF = mean(subData[(j-3):(j-1),]$SF)
        NewRow$ROE = mean(subData[(j-3):(j-1),]$ROE)
        NewRow$aLI = mean(subData[(j-3):(j-1),]$aLI)
        NewRow$WPA = mean(subData[(j-3):(j-1),]$WPA)
        NewRow$RE24 = mean(subData[(j-3):(j-1),]$RE24)
        
        TodayCumPitch = rbind(TodayCumPitch, NewRow)
        
        
      }
      
      }
    
  }
  
  names(TodayCumPitch)[names(TodayCumPitch) == 'Opp'] <- 'Opp2'
  names(TodayCumPitch)[names(TodayCumPitch) == 'Tm'] <- 'Opp'
  TodayCumPitch$Opp = opp
  TodayCumBat$Opp = opp
  Todayz = merge(TodayCumBat, TodayCumPitch, by = c("Date","Opp"))
  names(Todayz)[names(Todayz) %nin% names(MergeData2018)]
  
  Todayz = subset(Todayz, select = -empty.y )
  Todayz = subset(Todayz, select = -`DFS(FD)` )
  Todayz = subset(Todayz, select = -empty.x )
  Todayz = subset(Todayz, select = -`DFS(DK)` )
  
  names(Todayz)[names(Todayz) %nin% names(MergeData2018)]
  names(MergeData2017)[names(MergeData2017) %nin% names(Todayz)]

  return(Todayz)
}

TodayPitcher <- function(data, team, opp, pitcher){
  pitchingToday = subset(pitchingAll, pitchingAll$Name == pitcher)
  
  pitchingToday =  pitchingToday[ order(as.Date(pitchingToday$Date), decreasing = TRUE), ]

 
  OpposingTeam = subset(B2018, as.Date(B2018$Date) < DateCheck & 
                          as.character(B2018$Tm) == team &
                          as.Date(B2018$Date) > (DateCheck - 7))
  
  pitchingToday[1,]$OppTeamH = mean(OpposingTeam$H)
  pitchingToday[1,]$OppTeamHR = mean(OpposingTeam$HR)
  pitchingToday[1,]$OppTeamR = mean(OpposingTeam$R)
  pitchingToday[1,]$OppTeam2B = mean(OpposingTeam$`2B`)
  pitchingToday[1,]$OppTeam3B = mean(OpposingTeam$`3B`)
  Ptest = rbind(Ptest, pitchingToday[1,] )
  return(Ptest)
}

Today=MergeData2018[0,]
DateCheck = as.Date(DateCheck)
PitchersToday = pitchingToday[0,]
Ptest = pitchingToday[0,]



Today  = rbind(Today ,TodayData("", "NYY", "BAL", "Andrew Cashner"))
Today  = rbind(Today ,TodayData("", "BAL", "NYY", "Sonny Gray"))
Today  = rbind(Today ,TodayData("", "CHC", "NYM", "Zach Wheeler"))
Today  = rbind(Today ,TodayData("", "NYM", "CHC", "Tyler Chatwood"))
Today  = rbind(Today ,TodayData("", "TOR", "DET", "Blaine Hardy"))
Today  = rbind(Today ,TodayData("", "DET", "TOR", "Jaime Garcia"))
Today  = rbind(Today ,TodayData("", "WSN", "ATL", "Mike Foltynewicz"))
Today  = rbind(Today ,TodayData("", "ATL", "WSN", "Stephen Strasburg"))
Today  = rbind(Today ,TodayData("", "BOS", "HOU", "Gerrit Cole"))
Today  = rbind(Today ,TodayData("", "HOU", "BOS", "Chris Sale"))
Today  = rbind(Today ,TodayData("", "MIL", "CHW", "Hector Santiago"))
Today  = rbind(Today ,TodayData("", "CHW", "MIL", "Chase Anderson"))
Today  = rbind(Today ,TodayData("", "CLE", "MIN", "Jose Berrios"))
Today  = rbind(Today ,TodayData("", "MIN", "CLE", "Carlos Carrasco"))
Today  = rbind(Today ,TodayData("", "OAK", "KCR", "Ian Kennedy"))
Today  = rbind(Today ,TodayData("", "KCR", "OAK", "Frankie Montas"))
Today  = rbind(Today ,TodayData("", "PIT", "STL", "Miles Mikolas"))
Today  = rbind(Today ,TodayData("", "STL", "PIT", "Jameson Taillon"))
Today  = rbind(Today ,TodayData("", "LAD", "COL", "Tyler Anderson"))
Today  = rbind(Today ,TodayData("", "COL", "LAD", "Scott Alexander"))
Today  = rbind(Today ,TodayData("", "MIA", "ARI", "Clay Buchholz"))
Today  = rbind(Today ,TodayData("", "ARI", "MIA", "Elieser Hernandez"))
Today  = rbind(Today ,TodayData("", "TEX", "LAA", "Jaime Barria"))
Today  = rbind(Today ,TodayData("", "LAA", "TEX", "Bartolo Colon"))
Today  = rbind(Today ,TodayData("", "CIN", "SDP", "Andrew Lockett"))
Today  = rbind(Today ,TodayData("", "SDP", "CIN", "Tyler Mahle"))
Today  = rbind(Today ,TodayData("", "TBR", "SEA", "Mike Leake"))
Today  = rbind(Today ,TodayData("", "SEA", "TBR", "Sergio Romo"))
Today  = rbind(Today ,TodayData("", "PHI", "SFG", "Chris Stratton"))
Today  = rbind(Today ,TodayData("", "SFG", "PHI", "Nick Pivetta"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "BAL", "NYY", "Sonny Gray"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "NYY", "BAL", "Andrew Cashner"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "NYM", "CHC", "Tyler Chatwood"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "CHC", "NYM", "Zach Wheeler"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "DET", "TOR", "Jaime Garcia"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "TOR", "DET", "Blaine Hardy"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "ATL", "WSN", "Stephen Strasburg"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "WSN", "ATL", "Mike Foltynewicz"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "HOU", "BOS", "Chris Sale"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "BOS", "HOU", "Gerrit Cole"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "CHW", "MIL", "Chase Anderson"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "MIL", "CHW", "Hector Santiago"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "MIN", "CLE", "Carlos Carrasco"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "CLE", "MIN", "Jose Berrios"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "KCR", "OAK", "Frankie Montas"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "OAK", "KCR", "Ian Kennedy"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "STL", "PIT", "Jameson Taillon"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "PIT", "STL", "Miles Mikolas"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "COL", "LAD", "Scott Alexander"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "LAD", "COL", "Tyler Anderson"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "ARI", "MIA", "Elieser Hernandez"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "MIA", "ARI", "Clay Buchholz"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "LAA", "TEX", "Bartolo Colon"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "TEX", "LAA", "Jaime Barria"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "SDP", "CIN", "Tyler Mahle"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "CIN", "SDP", "Andrew Lockett"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "SEA", "TBR", "Sergio Romo"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "TBR", "SEA", "Mike Leake"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "SFG", "PHI", "Nick Pivetta"))
PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "PHI", "SFG", "Chris Stratton"))



####### MErge with Pitching stats from BBSavant





#####################################
DateCheck = "2018-6-1"
AllData =  subset(AllData, !grepl('[0-9]', AllData$Name.y))
AllData =  subset(AllData, !grepl('[0-9]', AllData$Name.x))
names(AllData)[names(AllData) == 'X2B.x'] <- '2B.x'
names(AllData)[names(AllData) == 'X3B.x'] <- '3B.x'
names(AllData)[names(AllData) == 'X2B.y'] <- '2B.y'
names(AllData)[names(AllData) == 'X3B.y'] <- '3B.y'

AllDataCleaned = AllData[,c("Date","Opp","Name.x","Position.x","Bats","Tm","AB.x","R.x","H.x","2B.x","3B.x","HR.x","RBI","BB.x",
                            "IBB.x","SO.x","HBP.x","SH","SF.x","ROE.x","GDP.x","SB.x","CS.x","BA",
                            "OBP","SLG","BOP","aLI.x","WPA.x","RE24.x","DK.x" ,"HRA","SBA",
                            "IP","H.y","ER","BB.y","Name.y","Pitches",
                            "SO.y","HR.y","HBP.y","ERA","Str","StL","StS","GB","FB",
                            "LD","PU","Unk","GSc","IR","IS","SB.y","CS.y","PO","2B.y",
                            "3B.y","IBB.y","GDP.y","SF.y","ROE.y","aLI.y","WPA.y",
                            
                            'PBP1','PBP2','PBP3','PBP4','PBP5','PBP6','PBP7','PBP8','PBP9','PBPQ1','PBPQ2',
                            'PBPQ3','PBPQ4',
                            'EV1A','EVA2','EVA3','EVA4','EVA5','EVA6','EVA7','EVA8','EVA9','EVQA1',
                            'EVQA2',
                            'PB1','PB2','PB3','PB4','PB5','PB6','PB7','PB8','PB9',
                            'EV1','EV2','EV3','EV4','EV5','EV6','EV7','EV8','EV9'
                            
                            )]
AllDataCleaned = unique(AllDataCleaned)
# names(AllDataCleaned)[names(AllDataCleaned) == 'Player.x'] <- 'Player'
# AllDataCleaned$Date = as.Date(AllDataCleaned$Date, "%m/%d/%Y")
AllDataCleaned$Date = as.Date(AllDataCleaned$Date, "%Y-%m-%d")
# Batters2016Cleaned_Test = subset(AllDataCleaned, (as.Date(AllDataCleaned$Date) == as.Date(DateCheck)))

Batters2016Cleaned_Test = Today
Today$DK.x = 0
Todayz = Today
Batters2016Cleaned_Train = subset(AllDataCleaned, as.Date(AllDataCleaned$Date) < as.Date(DateCheck))


playerNames = (Batters2016Cleaned_Test$Name.x)


ResultsBatters = data.frame( Date = factor(),RFPred = numeric(), Name = factor(), HTeam = factor(), Pos = factor(), Actual = numeric(),
                             HR = numeric(), SB=numeric(), H = numeric(), HRSplitL30 = numeric(),
                             HSplitL30 = numeric(), HRSplit = numeric(), HSplit = numeric(),
                             SplitsGood = numeric(), Splits30Good = numeric())

for (each in 1:length(playerNames)){
  # Batters2016Cleaned_Test = subset(AllDataCleaned, as.Date(AllDataCleaned$Date) == DateCheck
  #                                  & AllDataCleaned$Name.x == as.character(playerNames[each]) )
  # didPlay = subset(B2018, as.Date(B2018$Date) == DateCheck
  #                  & B2018$Name == as.character(playerNames[each]) & B2018$PA  > 2)
  # if (nrow(didPlay) != 1){
  #   next
  # }

  

  Batters2016Cleaned_Test = subset(Todayz, as.Date(Todayz$Date) == DateCheck
  & Todayz$Name.x == as.character(playerNames[each]) )
  

  
  Batters2016Cleaned_Train = subset(AllDataCleaned, as.Date(AllDataCleaned$Date) < DateCheck
                                    & AllDataCleaned$Name.x == as.character(playerNames[each]) )
  
  Batters2016Subset = subset( AllDataCleaned, as.Date(AllDataCleaned$Date) < DateCheck &
                                    as.Date(AllDataCleaned$Date) > as.Date(DateCheck) - 15 
                                    & AllDataCleaned$Name.x == as.character(playerNames[each]) )
                                    
                                    
  print (playerNames[each])
  print (each)
  ### This ensures atleast 1 row of data exists for prediction
  if (nrow(Batters2016Cleaned_Test) < 1 ){
    next
  }

  
  if (nrow(Batters2016Cleaned_Train) < 1){
    next
  }
  Batters2016Cleaned_Train[is.na(Batters2016Cleaned_Train)] = 0
  Batters2016Cleaned_Test[is.na(Batters2016Cleaned_Test)] = 0
  
  rf = randomForest( Batters2016Cleaned_Train[,c("AB.x","R.x","2B.x","3B.x","HR.x","RBI","BB.x", 
                                                 "IBB.x","SO.x","HBP.x","SH","SF.x","ROE.x","GDP.x","SB.x","CS.x","BA",
                                                 "OBP","SLG","BOP","aLI.x","WPA.x","RE24.x",
                                                 "IP","H.y","ER","BB.y",
                                                 "SO.y","HR.y","HBP.y","ERA","Str","StL","StS","GB","FB",
                                                 "LD","PU","Unk","GSc","IR","IS","SB.y","CS.y","PO","2B.y",
                                                 "3B.y","IBB.y","GDP.y","SF.y","ROE.y","aLI.y","WPA.y",
                                                 'PBP1','PBP2','PBP3','PBP4','PBP5','PBP6','PBP7','PBP8','PBP9','PBPQ1','PBPQ2',
                                                 'PBPQ3','PBPQ4',
                                                 'EV1A','EVA2','EVA3','EVA4','EVA5','EVA6','EVA7','EVA8','EVA9','EVQA1',
                                                 'EVQA2',
                                                 'PB1','PB2','PB3','PB4','PB5','PB6','PB7','PB8','PB9',
                                                 'EV1','EV2','EV3','EV4','EV5','EV6','EV7','EV8','EV9'
                                                 )], 
                     y = Batters2016Cleaned_Train[,c("DK.x" )], ntree=500
                     ,type='regression')
  RFPred = predict( rf,  Batters2016Cleaned_Test[,c("AB.x","R.x","2B.x","3B.x","HR.x","RBI","BB.x", 
                                                    "IBB.x","SO.x","HBP.x","SH","SF.x","ROE.x","GDP.x","SB.x","CS.x","BA",
                                                    "OBP","SLG","BOP","aLI.x","WPA.x","RE24.x",
                                                    "IP","H.y","ER","BB.y",
                                                    "SO.y","HR.y","HBP.y","ERA","Str","StL","StS","GB","FB",
                                                    "LD","PU","Unk","GSc","IR","IS","SB.y","CS.y","PO","2B.y",
                                                    "3B.y","IBB.y","GDP.y","SF.y","ROE.y","aLI.y","WPA.y",
                                                    'PBP1','PBP2','PBP3','PBP4','PBP5','PBP6','PBP7','PBP8','PBP9','PBPQ1','PBPQ2',
                                                    'PBPQ3','PBPQ4',
                                                    'EV1A','EVA2','EVA3','EVA4','EVA5','EVA6','EVA7','EVA8','EVA9','EVQA1',
                                                    'EVQA2',
                                                    'PB1','PB2','PB3','PB4','PB5','PB6','PB7','PB8','PB9',
                                                    'EV1','EV2','EV3','EV4','EV5','EV6','EV7','EV8','EV9')] 
                    ,type = c("response") )
  Prediction2 =  as.data.frame(RFPred)
  
  Prediction2["Name"] =  as.data.frame(Batters2016Cleaned_Test$Name.x)
  Prediction2["HTeam"] =  as.data.frame(Batters2016Cleaned_Test$Tm)
  Prediction2["Pos"] =  as.data.frame(Batters2016Cleaned_Test$Position.x)
  Prediction2["Actual"] =  as.data.frame(Batters2016Cleaned_Test$DK.x)
  Prediction2$H = mean(Batters2016Subset$H.x)
  Prediction2$SB = sum(Batters2016Subset$SBA)
  Prediction2$HR = sum(Batters2016Subset$HRA)
  
  Batters2016Subset = subset( AllDataCleaned, as.Date(AllDataCleaned$Date) < DateCheck &
                                as.Date(AllDataCleaned$Date) > as.Date(DateCheck) - 30 
                              & AllDataCleaned$Name.x == as.character(playerNames[each]) &
                               as.character(  AllDataCleaned$Pitches ) == as.character(Batters2016Cleaned_Test$Pitches))
  
  
  Total = subset( AllDataCleaned, as.Date(AllDataCleaned$Date) < DateCheck &
                                as.Date(AllDataCleaned$Date) > as.Date(DateCheck) - 30 
                              & AllDataCleaned$Name.x == as.character(playerNames[each]) )
  
  Prediction2$HRSplitL30 = sum(Batters2016Subset$HRA)/(sum(Total$HRA) + 1)
  
  Prediction2$HSplitL30 = sum(Batters2016Subset$H.x)/(sum(Total$H.x) + 1)
  
  
  Batters2016Subset = subset( AllDataCleaned, as.Date(AllDataCleaned$Date) < DateCheck 
                              & AllDataCleaned$Name.x == as.character(playerNames[each]) &
                                as.character(  AllDataCleaned$Pitches ) == as.character(Batters2016Cleaned_Test$Pitches))
  
  Prediction2$HRSplit = sum(Batters2016Subset$HRA)/(sum(Batters2016Cleaned_Train$HRA) + 1)
  
  Prediction2$HSplit = sum(Batters2016Subset$H.x)/(sum(Batters2016Cleaned_Train$H.x) + 1)
  
  Prediction2$SplitsGood = 0
  if (Prediction2$HRSplit > 0.5 & Prediction2$HSplit > 0.5){
    Prediction2$SplitsGood = 1  
  }
  
  Prediction2$Splits30Good = 0
  if (Prediction2$HSplitL30 > 0.5 & Prediction2$HRSplitL30 > 0.5){
    Prediction2$Splits30Good = 1  
  }
  
  Prediction2$Date = as.character(DateCheck)
  ResultsBatters = rbind(ResultsBatters, Prediction2)
}

write.csv(ResultsBatters, file = "5-31.csv", row.names = FALSE)

########### Pitching Pred
# DateCheck = "2018-5-19"
PitchingAllDataCleaned = pitchingAll[,c("Name","Pitches","Date","Tm","Home","Opp",
                                "Dec","DR","IP","H","R","ER","BB","SO","HR","HBP",
                                "ERA","BF","Pit","Str","StL","StS","GB","FB","LD","PU","Unk","GSc",
                                "IR","IS","SB","CS","PO","AB","X2B","X3B","IBB","GDP","SF","ROE","aLI",
                                "WPA","RE24","DFS.DK.","DFS.FD.","Entered","Exited","empty","DK","OppTeamH",
                                "OppTeamHR","OppTeamR","OppTeam2B","OppTeam3B")]
PitchingAllDataCleaned = unique(PitchingAllDataCleaned)
PitchingAllDataCleaned$Date = as.Date(PitchingAllDataCleaned$Date, "%Y-%m-%d")
# Pitchers2016Cleaned_Test = subset(PitchingAllDataCleaned, (as.Date(PitchingAllDataCleaned$Date) == as.Date(DateCheck)))
Pitchers2016Cleaned_Test = PitchersToday
Pitchers2016Cleaned_Train = subset(PitchingAllDataCleaned, as.Date(PitchingAllDataCleaned$Date) < as.Date(DateCheck))


playerNames = (Pitchers2016Cleaned_Test$Name)


ResultsPitchers = data.frame( RFPred = numeric(), Name = factor(), HTeam = factor(), Pos = factor(), Actual = numeric())

for (each in 1:length(playerNames)){
  # Batters2016Cleaned_Test = subset(PitchingAllDataCleaned, as.Date(PitchingAllDataCleaned$Date) == DateCheck
  #                                  & PitchingAllDataCleaned$Name == as.character(playerNames[each]) )
  # if (Batters2016Cleaned_Test$IP < 3){
  #   next
  # }

   Batters2016Cleaned_Test = subset(PitchersToday,
                                     PitchersToday$Name == as.character(playerNames[each]) )
  
  Batters2016Cleaned_Train = subset(PitchingAllDataCleaned, as.Date(PitchingAllDataCleaned$Date) < DateCheck
                                    & PitchingAllDataCleaned$Name == as.character(playerNames[each]) )
  
  if (nrow(Batters2016Cleaned_Train) < 2){
    next
  }
  
  Batters2016Cleaned_Train =  Batters2016Cleaned_Train[ order(as.Date(Batters2016Cleaned_Train$Date), decreasing = TRUE), ]
  
  # Batters2016Cleaned_Train = Batters2016Cleaned_Train[2:(nrow(Batters2016Cleaned_Train)),]
  
  print (playerNames[each])
  print (each)
  ### This ensures atleast 1 row of data exists for prediction
  if (nrow(Batters2016Cleaned_Test) < 1 ){
    next
  }
  
  if (nrow(Batters2016Cleaned_Train) < 1){
    next
  }
  Batters2016Cleaned_Train[is.na(Batters2016Cleaned_Train)] = 0
  Batters2016Cleaned_Test[is.na(Batters2016Cleaned_Test)] = 0

  rfP = data.frame()
  # Prediction2 = data.frame( RFPred = numeric(), Name = factor(), HTeam = factor(), Pos = factor(), Actual = numeric())
  for(j in 1:5){
    
    rf = randomForest( Batters2016Cleaned_Train[,c("Pitches","Home",
                                                   "Dec","DR","IP","H","R","ER","BB","SO","HR","HBP",
                                                   "ERA","BF","Pit","Str","StL","StS","GB","FB","LD","PU","Unk","GSc",
                                                   "IR","IS","SB","CS","PO","AB","X2B","X3B","IBB","GDP","SF","ROE","aLI",
                                                   "WPA","RE24","Entered","empty","OppTeamH",
                                                   "OppTeamHR","OppTeamR","OppTeam2B","OppTeam3B")], 
                       y = Batters2016Cleaned_Train[,c("DK" )], ntree=300
                       ,type='regression')
    RFPred = predict( rf,  Batters2016Cleaned_Test[,c("Pitches","Home",
                                                      "Dec","DR","IP","H","R","ER","BB","SO","HR","HBP",
                                                      "ERA","BF","Pit","Str","StL","StS","GB","FB","LD","PU","Unk","GSc",
                                                      "IR","IS","SB","CS","PO","AB","X2B","X3B","IBB","GDP","SF","ROE","aLI",
                                                      "WPA","RE24","Entered","empty","OppTeamH",
                                                      "OppTeamHR","OppTeamR","OppTeam2B","OppTeam3B")] 
                      ,type = c("response") )
    rfP[1,j] = as.numeric(RFPred)
    
  }
  
  Prediction2 = as.data.frame(median(rfP[,1]))
  Prediction2["Name"] =  as.data.frame(Batters2016Cleaned_Test$Name)
  Prediction2["HTeam"] =  as.data.frame(Batters2016Cleaned_Test$Tm)
  Prediction2["Pos"] =  as.data.frame(Batters2016Cleaned_Test$Position)
  Prediction2["Actual"] =  as.data.frame(Batters2016Cleaned_Test$DK)
  ResultsPitchers = rbind(ResultsPitchers, Prediction2)
}



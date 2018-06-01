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
pitchingStats2017 = read.csv("pitchingStats2017.csv", header = TRUE)
colnames(B2017) = c('Name','Bats','','Position', 'Gcar','Gtm','Date','Tm','Home','Opp','Rslt','Inngs','PA','AB','R','H','2B','3B','HR','RBI','BB','IBB','SO',
                    'HBP','SH','SF','ROE','GDP','SB','CS','BA','OBP','SLG','OPS','BOP','aLI','WPA','RE24','DK','FD','Pos')

## Cleaning Batting 2017
B2017 = cleanData(B2017, 13, 38)
B2017$Home = as.numeric(levels(B2017$Home))[B2017$Home] 
B2017$Home[is.na(B2017$Home)] <- 0
B2017$DK = B2017$H*3 + B2017$`2B`*3 + B2017$`3B` * 8 + B2017$HR * 10 + B2017$R * 2 + B2017$BB * 2 + B2017$HBP * 2 + B2017$SB * 5

colnames(P2017) = c('Name','Pitches','','Position','Gcar','Gtm','Date','Tm','Home','Opp','Rslt','Inngs','Dec','DR','IP','H','R',
                    'ER','BB','SO','HR','HBP','ERA','BF','Pit','Str','StL','StS','GB','FB','LD','PU','Unk',
                    'GSc','IR','IS','SB','CS','PO','AB','2B','3B','IBB','GDP','SF','ROE','aLI','WPA','RE24','DFS(DK)','DFS(FD)','Entered','Exited','empty')
#Cleaning Pitching 2017
P2017 = cleanData(P2017, 13, 50)
P2017$Home = as.numeric(levels(P2017$Home))[P2017$Home] 
P2017$Home[is.na(P2017$Home)] <- 0

P2017$DK = P2017$IP*2.25 + P2017$SO * 2 + P2017$ER * -2 + P2017$H *  -.6 + P2017$BB * -.6 



B2017$Date =  as.Date(B2017$Date, "%b %d %Y")
tB2017 = B2017[1,]
tB2017$HRA = 0
tB2017$SBA = 0
tB2017$`2BA` = 0


CumulativeBattingStats2017 = tB2017[0,]
rm(tB2017)
PlayerList = unique(B2017$Name)

## Calculate Cumulative stats for each player in 2017/2018 season
for(i in 1:length(PlayerList)){
  
  subData = subset(B2017, B2017$Name == PlayerList[i])
  print(i)
  subData =  subData[ order(as.Date(subData$Date), decreasing = FALSE ), ]
  
  for(j in 1:nrow(subData)){
    if ( j < 3){
      NewRow = subData[j,]
      NewRow$`2BA` = 0
      NewRow$HRA = 0
      NewRow$SBA = 0
      
      CumulativeBattingStats2017 = rbind(CumulativeBattingStats2017, NewRow)    }
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
      
      CumulativeBattingStats2017 = rbind(CumulativeBattingStats2017, NewRow)
    }
    
  }
}


P2017$Date =  as.Date(P2017$Date, "%b %d %Y")


CumulativePitchingStats2017 = P2017[0,]
PlayerList = unique(P2017$Name)

## Calculate Cumulative stats for each player in 2017/2018 season
for(i in 1:length(PlayerList)){
  
  subData = subset(P2017, P2017$Name == PlayerList[i])
  print(i)
  subData =  subData[ order(as.Date(subData$Date), decreasing = FALSE ), ]
  
  for(j in 1:nrow(subData)){
    if ( j < 3){
      CumulativePitchingStats2017 = rbind(CumulativePitchingStats2017, subData[j,])
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
      
      CumulativePitchingStats2017 = rbind(CumulativePitchingStats2017, NewRow)
    }
    
  }
}

write.csv(CumulativeBattingStats2017, file = "CumBatting2017.csv")
write.csv(CumulativePitchingStats2017, file = "CumPitching2017.csv")
CumulativeBattingStats2017 = read.csv("CumBatting2017.csv", header = TRUE)
CumulativePitchingStats2017 = read.csv("CumPitching2017.csv", header = TRUE)




# testBatting = subset(CumulativeBattingStats, CumulativeBattingStats$Tm == "CLE")
testPitching = subset(CumulativePitchingStats2017, grepl("GS[0-9]", CumulativePitchingStats2017$Inngs))
names(testPitching)[names(testPitching) == 'Opp'] <- 'Opp2'
names(testPitching)[names(testPitching) == 'Tm'] <- 'Opp'


MergeData2017 = merge(CumulativeBattingStats2017, testPitching, by = c("Date","Opp"))
write.csv(MergeData2017, file = "MergeData2017.csv")
MergeData2017 = read.csv("MergeData2017.csv")

MergeData2017 = subset(MergeData2017,  !is.na(MergeData2017$Date))
names(MergeData2017)[names(MergeData2017) %nin% names(MergeData2017)]


################################## PITCHERS

# write.csv(MergeData2017, file = "MergeData2017.csv")

spearmanP = varclus(as.matrix(AllData[,c( "AB.x","R.x","H.x","2B.x","3B.x","HR.x","RBI","BB.x",
                                          "IBB.x","SO.x","HBP.x","SH","SF.x","ROE.x","GDP.x","SB.x","CS.x","BA",
                                          "OBP","SLG","BOP","aLI.x","WPA.x","RE24.x",
                                          "IP","H.y","ER","BB.y",
                                          "SO.y","HR.y","HBP.y","ERA","Str","StL","StS","GB","FB",
                                          "LD","PU","Unk","GSc","IR","IS","SB.y","CS.y","PO","2B.y",
                                          "3B.y","IBB.y","GDP.y","SF.y","ROE.y","aLI.y","WPA.y")], similarity = "spearman"))
plot(spearmanP)
abline(h=0.3)

####### Pitching stats
DateCheck = "2018-5-9"
testPitching = CumulativePitchingStats2017
PlayerList = testPitching$Name
testPitching$OppTeamH = 0
testPitching$OppTeamHR = 0
testPitching$OppTeamR =0
testPitching$OppTeam2B = 0
testPitching$OppTeam3B = 0
pitchingStats2017 = testPitching[0,]

for(each in 1:nrow(testPitching)){
  print(each)
  pitichingDate = as.Date(testPitching[each,]$Date)
  oppTeam =as.character(testPitching[each,]$Opp)
  OpposingTeam = subset(B2017, as.Date(B2017$Date) < pitichingDate & 
                          as.character(B2017$Tm) == oppTeam &
                          as.Date(B2017$Date) > (pitichingDate - 7))
  
  testPitching[each,]$OppTeamH = mean(OpposingTeam$H)
  testPitching[each,]$OppTeamHR = mean(OpposingTeam$HR)
  testPitching[each,]$OppTeamR = mean(OpposingTeam$R)
  testPitching[each,]$OppTeam2B = mean(OpposingTeam$`2B`)
  testPitching[each,]$OppTeam3B = mean(OpposingTeam$`3B`)
  
  pitchingStats2017 = rbind(pitchingStats2017, testPitching[each,])
  
  
}
write.csv(pitchingStats2017, file = "pitchingStats2017.csv")



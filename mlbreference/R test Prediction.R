playerName = "Matt Carpenter"  
Batters2016Cleaned_Test = subset(AllDataCleaned, as.Date(AllDataCleaned$Date) == DateCheck
                                   & AllDataCleaned$Name.x == playerName) 
  
  # 
  # Batters2016Cleaned_Test = subset(Todayz, as.Date(Todayz$Date) == DateCheck
  #                                  & Todayz$Name.x == as.character(playerNames[each]) )
  
  Batters2016Cleaned_Train = subset(AllDataCleaned, as.Date(AllDataCleaned$Date) < DateCheck
                                    & AllDataCleaned$Name.x == playerName) 
  
  
  Batters2016Cleaned_Train[is.na(Batters2016Cleaned_Train)] = 0
  Batters2016Cleaned_Test[is.na(Batters2016Cleaned_Test)] = 0
  
  rf = randomForest( Batters2016Cleaned_Train[,c("AB.x","R.x","2B.x","3B.x","HR.x","RBI","BB.x","Bats","Pitches",
                                                 "IBB.x","SO.x","HBP.x","SH","SF.x","ROE.x","GDP.x","SB.x","CS.x","BA",
                                                 "OBP","SLG","BOP","aLI.x","WPA.x","RE24.x",
                                                 "IP","H.y","ER","BB.y",
                                                 "SO.y","HR.y","HBP.y","ERA","Str","StL","StS","GB","FB",
                                                 "LD","PU","Unk","GSc","IR","IS","SB.y","CS.y","PO","2B.y",
                                                 "3B.y","IBB.y","GDP.y","SF.y","ROE.y","aLI.y","WPA.y")], 
                     y = Batters2016Cleaned_Train[,c("DK.x" )], ntree=500
                     ,type='regression')
  RFPred = predict( rf,  Batters2016Cleaned_Test[,c("AB.x","R.x","2B.x","3B.x","HR.x","RBI","BB.x","Bats","Pitches",
                                                    "IBB.x","SO.x","HBP.x","SH","SF.x","ROE.x","GDP.x","SB.x","CS.x","BA",
                                                    "OBP","SLG","BOP","aLI.x","WPA.x","RE24.x",
                                                    "IP","H.y","ER","BB.y",
                                                    "SO.y","HR.y","HBP.y","ERA","Str","StL","StS","GB","FB",
                                                    "LD","PU","Unk","GSc","IR","IS","SB.y","CS.y","PO","2B.y",
                                                    "3B.y","IBB.y","GDP.y","SF.y","ROE.y","aLI.y","WPA.y")] 
                    ,type = c("response") )
  
  RFPred
  Batters2016Cleaned_Test$DK.x
  varImpPlot(rf)
  
 correlation = cor(Batters2016Cleaned_Train[,c("AB.x","R.x","2B.x","3B.x","HR.x","RBI","BB.x",
                                "IBB.x","SO.x","HBP.x","SH","SF.x","ROE.x","GDP.x","SB.x","CS.x","BA",
                                "OBP","SLG","BOP","aLI.x","WPA.x","RE24.x",
                                "IP","H.y","ER","BB.y","DK.x",
                                "SO.y","HR.y","HBP.y","ERA","Str","StL","StS","GB","FB",
                                "LD","PU","Unk","GSc","IR","IS","SB.y","CS.y","PO","2B.y",
                                "3B.y","IBB.y","GDP.y","SF.y","ROE.y","aLI.y","WPA.y")])

 metricToTest = "ERA"
 sds = sd(Batters2016Cleaned_Train[,c(metricToTest)])
 
 testD  = subset(Batters2016Cleaned_Train,
                 Batters2016Cleaned_Train[,c(metricToTest)] > (Batters2016Cleaned_Test[,c(metricToTest)] - sds) 
        & Batters2016Cleaned_Train[,c(metricToTest)] < (Batters2016Cleaned_Test[,c(metricToTest)] + sds)
        )
 
 
 metricToTest = "2B.x"
 sds = sd(testD[,c(metricToTest)])
 
 testD  = subset(testD,
                 testD[,c(metricToTest)] > (Batters2016Cleaned_Test[,c(metricToTest)] - sds) 
                 & testD[,c(metricToTest)] < (Batters2016Cleaned_Test[,c(metricToTest)] + sds)
 )
 
 
 metricToTest = "IS"
 sds = sd(testD[,c(metricToTest)])
 
 testD  = subset(testD,
                 testD[,c(metricToTest)] > (Batters2016Cleaned_Test[,c(metricToTest)] - sds) 
                 & testD[,c(metricToTest)] < (Batters2016Cleaned_Test[,c(metricToTest)] + sds)
 )
  
 metricToTest = "H.y"
 sds = sd(testD[,c(metricToTest)])
 
 testD  = subset(testD,
                 testD[,c(metricToTest)] > (Batters2016Cleaned_Test[,c(metricToTest)] - sds) 
                 & testD[,c(metricToTest)] < (Batters2016Cleaned_Test[,c(metricToTest)] + sds)
 )
 
 
 summary(testD$DK.x)
  rm(testD)
 
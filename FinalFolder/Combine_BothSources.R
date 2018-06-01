MlbRefData = read.csv("AllData.csv")
FBData = read.csv("Batters2016Cleaned.csv")
names(FBData)[names(FBData) == 'Player.x'] <- 'Name.x'


MergedData = merge(x = FBData, y= MlbRefData, by = c("Date","Name.x"), all.x = TRUE)





spearmanP = varclus(as.matrix(MergedData[,c( "AB.x","Rating.x","OppBP","ParkF.x","Temp.x","WindSpd.x",
                                          "Order","X2B.x","X3B.x","HR.x","RBI","BB.x",
                                          "IBB.x","SO.x","HBP.x","SH","SF.x","ROE.x","GDP.x","SB.x","CS.x","BA",
                                          "OBP","SLG","BOP","aLI.x","WPA.x","RE24.x",
                                          "IP.y","H.y","ER","BB.y","Act.Pts",
                                          "SO.y","HR.y","HBP.y","ERA","Str","StL","StS","GB","FB.y",
                                          "LD","PU","IR","IS","SB.y","CS.y","PO","X2B.y",
                                          "X3B.y","IBB.y","GDP.y","SF.y","ROE.y","aLI.y","WPA.y",
                                          "C1.x","C2.x","C3.x","C4.x","C5.x","C6.x","C7.x","C8.x","C9.x",
                                          "C6.y","C7.y")], similarity = "spearman"))
plot(spearmanP)
abline(h=0.3)
####################################




DateCheck = "2018-4-17"
# AllData =  subset(AllData, !grepl('[0-9]', AllData$Name.y))
# AllData =  subset(AllData, !grepl('[0-9]', AllData$Name.x))
# names(AllData)[names(AllData) == 'X2B.x'] <- '2B.x'
# names(AllData)[names(AllData) == 'X3B.x'] <- '3B.x'
# names(AllData)[names(AllData) == 'X2B.y'] <- '2B.y'
# names(AllData)[names(AllData) == 'X3B.y'] <- '3B.y'
MergedData[is.na(MergedData)] = 0
AllDataCleaned = MergedData[,c("Date","Name.x","Rating.x","Pos.x","Team","Salary.x","Act.Pts",
                               "Pos.x","AB.x","OppBP","ParkF.x","Temp.x","WindSpd.x",
                               "Order","R.x","H.x","X2B.x","X3B.x","HR.x","RBI","BB.x",
                               "IBB.x","SO.x","HBP.x","SH","SF.x","ROE.x","GDP.x","SB.x","CS.x","BA",
                               "OBP","SLG","BOP","aLI.x","WPA.x","RE24.x","DK.x" ,
                               "IP.y","H.y","ER","BB.y","Name.y",
                               "SO.y","HR.y","HBP.y","ERA","Str","StL","StS","GB","FB.y",
                               "LD","PU","IR","IS","SB.y","CS.y","PO","X2B.y",
                               "X3B.y","IBB.y","GDP.y","SF.y","ROE.y","aLI.y","WPA.y",
                               "C1.x","C2.x","C3.x","C4.x","C5.x","C6.x","C7.x","C8.x","C9.x",
                               "C1.y","C2.y","C3.y","C4.y","C5.y","C6.y","C7.y","C8.y","C9.y")]


AllDataCleaned = unique(AllDataCleaned)
# names(AllDataCleaned)[names(AllDataCleaned) == 'Player.x'] <- 'Player'
# AllDataCleaned$Date = as.Date(AllDataCleaned$Date, "%m/%d/%Y")
AllDataCleaned$Date = as.Date(AllDataCleaned$Date, "%Y-%m-%d")
Batters2016Cleaned_Test = subset(AllDataCleaned, (as.Date(AllDataCleaned$Date) == as.Date(DateCheck)))
# Batters2016Cleaned_Test = Today
# Todayz = Today
Batters2016Cleaned_Train = subset(AllDataCleaned, as.Date(AllDataCleaned$Date) < as.Date(DateCheck))


playerNames = (Batters2016Cleaned_Test$Name.x)


ResultsBatters = data.frame( RFPred = numeric(), Name = factor(), HTeam = factor(), Pos = factor(), Actual = numeric())

for (each in 1:length(playerNames)){
  Batters2016Cleaned_Test = subset(AllDataCleaned, as.Date(AllDataCleaned$Date) == DateCheck
                                   & AllDataCleaned$Name.x == as.character(playerNames[each]) )
  
  
  # Batters2016Cleaned_Test = subset(Todayz, as.Date(Todayz$Date) == DateCheck
  #                                  & Todayz$Name.x == as.character(playerNames[each]) )
  
  Batters2016Cleaned_Train = subset(AllDataCleaned, as.Date(AllDataCleaned$Date) < DateCheck
                                    & AllDataCleaned$Name.x == as.character(playerNames[each]) )
  
  
  print (playerNames[each])
  print (each)
  ### This ensures atleast 1 row of data exists for prediction
  if (nrow(Batters2016Cleaned_Test) < 1 ){
    next
  }
  
  #### If less than 15 rows then use that Teams's data
  # if (nrow(Batters2016Cleaned_Train) < 10){
  #   Batters2016Cleaned_Train = subset(AllDataCleaned, as.Date(AllDataCleaned$Date) < DateCheck
  #                                     & AllDataCleaned$Team 
  #                                     == as.character( unique ( subset(AllDataCleaned, 
  #                                                                      AllDataCleaned$Name.x == as.character(playerNames[each]) )$Tm ) )
  #                                     
  #   )
  #   
  # }
  # 
  #   if ((Batters2016Cleaned_Test$Rating) < 45){
  #     next
  #   }
  
  if (nrow(Batters2016Cleaned_Train) < 15){
    next
  }
  Batters2016Cleaned_Train[is.na(Batters2016Cleaned_Train)] = 0
  Batters2016Cleaned_Test[is.na(Batters2016Cleaned_Test)] = 0
  
  rf = randomForest( Batters2016Cleaned_Train[,c("Pos.x","AB.x","Rating.x","OppBP","ParkF.x","Temp.x","WindSpd.x",
                                                 "Order","X2B.x","X3B.x","HR.x","RBI","BB.x",
                                                 "IBB.x","SO.x","HBP.x","SH","SF.x","ROE.x","GDP.x","SB.x","CS.x","BA",
                                                 "OBP","SLG","BOP","aLI.x","WPA.x","RE24.x",
                                                 "IP.y","H.y","ER","BB.y",
                                                 "SO.y","HR.y","HBP.y","ERA","Str","StL","StS","GB","FB.y",
                                                 "LD","PU","IR","IS","SB.y","CS.y","PO","X2B.y",
                                                 "X3B.y","IBB.y","GDP.y","SF.y","ROE.y","aLI.y","WPA.y",
                                                 "C1.x","C2.x","C3.x","C4.x","C5.x","C6.x","C7.x","C8.x","C9.x",
                                                 "C6.y","C7.y")], 
                     y = Batters2016Cleaned_Train[,c("Act.Pts" )], ntree=300
                     ,type='regression')
  RFPred = predict( rf,  Batters2016Cleaned_Test[,c("Pos.x","AB.x","Rating.x","OppBP","ParkF.x","Temp.x","WindSpd.x",
                                                    "Order","X2B.x","X3B.x","HR.x","RBI","BB.x",
                                                    "IBB.x","SO.x","HBP.x","SH","SF.x","ROE.x","GDP.x","SB.x","CS.x","BA",
                                                    "OBP","SLG","BOP","aLI.x","WPA.x","RE24.x",
                                                    "IP.y","H.y","ER","BB.y",
                                                    "SO.y","HR.y","HBP.y","ERA","Str","StL","StS","GB","FB.y",
                                                    "LD","PU","IR","IS","SB.y","CS.y","PO","X2B.y",
                                                    "X3B.y","IBB.y","GDP.y","SF.y","ROE.y","aLI.y","WPA.y",
                                                    "C1.x","C2.x","C3.x","C4.x","C5.x","C6.x","C7.x","C8.x","C9.x",
                                                    "C6.y","C7.y")] 
                    ,type = c("response") )
  Prediction2 =  as.data.frame(RFPred)
  
  Prediction2["Name"] =  as.data.frame(Batters2016Cleaned_Test$Name.x)
  Prediction2["HTeam"] =  as.data.frame(Batters2016Cleaned_Test$Team)
  Prediction2["Pos"] =  as.data.frame(Batters2016Cleaned_Test$Pos.x)
  Prediction2["Actual"] =  as.data.frame(Batters2016Cleaned_Test$Act.Pts)
  ResultsBatters = rbind(ResultsBatters, Prediction2)
}

write.csv(ResultsBatters, file = "4-17.csv")

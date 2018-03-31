############ Load Library
install.packages("corrplot")
install.packages("brnn")
install.packages("h2o")
install.packages("randomForest")
install.packages("Matrix")
install.packages("xgboost")
install.packages("stringdist")
install.packages("varhandle")
install.packages("mxnet")

library(Hmisc)
library(corrplot)
library(brnn)
library(h2o)
library(randomForest)
library(Matrix)
library(xgboost)
library(stringdist)
library(varhandle)

require(devtools)
install_version("DiagrammeR", version = "0.9.0", repos = "http://cran.us.r-project.org")
require(DiagrammeR)

library(mxnet)
###########################################################
localH2O <- h2o.init()

Batters2016 = read.csv('Batters_16-17.csv')
Pitchers2016 = read.csv('Pitchers_16-17.csv')

Batters2018 = read.csv('Batters_17-18.csv')


Batters2016 = rbind(Batters2018, Batters2016)

# Data Prep
Batters2016[Batters2016 == 'nbsp;'] = 0
Pitchers2016[Pitchers2016 == 'nbsp;'] = 0
Batters2016[Batters2016 == "-"] = 0
Pitchers2016[Pitchers2016 == "-"] = 0
Batters2016$Date =  as.Date(Batters2016$Date, "%m/%d/%Y")
Pitchers2016$Date =  as.Date(Pitchers2016$Date, "%m/%d/%Y")


Batters2016[is.na(Batters2016)] = 0
Pitchers2016[is.na(Pitchers2016)] = 0
Batters2016$FB = unfactor(Batters2016$FB)
Batters2016$GB = unfactor(Batters2016$GB)
Batters2016$LD = unfactor(Batters2016$LD)
Batters2016$HH = unfactor(Batters2016$HH)


Pitchers2016$FB = unfactor(Pitchers2016$FB)
Pitchers2016$GB = unfactor(Pitchers2016$GB)
Pitchers2016$LD = unfactor(Pitchers2016$LD)
Pitchers2016$HH = unfactor(Pitchers2016$HH)


Batters2016[is.na(Batters2016)] = 0
Pitchers2016[is.na(Pitchers2016)] = 0


#'
#' All Column names
("Date","Like","Lock","Rating","Player","Salary",
  "Pos","Order","Team","Opp","Min","Max",
  "Proj","Ceiling","Floor","ProjplusMinus","Pts_Sal","LowProjOwn",
  "HighProjOwn,","Imp.Pts","Act.Pts","wOBA","wOBADiff","ISO",
  "ISODiff","SLG","SO_AB","HR_AB","SB_G","OppBP",
  "Pro","My","Ump","Bargain","ParkF","Runs",
  "OppRuns","ST","ML","O_U","MLPer","Rating_M",
  "Temp","WindSpd","WindDir","Humidity","Precip","Cnt",
  "Dist","EV","FB","GB","LD","HH",
  "DistST","EVST","HHST","Rec.BBL","Air","CntPer",
  "DistPer","EVM","FBM","GBM","LDM","HH.",
  "GB_FB","AirPer","OppwOBA","OppISO","OppwOBAMonth","OppISOMonth",
  "PPG","Change","plusMinus","Consistency","Upside","Duds",
  "Count","PPGYear","ChangeYear","ConsistencyYear","UpsideYear","DudsYear",
  "CountYear","ACountYear")

#Pitchers
("Date","Like","Lock","Rating","Player","Salary","Team",
  "Opp","Min","Max","Proj","Ceiling","Floor","Proj_plusMinus",
  "Pts_Sal","LowProjOwn","HighProjOwn","ImpPts","ActPts","WHIP","HR_9",
  "SO_9","IP","QualS_S","SO_AB","wOBA","Pro","My",
  "Ump","Bargain","KPred","ParkF","Runs","OppRuns","delta",
  "ML","O_U","MLYear","RatingYear","Temp","WindSpd","WindDir",
  "Humidity","Precip","Cnt","Dist","EV","FB","GB",
  "LD","HH","Speed","S","Dist_delta","EV_delta","PCnt",
  "HH_delta","Spd_delta","RecBBL","Air","CntYear","DistYear","EVYear",
  "FBYear","GBYear","LDYear","HHYear","GB_FB","SpeedYear","SYear",
  "PCntYear","AirYear","PPG","Change","plusMinusYear","Consistency","Upside",
  "Duds","Count","PPGYear","ChangeYear","ConsistencyYear","UpsideYear","DudsYear",
  "CountYear")
#'
## varclus Batters
spearmanP = varclus(as.matrix(Batters2016[,c("Rating","Salary",
                                             "ProjplusMinus","Pts_Sal","LowProjOwn",
                                             "Imp.Pts","Act.Pts","wOBA","wOBADiff","ISO",
                                             "ISODiff","SO_AB","HR_AB","SB_G","OppBP",
                                             "Pro","Bargain","ParkF","Runs",
                                             "OppRuns","ST","ML","O_U","MLPer","Rating_M",
                                             "Temp","WindSpd","Humidity","Precip","Cnt",
                                             "Dist","EV",
                                             "DistST","EVST","HHST","Rec.BBL","Air",
                                             "DistPer","EVM","AirPer","OppwOBA","OppISO","OppwOBAMonth","OppISOMonth",
                                             "PPG","Change","plusMinus",
                                             "Count","PPGYear","ChangeYear",
                                             "CountYear")] ), similarity = "spearman")
plot(spearmanP)
abline(h=0.3)


## Varclus Pitchers
spearmanP = varclus(as.matrix(Pitchers2016[,c("Rating","Salary",
                                              "Proj","Ceiling","Floor","Proj_plusMinus",
                                              "Pts_Sal","LowProjOwn","HighProjOwn","ImpPts","ActPts","WHIP","HR_9",
                                              "SO_9","IP","QualS_S","SO_AB","wOBA","Pro",
                                              "Bargain","KPred","ParkF","Runs","OppRuns","delta",
                                              "ML","O_U","MLYear","RatingYear","Temp","WindSpd",
                                              "Humidity","Precip","Cnt","Dist","EV","FB","GB",
                                              "LD","HH","Speed","S","Dist_delta","EV_delta","PCnt",
                                              "HH_delta","Spd_delta","RecBBL","Air","CntYear","DistYear","EVYear",
                                              "SpeedYear","SYear",
                                              "PCntYear","AirYear","PPG","Change","plusMinusYear","Count",
                                              "PPGYear","ChangeYear","CountYear")] ), similarity = "spearman")
plot(spearmanP)
abline(h=0.3)


## Perdiction
DateCheck = "2018-03-30"
Batters2016Cleaned = Batters2016[,c("Date","Rating","Player","Salary",
                                    "Pos","Order","Team","Opp","Rating","Salary",
                              "ProjplusMinus","Pts_Sal","LowProjOwn",
                              "Imp.Pts","Act.Pts","wOBA","wOBADiff","ISO",
                              "ISODiff","SO_AB","HR_AB","SB_G","OppBP",
                              "Pro","Bargain","ParkF","Runs",
                              "OppRuns","ST","ML","O_U","MLPer","Rating_M",
                              "Temp","WindSpd","Humidity","Precip","Cnt",
                              "Dist","EV","FB","GB","LD","HH",
                              "DistST","EVST","HHST","Rec.BBL","Air",
                              "DistPer","EVM","AirPer","OppwOBA","OppISO","OppwOBAMonth","OppISOMonth",
                              "PPG","Change","plusMinus",
                              "Count","PPGYear","ChangeYear",
                              "CountYear")]
Batters2016Cleaned = unique(Batters2016Cleaned)
Batters2016Cleaned_Test = subset(Batters2016Cleaned, (as.Date(Batters2016Cleaned$Date) == as.Date(DateCheck)))
Batters2016Cleaned_Train = subset(Batters2016Cleaned, as.Date(Batters2016Cleaned$Date) < as.Date(DateCheck))


playerNames = unique(Batters2016Cleaned_Test$Player)


ResultsBatters = data.frame( RFPred = numeric(), Xgb = numeric(), Name = factor(), Pos = factor() ,
                      Salary = numeric(), Actual = numeric() , HTeam = factor(), OTeam = factor(),
                      Pts = numeric(), DNNPer = numeric(), DNN = numeric(),xgbPLUSMINUS = numeric(),
                      RFPLUSMINUS = numeric())

## Prediction
for (each in 1:length(playerNames)){
  Batters2016Cleaned_Test = subset(Batters2016Cleaned, Batters2016Cleaned$Date == DateCheck 
                             & Batters2016Cleaned$Player == as.character(playerNames[each]) )
  
  Batters2016Cleaned_Train = subset(Batters2016Cleaned, Batters2016Cleaned$Date != DateCheck
                              & Batters2016Cleaned$Player == as.character(playerNames[each]) )
  Batters2016Cleaned_Train = subset(Batters2016Cleaned_Train, as.Date(Batters2016Cleaned_Train$Date) > "2017-07-05")
  print (playerNames[each])
  print (each)
  ### This ensures atleast 1 row of data exists for prediction
  if (nrow(Batters2016Cleaned_Test) < 1 ){
    next
  }
  
  #### If less than 15 rows then use that Teams's data
  if (nrow(Batters2016Cleaned_Train) < 20){
    Batters2016Cleaned_Train = subset(Batters2016Cleaned, Batters2016Cleaned$Date != DateCheck
                                & Batters2016Cleaned$Team
                                == as.character( unique ( subset(Batters2016Cleaned, Batters2016Cleaned$Player == as.character(playerNames[each]) )$Team ) )
                                
    )
    
  }
  # 
  #   if ((Batters2016Cleaned_Test$Rating) < 45){
  #     next
  #   }
  
  if (nrow(Batters2016Cleaned_Train) < 15){
    next
  }
  
  ######### Construct Models
  indices = sample(1:nrow(Batters2016Cleaned_Train), 7, replace = FALSE)
  Batters2016Cleaned_Train_PlusMinusTrain = Batters2016Cleaned_Train[-indices, ]
  Batters2016Cleaned_Train_PlusMinusTest = Batters2016Cleaned_Train[indices, ]
  
  
  #########RF
  rf = randomForest( Batters2016Cleaned_Train[,c("Rating","Salary","Order","FB","GB","LD","HH",
                                                 "ProjplusMinus","Pts_Sal","LowProjOwn",
                                                 "Imp.Pts","wOBA","wOBADiff","ISO",
                                                 "ISODiff","SO_AB","HR_AB","SB_G","OppBP",
                                                 "Pro","Bargain","ParkF","Runs",
                                                 "OppRuns","ST","ML","O_U","MLPer","Rating_M",
                                                 "Temp","WindSpd","Humidity","Precip","Cnt",
                                                 "Dist","EV",
                                                 "DistST","EVST","HHST","Rec.BBL","Air",
                                                 "DistPer","EVM","AirPer","OppwOBA","OppISO","OppwOBAMonth","OppISOMonth",
                                                 "PPG","Change","plusMinus",
                                                 "Count","PPGYear","ChangeYear",
                                                 "CountYear")], 
                     y = Batters2016Cleaned_Train[,c("Act.Pts")], ntree=300
                     ,type='regression')
  
  rf_PlusMinus =  randomForest( Batters2016Cleaned_Train_PlusMinusTrain[,c("Rating","Salary","Order","FB","GB","LD","HH",
                                                                           "ProjplusMinus","Pts_Sal","LowProjOwn",
                                                                           "Imp.Pts","wOBA","wOBADiff","ISO",
                                                                           "ISODiff","SO_AB","HR_AB","SB_G","OppBP",
                                                                           "Pro","Bargain","ParkF","Runs",
                                                                           "OppRuns","ST","ML","O_U","MLPer","Rating_M",
                                                                           "Temp","WindSpd","Humidity","Precip","Cnt",
                                                                           "Dist","EV",
                                                                           "DistST","EVST","HHST","Rec.BBL","Air",
                                                                           "DistPer","EVM","AirPer","OppwOBA","OppISO","OppwOBAMonth","OppISOMonth",
                                                                           "PPG","Change","plusMinus",
                                                                           "Count","PPGYear","ChangeYear",
                                                                           "CountYear" )], 
                                y = Batters2016Cleaned_Train_PlusMinusTrain[,c("Act.Pts")], ntree=300
                                ,type='regression')
  
  ####XGB
  # trainSparceMatrix = sparse.model.matrix( Batters2016Cleaned_Train$`Act.Pts` ~ 
  #                                            ( Batters2016Cleaned_Train$Rating + Batters2016Cleaned_Train$`Usg.Proj` + Batters2016Cleaned_Train$Pts_Sal 
  #                                              + Batters2016Cleaned_Train$`Min.Proj` + Batters2016Cleaned_Train$Pro + Batters2016Cleaned_Train$Bargain
  #                                              + Batters2016Cleaned_Train$Own1 + Batters2016Cleaned_Train$PER + Batters2016Cleaned_Train$Pts + Batters2016Cleaned_Train$Usage +
  #                                                Batters2016Cleaned_Train$Opp_Plus_Minus + Batters2016Cleaned_Train$PaceD + Batters2016Cleaned_Train$TS_Per + Batters2016Cleaned_Train$Fouls_36 +
  #                                                Batters2016Cleaned_Train$Points_Touch + Batters2016Cleaned_Train$Touches + Batters2016Cleaned_Train$Rest + Batters2016Cleaned_Train$Pts +
  #                                                Batters2016Cleaned_Train$`Opp.Pts` + Batters2016Cleaned_Train$delta + Batters2016Cleaned_Train$Spread + Batters2016Cleaned_Train$O_U +
  #                                                Batters2016Cleaned_Train$Spread_per + Batters2016Cleaned_Train$Upside + Batters2016Cleaned_Train$Duds + Batters2016Cleaned_Train$Count +
  #                                                Batters2016Cleaned_Train$YPlus_Minus + Batters2016Cleaned_Train$YDuds + Batters2016Cleaned_Train$YCount+    Batters2016Cleaned_Train$PTS +
  #                                                Batters2016Cleaned_Train$REB + Batters2016Cleaned_Train$STL + Batters2016Cleaned_Train$BLK ))
  # 
  # Labels = Matrix(Batters2016Cleaned_Train$`Act.Pts`, sparse = TRUE)
  # 
  # dtrain <- xgb.DMatrix(data = trainSparceMatrix, label=Labels)
  # 
  # trainSparceMatrix_PlusMinus = sparse.model.matrix(     Batters2016Cleaned_Train_PlusMinusTrain$`Act.Pts` ~ 
  #                                                          (Batters2016Cleaned_Train_PlusMinusTrain$Rating +  Batters2016Cleaned_Train_PlusMinusTrain$`Usg.Proj` + Batters2016Cleaned_Train_PlusMinusTrain$Pts_Sal 
  #                                                           + Batters2016Cleaned_Train_PlusMinusTrain$`Min.Proj` + Batters2016Cleaned_Train_PlusMinusTrain$Pro + Batters2016Cleaned_Train_PlusMinusTrain$Bargain
  #                                                           + Batters2016Cleaned_Train_PlusMinusTrain$Own1 + Batters2016Cleaned_Train_PlusMinusTrain$PER + Batters2016Cleaned_Train_PlusMinusTrain$Pts + Batters2016Cleaned_Train_PlusMinusTrain$Usage +
  #                                                             Batters2016Cleaned_Train_PlusMinusTrain$Opp_Plus_Minus + Batters2016Cleaned_Train_PlusMinusTrain$PaceD + Batters2016Cleaned_Train_PlusMinusTrain$TS_Per + Batters2016Cleaned_Train_PlusMinusTrain$Fouls_36 +
  #                                                             Batters2016Cleaned_Train_PlusMinusTrain$Points_Touch + Batters2016Cleaned_Train_PlusMinusTrain$Touches + Batters2016Cleaned_Train_PlusMinusTrain$Rest + Batters2016Cleaned_Train_PlusMinusTrain$Pts +
  #                                                             Batters2016Cleaned_Train_PlusMinusTrain$`Opp.Pts` + Batters2016Cleaned_Train_PlusMinusTrain$delta + Batters2016Cleaned_Train_PlusMinusTrain$Spread + Batters2016Cleaned_Train_PlusMinusTrain$O_U +
  #                                                             Batters2016Cleaned_Train_PlusMinusTrain$Spread_per + Batters2016Cleaned_Train_PlusMinusTrain$Upside + Batters2016Cleaned_Train_PlusMinusTrain$Duds + Batters2016Cleaned_Train_PlusMinusTrain$Count +
  #                                                             Batters2016Cleaned_Train_PlusMinusTrain$YPlus_Minus + Batters2016Cleaned_Train_PlusMinusTrain$YDuds + Batters2016Cleaned_Train_PlusMinusTrain$YCount+    Batters2016Cleaned_Train_PlusMinusTrain$PTS +
  #                                                             Batters2016Cleaned_Train_PlusMinusTrain$REB + Batters2016Cleaned_Train_PlusMinusTrain$STL + Batters2016Cleaned_Train_PlusMinusTrain$BLK ))
  # Labels_PlusMinus = Matrix(Batters2016Cleaned_Train_PlusMinusTrain$`Act.Pts`, sparse = TRUE)
  # 
  # dtrain_PlusMinus <- xgb.DMatrix(data = trainSparceMatrix_PlusMinus, label=Labels_PlusMinus)
  
  ###H20
  
  # TrainingH20= as.h2o(Batters2016Cleaned_Train)
  # splits <- h2o.splitFrame(TrainingH20, c(0.9),  seed=1234)
  # 
  # 
  # trainDNN  <- h2o.assign(splits[[1]], "train.hex") # 60%
  # validDNN   <- h2o.assign(splits[[2]], "valid.hex") # 60%
  # TrainingH202= as.h2o(Batters2016Cleaned_Test)
  # splits2 <- h2o.splitFrame(TrainingH202,  seed=0)
  # testDNN <- h2o.assign(splits2[[1]], "test.hex")  # 20%
  # 
  # response <- "Act.Pts"
  # 
  # predictors <- c("Rating", "Salary","Proj_Plus_Minus","Pts_Sal","Usg.Proj","Min.Proj",
  #                 "Own1","Own2","Imp.Pts","FP_Min","PER",
  #                 "Usage","Pro","Bargain","Opp_Plus_Minus","PaceD","TS_Per","Fouls_36",
  #                 "Points_Touch","Touches","Rest","Pts","Opp.Pts","delta","Spread",
  #                 "O_U","Spread_per","PPG","Consistency",
  #                 "Upside","Duds","Count","YPPG","YPlus_Minus","YConsistency","YUpside","YDuds","YCount","PTS","REB","STL","BLK" )
  # 
  # m1 = tryCatch(
  #   {expr = h2o.deeplearning(
  #     model_id="dl_model_first",
  #     training_frame=trainDNN,
  #     validation_frame=validDNN,   ## validation dataset: used for scoring and early stopping
  #     x=predictors,
  #     y=response,
  #     nfold = 5,
  #     #activation="Rectifier",  ## default
  #     hidden=c(300,100),       ## default: 2 hidden layers with 200 neurons each
  #     variable_importances=T,
  #     epochs = 5,
  #     categorical_encoding = "OneHotInternal"
  #   )},
  #   error = function(i){return (0)}
  #   
  # )
  # 
  #################################Predictions
  
  RFPred = predict( rf,  Batters2016Cleaned_Test[,c("Rating","Salary","Order","FB","GB","LD","HH",
                                                    "ProjplusMinus","Pts_Sal","LowProjOwn",
                                                    "Imp.Pts","wOBA","wOBADiff","ISO",
                                                    "ISODiff","SO_AB","HR_AB","SB_G","OppBP",
                                                    "Pro","Bargain","ParkF","Runs",
                                                    "OppRuns","ST","ML","O_U","MLPer","Rating_M",
                                                    "Temp","WindSpd","Humidity","Precip","Cnt",
                                                    "Dist","EV",
                                                    "DistST","EVST","HHST","Rec.BBL","Air",
                                                    "DistPer","EVM","AirPer","OppwOBA","OppISO","OppwOBAMonth","OppISOMonth",
                                                    "PPG","Change","plusMinus",
                                                    "Count","PPGYear","ChangeYear",
                                                    "CountYear"  )] 
                    ,type = c("response") )
  
  
  RFPred_PlusMinus = predict( rf_PlusMinus,  Batters2016Cleaned_Train_PlusMinusTest[,c("Rating","Salary","Order","FB","GB","LD","HH",
                                                                                       "ProjplusMinus","Pts_Sal","LowProjOwn",
                                                                                       "Imp.Pts","wOBA","wOBADiff","ISO",
                                                                                       "ISODiff","SO_AB","HR_AB","SB_G","OppBP",
                                                                                       "Pro","Bargain","ParkF","Runs",
                                                                                       "OppRuns","ST","ML","O_U","MLPer","Rating_M",
                                                                                       "Temp","WindSpd","Humidity","Precip","Cnt",
                                                                                       "Dist","EV",
                                                                                       "DistST","EVST","HHST","Rec.BBL","Air",
                                                                                       "DistPer","EVM","AirPer","OppwOBA","OppISO","OppwOBAMonth","OppISOMonth",
                                                                                       "PPG","Change","plusMinus",
                                                                                       "Count","PPGYear","ChangeYear",
                                                                                       "CountYear"  )] 
                              ,type = c("response") )
  plusMinus = Batters2016Cleaned_Train_PlusMinusTest$`Act.Pts` - RFPred_PlusMinus
  RFPLUSMINUS_M = ceil(min(plusMinus))
  RFPLUSMINUS_P = ceil(max(plusMinus))
  # 
  # testSparseMatrix = sparse.model.matrix( 
  #   Batters2016Cleaned_Test$`Act.Pts` ~ 
  #     (Batters2016Cleaned_Test$Rating + Batters2016Cleaned_Test$`Usg.Proj` + Batters2016Cleaned_Test$Pts_Sal 
  #      + Batters2016Cleaned_Test$`Min.Proj` + Batters2016Cleaned_Test$Pro + Batters2016Cleaned_Test$Bargain
  #      + Batters2016Cleaned_Test$Own1 + Batters2016Cleaned_Test$PER + Batters2016Cleaned_Test$Pts + Batters2016Cleaned_Test$Usage +
  #        Batters2016Cleaned_Test$Opp_Plus_Minus + Batters2016Cleaned_Test$PaceD + Batters2016Cleaned_Test$TS_Per + Batters2016Cleaned_Test$Fouls_36 +
  #        Batters2016Cleaned_Test$Points_Touch + Batters2016Cleaned_Test$Touches + Batters2016Cleaned_Test$Rest + Batters2016Cleaned_Test$Pts +
  #        Batters2016Cleaned_Test$`Opp.Pts` + Batters2016Cleaned_Test$delta + Batters2016Cleaned_Test$Spread + Batters2016Cleaned_Test$O_U +
  #        Batters2016Cleaned_Test$Spread_per + Batters2016Cleaned_Test$Upside + Batters2016Cleaned_Test$Duds + Batters2016Cleaned_Test$Count +
  #        Batters2016Cleaned_Test$YPlus_Minus + Batters2016Cleaned_Test$YDuds + Batters2016Cleaned_Test$YCount+    
  #        Batters2016Cleaned_Test$REB + Batters2016Cleaned_Test$STL + Batters2016Cleaned_Test$BLK
  #     )) 
  # 
  # xgbO = xgboost(data = dtrain ,booster = "gblinear" , eta = 0.1 , max_depth=50, nthread = 4,
  #                nrounds=2000,objective = "reg:linear" , verbose = 0 )
  # 
  # predict(xgbO,testSparseMatrix )
  # 
  # 
  # testSparseMatrix_PlusMinus = sparse.model.matrix( 
  #   Batters2016Cleaned_Train_PlusMinusTest$`Act.Pts` ~ 
  #     (Batters2016Cleaned_Train_PlusMinusTest$Rating + Batters2016Cleaned_Train_PlusMinusTest$`Usg.Proj` + Batters2016Cleaned_Train_PlusMinusTest$Pts_Sal 
  #      + Batters2016Cleaned_Train_PlusMinusTest$`Min.Proj` + Batters2016Cleaned_Train_PlusMinusTest$Pro + Batters2016Cleaned_Train_PlusMinusTest$Bargain
  #      + Batters2016Cleaned_Train_PlusMinusTest$Own1 + Batters2016Cleaned_Train_PlusMinusTest$PER + Batters2016Cleaned_Train_PlusMinusTest$Pts + Batters2016Cleaned_Train_PlusMinusTest$Usage +
  #        Batters2016Cleaned_Train_PlusMinusTest$Opp_Plus_Minus + Batters2016Cleaned_Train_PlusMinusTest$PaceD + Batters2016Cleaned_Train_PlusMinusTest$TS_Per + Batters2016Cleaned_Train_PlusMinusTest$Fouls_36 +
  #        Batters2016Cleaned_Train_PlusMinusTest$Points_Touch + Batters2016Cleaned_Train_PlusMinusTest$Touches + Batters2016Cleaned_Train_PlusMinusTest$Rest + Batters2016Cleaned_Train_PlusMinusTest$Pts +
  #        Batters2016Cleaned_Train_PlusMinusTest$`Opp.Pts` + Batters2016Cleaned_Train_PlusMinusTest$delta + Batters2016Cleaned_Train_PlusMinusTest$Spread + Batters2016Cleaned_Train_PlusMinusTest$O_U +
  #        Batters2016Cleaned_Train_PlusMinusTest$Spread_per + Batters2016Cleaned_Train_PlusMinusTest$Upside + Batters2016Cleaned_Train_PlusMinusTest$Duds + Batters2016Cleaned_Train_PlusMinusTest$Count +
  #        Batters2016Cleaned_Train_PlusMinusTest$YPlus_Minus + Batters2016Cleaned_Train_PlusMinusTest$YDuds + Batters2016Cleaned_Train_PlusMinusTest$YCount+    Batters2016Cleaned_Train_PlusMinusTest$PTS +
  #        Batters2016Cleaned_Train_PlusMinusTest$REB + Batters2016Cleaned_Train_PlusMinusTest$STL + Batters2016Cleaned_Train_PlusMinusTest$BLK
  #     )) 
  # 
  # xgbO_PlusMinus = xgboost(data = dtrain_PlusMinus ,booster = "gblinear" , eta = 0.1 , max_depth=50, nthread = 4,
  #                          nrounds=2000,objective = "reg:linear" , verbose = 0 )
  # 
  # plusMinus =  Batters2016Cleaned_Train_PlusMinusTest$`Act.Pts` - predict(xgbO_PlusMinus,testSparseMatrix_PlusMinus )
  # xgbPLUSMINUS_M = ceil(min(plusMinus))
  # xgbPLUSMINUS_P = ceil(max(plusMinus))
  ##################################
  
  
  
  Prediction2 =  as.data.frame(RFPred)
  Prediction2["RFPer"] = as.data.frame(  Prediction2["RFPred"]*100/(Batters2016Cleaned_Test$`Salary`) )
  Prediction2["RF_M"] =  as.data.frame(RFPLUSMINUS_M)
  Prediction2["RF_P"] =  as.data.frame(RFPLUSMINUS_P)
  Prediction2["Actual"] =  as.data.frame(Batters2016Cleaned_Test$`Act.Pts`)
  Prediction2["Salary"] =  as.data.frame(Batters2016Cleaned_Test$`Salary`)
  Prediction2["Name"] =  as.data.frame(Batters2016Cleaned_Test$Player)
  Prediction2["HTeam"] =  as.data.frame(Batters2016Cleaned_Test$Team)
  Prediction2["Opp"] = as.data.frame(Batters2016Cleaned_Test$Opp)
  Prediction2["Pts"] =  as.data.frame(Batters2016Cleaned_Test$Pts)
  Prediction2["Pos"] =  as.data.frame(Batters2016Cleaned_Test$Pos)
  Prediction2["Xgb"] =  0 #as.data.frame(predict(xgbO, testSparseMatrix))
  Prediction2["XgbPer"] = 0 #as.data.frame(  Prediction2["Xgb"]*100/(Batters2016Cleaned_Test$`Salary`) )
  Prediction2["xgb_M"] =  0 #as.data.frame(xgbPLUSMINUS_M)
  Prediction2["xgb_P"] =  0 #as.data.frame(xgbPLUSMINUS_P)
  Prediction2["DNN"] = 0
  # if (typeof(m1) == "S4"){
  #   Prediction2["DNN"] =  0 #as.data.frame(h2o.predict(m1,newdata=testDNN))    
  # }
  # else{
  #   Prediction2["DNN"] = 0
  # }
  # 
  Prediction2["DNNPer"] = 0# as.data.frame(  Prediction2["DNN"]*100/(Batters2016Cleaned_Test$`Salary`) )
  
  ResultsBatters = rbind(ResultsBatters, Prediction2)
  
}




write.csv(ResultsBatters, file = "MLB_03_30_2018.csv")

#### Pitchers
Pitchers2016Cleaned = Pitchers2016[,c("Date","Rating","Player","Salary",
                                      "Ceiling","Floor","Proj_plusMinus",
                                      "Pts_Sal","LowProjOwn","HighProjOwn","ActPts","WHIP","HR_9",
                                      "SO_9","IP","QualS_S","SO_AB","wOBA","Pro",
                                      "Bargain","KPred","ParkF","Runs","OppRuns","delta",
                                      "ML","O_U","MLYear","RatingYear","Temp","WindSpd",
                                      "Humidity","Precip","Cnt","Dist","EV","GB",
                                      "LD","HH","Speed","S","Dist_delta","EV_delta","PCnt",
                                      "Spd_delta","RecBBL","Air","CntYear","DistYear","EVYear",
                                      "SpeedYear","SYear",
                                      "PCntYear","AirYear","PPG","Change","plusMinusYear","Count",
                                      "PPGYear","ChangeYear","CountYear")]

Pitchers2016Cleaned = unique(Pitchers2016Cleaned)
Pitchers2016Cleaned_Test = subset(Pitchers2016Cleaned, (as.Date(Pitchers2016Cleaned$Date) == as.Date(DateCheck)))
Pitchers2016Cleaned_Train = subset(Pitchers2016Cleaned, as.Date(Pitchers2016Cleaned$Date) < as.Date(DateCheck))


playerNames = unique(Pitchers2016Cleaned_Test$Player)


ResultsPitchers = data.frame( RFPred = numeric(), Xgb = numeric(), Name = factor(), Pos = factor() ,
                             Salary = numeric(), Actual = numeric() , HTeam = factor(), OTeam = factor(),
                             Pts = numeric(), DNNPer = numeric(), DNN = numeric(),xgbPLUSMINUS = numeric(),
                             RFPLUSMINUS = numeric())



## Prediction
for (each in 20:length(playerNames)){
  Pitchers2016Cleaned_Test = subset(Pitchers2016Cleaned, Pitchers2016Cleaned$Date == DateCheck 
                                    & Pitchers2016Cleaned$Player == as.character(playerNames[each]) )
  
  Pitchers2016Cleaned_Train = subset(Pitchers2016Cleaned, Pitchers2016Cleaned$Date != DateCheck
                                     & Pitchers2016Cleaned$Player == as.character(playerNames[each]) )
  Pitchers2016Cleaned_Train = subset(Pitchers2016Cleaned_Train, as.Date(Pitchers2016Cleaned_Train$Date) > "2017-07-05")
  print (playerNames[each])
  print (each)
  ### This ensures atleast 1 row of data exists for prediction
  if (nrow(Pitchers2016Cleaned_Test) < 1 ){
    next
  }
  

  ######### Construct Models
  indices = sample(1:nrow(Pitchers2016Cleaned_Train), 7, replace = FALSE)
  Pitchers2016Cleaned_Train_PlusMinusTrain = Pitchers2016Cleaned_Train[-indices, ]
  Pitchers2016Cleaned_Train_PlusMinusTest = Pitchers2016Cleaned_Train[indices, ]
  
  
  #########RF
  rf = randomForest( Pitchers2016Cleaned_Train[,c("Rating","Salary",
                                                  "Proj_plusMinus",
                                                  "Pts_Sal","LowProjOwn","HighProjOwn","ActPts","WHIP","HR_9",
                                                  "SO_9","IP","QualS_S","SO_AB","wOBA","Pro",
                                                  "Bargain","KPred","ParkF","Runs","OppRuns","delta",
                                                  "ML","O_U","MLYear","RatingYear","Temp","WindSpd",
                                                  "Humidity","Precip","Cnt","Dist","EV","GB",
                                                  "LD","HH","Speed","S","Dist_delta","EV_delta","PCnt",
                                                  "Spd_delta","RecBBL","Air","CntYear","DistYear","EVYear",
                                                  "SpeedYear","SYear",
                                                  "PCntYear","AirYear","PPG","Change","plusMinusYear","Count",
                                                  "PPGYear","ChangeYear","CountYear")], 
                     y = Pitchers2016Cleaned_Train[,c("ActPts")], ntree=300
                     ,type='regression')
  
  rf_PlusMinus =  randomForest( Pitchers2016Cleaned_Train_PlusMinusTrain[,c("Rating","Salary",
                                                                            "Proj_plusMinus",
                                                                            "Pts_Sal","LowProjOwn","HighProjOwn","ActPts","WHIP","HR_9",
                                                                            "SO_9","IP","QualS_S","SO_AB","wOBA","Pro",
                                                                            "Bargain","KPred","ParkF","Runs","OppRuns","delta",
                                                                            "ML","O_U","MLYear","RatingYear","Temp","WindSpd",
                                                                            "Humidity","Precip","Cnt","Dist","EV","GB",
                                                                            "LD","HH","Speed","S","Dist_delta","EV_delta","PCnt",
                                                                            "Spd_delta","RecBBL","Air","CntYear","DistYear","EVYear",
                                                                            "SpeedYear","SYear",
                                                                            "PCntYear","AirYear","PPG","Change","plusMinusYear","Count",
                                                                            "PPGYear","ChangeYear","CountYear")], 
                                y = Pitchers2016Cleaned_Train_PlusMinusTrain[,c("ActPts")], ntree=300
                                ,type='regression')
  
  ####XGB
  # trainSparceMatrix = sparse.model.matrix( Pitchers2016Cleaned_Train$`Act.Pts` ~ 
  #                                            ( Pitchers2016Cleaned_Train$Rating + Pitchers2016Cleaned_Train$`Usg.Proj` + Pitchers2016Cleaned_Train$Pts_Sal 
  #                                              + Pitchers2016Cleaned_Train$`Min.Proj` + Pitchers2016Cleaned_Train$Pro + Pitchers2016Cleaned_Train$Bargain
  #                                              + Pitchers2016Cleaned_Train$Own1 + Pitchers2016Cleaned_Train$PER + Pitchers2016Cleaned_Train$Pts + Pitchers2016Cleaned_Train$Usage +
  #                                                Pitchers2016Cleaned_Train$Opp_Plus_Minus + Pitchers2016Cleaned_Train$PaceD + Pitchers2016Cleaned_Train$TS_Per + Pitchers2016Cleaned_Train$Fouls_36 +
  #                                                Pitchers2016Cleaned_Train$Points_Touch + Pitchers2016Cleaned_Train$Touches + Pitchers2016Cleaned_Train$Rest + Pitchers2016Cleaned_Train$Pts +
  #                                                Pitchers2016Cleaned_Train$`Opp.Pts` + Pitchers2016Cleaned_Train$delta + Pitchers2016Cleaned_Train$Spread + Pitchers2016Cleaned_Train$O_U +
  #                                                Pitchers2016Cleaned_Train$Spread_per + Pitchers2016Cleaned_Train$Upside + Pitchers2016Cleaned_Train$Duds + Pitchers2016Cleaned_Train$Count +
  #                                                Pitchers2016Cleaned_Train$YPlus_Minus + Pitchers2016Cleaned_Train$YDuds + Pitchers2016Cleaned_Train$YCount+    Pitchers2016Cleaned_Train$PTS +
  #                                                Pitchers2016Cleaned_Train$REB + Pitchers2016Cleaned_Train$STL + Pitchers2016Cleaned_Train$BLK ))
  # 
  # Labels = Matrix(Pitchers2016Cleaned_Train$`Act.Pts`, sparse = TRUE)
  # 
  # dtrain <- xgb.DMatrix(data = trainSparceMatrix, label=Labels)
  # 
  # trainSparceMatrix_PlusMinus = sparse.model.matrix(     Pitchers2016Cleaned_Train_PlusMinusTrain$`Act.Pts` ~ 
  #                                                          (Pitchers2016Cleaned_Train_PlusMinusTrain$Rating +  Pitchers2016Cleaned_Train_PlusMinusTrain$`Usg.Proj` + Pitchers2016Cleaned_Train_PlusMinusTrain$Pts_Sal 
  #                                                           + Pitchers2016Cleaned_Train_PlusMinusTrain$`Min.Proj` + Pitchers2016Cleaned_Train_PlusMinusTrain$Pro + Pitchers2016Cleaned_Train_PlusMinusTrain$Bargain
  #                                                           + Pitchers2016Cleaned_Train_PlusMinusTrain$Own1 + Pitchers2016Cleaned_Train_PlusMinusTrain$PER + Pitchers2016Cleaned_Train_PlusMinusTrain$Pts + Pitchers2016Cleaned_Train_PlusMinusTrain$Usage +
  #                                                             Pitchers2016Cleaned_Train_PlusMinusTrain$Opp_Plus_Minus + Pitchers2016Cleaned_Train_PlusMinusTrain$PaceD + Pitchers2016Cleaned_Train_PlusMinusTrain$TS_Per + Pitchers2016Cleaned_Train_PlusMinusTrain$Fouls_36 +
  #                                                             Pitchers2016Cleaned_Train_PlusMinusTrain$Points_Touch + Pitchers2016Cleaned_Train_PlusMinusTrain$Touches + Pitchers2016Cleaned_Train_PlusMinusTrain$Rest + Pitchers2016Cleaned_Train_PlusMinusTrain$Pts +
  #                                                             Pitchers2016Cleaned_Train_PlusMinusTrain$`Opp.Pts` + Pitchers2016Cleaned_Train_PlusMinusTrain$delta + Pitchers2016Cleaned_Train_PlusMinusTrain$Spread + Pitchers2016Cleaned_Train_PlusMinusTrain$O_U +
  #                                                             Pitchers2016Cleaned_Train_PlusMinusTrain$Spread_per + Pitchers2016Cleaned_Train_PlusMinusTrain$Upside + Pitchers2016Cleaned_Train_PlusMinusTrain$Duds + Pitchers2016Cleaned_Train_PlusMinusTrain$Count +
  #                                                             Pitchers2016Cleaned_Train_PlusMinusTrain$YPlus_Minus + Pitchers2016Cleaned_Train_PlusMinusTrain$YDuds + Pitchers2016Cleaned_Train_PlusMinusTrain$YCount+    Pitchers2016Cleaned_Train_PlusMinusTrain$PTS +
  #                                                             Pitchers2016Cleaned_Train_PlusMinusTrain$REB + Pitchers2016Cleaned_Train_PlusMinusTrain$STL + Pitchers2016Cleaned_Train_PlusMinusTrain$BLK ))
  # Labels_PlusMinus = Matrix(Pitchers2016Cleaned_Train_PlusMinusTrain$`Act.Pts`, sparse = TRUE)
  # 
  # dtrain_PlusMinus <- xgb.DMatrix(data = trainSparceMatrix_PlusMinus, label=Labels_PlusMinus)
  
  ###H20
  
  # TrainingH20= as.h2o(Pitchers2016Cleaned_Train)
  # splits <- h2o.splitFrame(TrainingH20, c(0.9),  seed=1234)
  # 
  # 
  # trainDNN  <- h2o.assign(splits[[1]], "train.hex") # 60%
  # validDNN   <- h2o.assign(splits[[2]], "valid.hex") # 60%
  # TrainingH202= as.h2o(Pitchers2016Cleaned_Test)
  # splits2 <- h2o.splitFrame(TrainingH202,  seed=0)
  # testDNN <- h2o.assign(splits2[[1]], "test.hex")  # 20%
  # 
  # response <- "Act.Pts"
  # 
  # predictors <- c("Rating", "Salary","Proj_Plus_Minus","Pts_Sal","Usg.Proj","Min.Proj",
  #                 "Own1","Own2","Imp.Pts","FP_Min","PER",
  #                 "Usage","Pro","Bargain","Opp_Plus_Minus","PaceD","TS_Per","Fouls_36",
  #                 "Points_Touch","Touches","Rest","Pts","Opp.Pts","delta","Spread",
  #                 "O_U","Spread_per","PPG","Consistency",
  #                 "Upside","Duds","Count","YPPG","YPlus_Minus","YConsistency","YUpside","YDuds","YCount","PTS","REB","STL","BLK" )
  # 
  # m1 = tryCatch(
  #   {expr = h2o.deeplearning(
  #     model_id="dl_model_first",
  #     training_frame=trainDNN,
  #     validation_frame=validDNN,   ## validation dataset: used for scoring and early stopping
  #     x=predictors,
  #     y=response,
  #     nfold = 5,
  #     #activation="Rectifier",  ## default
  #     hidden=c(300,100),       ## default: 2 hidden layers with 200 neurons each
  #     variable_importances=T,
  #     epochs = 5,
  #     categorical_encoding = "OneHotInternal"
  #   )},
  #   error = function(i){return (0)}
  #   
  # )
  # 
  #################################Predictions
  
  RFPred = predict( rf,  Pitchers2016Cleaned_Test[,c("Rating","Salary",
                                                     "Proj_plusMinus",
                                                     "Pts_Sal","LowProjOwn","HighProjOwn","ActPts","WHIP","HR_9",
                                                     "SO_9","IP","QualS_S","SO_AB","wOBA","Pro",
                                                     "Bargain","KPred","ParkF","Runs","OppRuns","delta",
                                                     "ML","O_U","MLYear","RatingYear","Temp","WindSpd",
                                                     "Humidity","Precip","Cnt","Dist","EV","GB",
                                                     "LD","HH","Speed","S","Dist_delta","EV_delta","PCnt",
                                                     "Spd_delta","RecBBL","Air","CntYear","DistYear","EVYear",
                                                     "SpeedYear","SYear",
                                                     "PCntYear","AirYear","PPG","Change","plusMinusYear","Count",
                                                     "PPGYear","ChangeYear","CountYear")] 
                    ,type = c("response") )
  
  
  RFPred_PlusMinus = predict( rf_PlusMinus, 
                              Pitchers2016Cleaned_Train_PlusMinusTest[,c("Rating","Salary",
                                                                         "Proj_plusMinus",
                                                                         "Pts_Sal","LowProjOwn","HighProjOwn","ActPts","WHIP","HR_9",
                                                                         "SO_9","IP","QualS_S","SO_AB","wOBA","Pro",
                                                                         "Bargain","KPred","ParkF","Runs","OppRuns","delta",
                                                                         "ML","O_U","MLYear","RatingYear","Temp","WindSpd",
                                                                         "Humidity","Precip","Cnt","Dist","EV","GB",
                                                                         "LD","HH","Speed","S","Dist_delta","EV_delta","PCnt",
                                                                         "Spd_delta","RecBBL","Air","CntYear","DistYear","EVYear",
                                                                         "SpeedYear","SYear",
                                                                         "PCntYear","AirYear","PPG","Change","plusMinusYear","Count",
                                                                         "PPGYear","ChangeYear","CountYear")] 
                              ,type = c("response") )
  plusMinus = Pitchers2016Cleaned_Train_PlusMinusTest$`ActPts` - RFPred_PlusMinus
  RFPLUSMINUS_M = ceil(min(plusMinus))
  RFPLUSMINUS_P = ceil(max(plusMinus))
  # 
  # testSparseMatrix = sparse.model.matrix( 
  #   Pitchers2016Cleaned_Test$`Act.Pts` ~ 
  #     (Pitchers2016Cleaned_Test$Rating + Pitchers2016Cleaned_Test$`Usg.Proj` + Pitchers2016Cleaned_Test$Pts_Sal 
  #      + Pitchers2016Cleaned_Test$`Min.Proj` + Pitchers2016Cleaned_Test$Pro + Pitchers2016Cleaned_Test$Bargain
  #      + Pitchers2016Cleaned_Test$Own1 + Pitchers2016Cleaned_Test$PER + Pitchers2016Cleaned_Test$Pts + Pitchers2016Cleaned_Test$Usage +
  #        Pitchers2016Cleaned_Test$Opp_Plus_Minus + Pitchers2016Cleaned_Test$PaceD + Pitchers2016Cleaned_Test$TS_Per + Pitchers2016Cleaned_Test$Fouls_36 +
  #        Pitchers2016Cleaned_Test$Points_Touch + Pitchers2016Cleaned_Test$Touches + Pitchers2016Cleaned_Test$Rest + Pitchers2016Cleaned_Test$Pts +
  #        Pitchers2016Cleaned_Test$`Opp.Pts` + Pitchers2016Cleaned_Test$delta + Pitchers2016Cleaned_Test$Spread + Pitchers2016Cleaned_Test$O_U +
  #        Pitchers2016Cleaned_Test$Spread_per + Pitchers2016Cleaned_Test$Upside + Pitchers2016Cleaned_Test$Duds + Pitchers2016Cleaned_Test$Count +
  #        Pitchers2016Cleaned_Test$YPlus_Minus + Pitchers2016Cleaned_Test$YDuds + Pitchers2016Cleaned_Test$YCount+    
  #        Pitchers2016Cleaned_Test$REB + Pitchers2016Cleaned_Test$STL + Pitchers2016Cleaned_Test$BLK
  #     )) 
  # 
  # xgbO = xgboost(data = dtrain ,booster = "gblinear" , eta = 0.1 , max_depth=50, nthread = 4,
  #                nrounds=2000,objective = "reg:linear" , verbose = 0 )
  # 
  # predict(xgbO,testSparseMatrix )
  # 
  # 
  # testSparseMatrix_PlusMinus = sparse.model.matrix( 
  #   Pitchers2016Cleaned_Train_PlusMinusTest$`Act.Pts` ~ 
  #     (Pitchers2016Cleaned_Train_PlusMinusTest$Rating + Pitchers2016Cleaned_Train_PlusMinusTest$`Usg.Proj` + Pitchers2016Cleaned_Train_PlusMinusTest$Pts_Sal 
  #      + Pitchers2016Cleaned_Train_PlusMinusTest$`Min.Proj` + Pitchers2016Cleaned_Train_PlusMinusTest$Pro + Pitchers2016Cleaned_Train_PlusMinusTest$Bargain
  #      + Pitchers2016Cleaned_Train_PlusMinusTest$Own1 + Pitchers2016Cleaned_Train_PlusMinusTest$PER + Pitchers2016Cleaned_Train_PlusMinusTest$Pts + Pitchers2016Cleaned_Train_PlusMinusTest$Usage +
  #        Pitchers2016Cleaned_Train_PlusMinusTest$Opp_Plus_Minus + Pitchers2016Cleaned_Train_PlusMinusTest$PaceD + Pitchers2016Cleaned_Train_PlusMinusTest$TS_Per + Pitchers2016Cleaned_Train_PlusMinusTest$Fouls_36 +
  #        Pitchers2016Cleaned_Train_PlusMinusTest$Points_Touch + Pitchers2016Cleaned_Train_PlusMinusTest$Touches + Pitchers2016Cleaned_Train_PlusMinusTest$Rest + Pitchers2016Cleaned_Train_PlusMinusTest$Pts +
  #        Pitchers2016Cleaned_Train_PlusMinusTest$`Opp.Pts` + Pitchers2016Cleaned_Train_PlusMinusTest$delta + Pitchers2016Cleaned_Train_PlusMinusTest$Spread + Pitchers2016Cleaned_Train_PlusMinusTest$O_U +
  #        Pitchers2016Cleaned_Train_PlusMinusTest$Spread_per + Pitchers2016Cleaned_Train_PlusMinusTest$Upside + Pitchers2016Cleaned_Train_PlusMinusTest$Duds + Pitchers2016Cleaned_Train_PlusMinusTest$Count +
  #        Pitchers2016Cleaned_Train_PlusMinusTest$YPlus_Minus + Pitchers2016Cleaned_Train_PlusMinusTest$YDuds + Pitchers2016Cleaned_Train_PlusMinusTest$YCount+    Pitchers2016Cleaned_Train_PlusMinusTest$PTS +
  #        Pitchers2016Cleaned_Train_PlusMinusTest$REB + Pitchers2016Cleaned_Train_PlusMinusTest$STL + Pitchers2016Cleaned_Train_PlusMinusTest$BLK
  #     )) 
  # 
  # xgbO_PlusMinus = xgboost(data = dtrain_PlusMinus ,booster = "gblinear" , eta = 0.1 , max_depth=50, nthread = 4,
  #                          nrounds=2000,objective = "reg:linear" , verbose = 0 )
  # 
  # plusMinus =  Pitchers2016Cleaned_Train_PlusMinusTest$`Act.Pts` - predict(xgbO_PlusMinus,testSparseMatrix_PlusMinus )
  # xgbPLUSMINUS_M = ceil(min(plusMinus))
  # xgbPLUSMINUS_P = ceil(max(plusMinus))
  ##################################
  
  
  
  Prediction2 =  as.data.frame(RFPred)
  Prediction2["RFPer"] = as.data.frame(  Prediction2["RFPred"]*100/(Pitchers2016Cleaned_Test$`Salary`) )
  Prediction2["RF_M"] =  as.data.frame(RFPLUSMINUS_M)
  Prediction2["RF_P"] =  as.data.frame(RFPLUSMINUS_P)
  Prediction2["Actual"] =  as.data.frame(Pitchers2016Cleaned_Test$`Act.Pts`)
  Prediction2["Salary"] =  as.data.frame(Pitchers2016Cleaned_Test$`Salary`)
  Prediction2["Name"] =  as.data.frame(Pitchers2016Cleaned_Test$Player)
  Prediction2["HTeam"] =  as.data.frame(Pitchers2016Cleaned_Test$Team)
  Prediction2["Opp"] = as.data.frame(Pitchers2016Cleaned_Test$Opp)
  Prediction2["Pts"] =  as.data.frame(Pitchers2016Cleaned_Test$Pts)
  Prediction2["Pos"] =  as.data.frame(Pitchers2016Cleaned_Test$Pos)
  Prediction2["Xgb"] =  0 #as.data.frame(predict(xgbO, testSparseMatrix))
  Prediction2["XgbPer"] = 0 #as.data.frame(  Prediction2["Xgb"]*100/(Pitchers2016Cleaned_Test$`Salary`) )
  Prediction2["xgb_M"] =  0 #as.data.frame(xgbPLUSMINUS_M)
  Prediction2["xgb_P"] =  0 #as.data.frame(xgbPLUSMINUS_P)
  Prediction2["DNN"] = 0
  # if (typeof(m1) == "S4"){
  #   Prediction2["DNN"] =  0 #as.data.frame(h2o.predict(m1,newdata=testDNN))    
  # }
  # else{
  #   Prediction2["DNN"] = 0
  # }
  # 
  Prediction2["DNNPer"] = 0# as.data.frame(  Prediction2["DNN"]*100/(Pitchers2016Cleaned_Test$`Salary`) )
  
  ResultsPitchers = rbind(ResultsPitchers, Prediction2)
  
}

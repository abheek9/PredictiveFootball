######


###  Boosting Predictions


######

library(caret)

testDataSameElos = read.csv("testDataAllTeamSameElo.csv", header = TRUE)
testDataSameElos$X = NULL

View(testDataSameElos)

testDataDifferentElos = read.csv("testDataDifferentEloTier.csv", header = TRUE)
testDataDifferentElos$X = NULL

View(testDataDifferentElos)

trainData1 = testDataSameElos[1:1140,]
result1 = trainData1$result
testData1 = testDataSameElos[1141:1900,]
real1 = testData1$result


trainData2 = testDataDifferentElos[1:1140,]
result2 = trainData2$result
testData2 = testDataDifferentElos[1141:1900,]
real2 = testData2$result


objControl = trainControl(method = 'cv',number = 10,classProbs= TRUE)

#####

## Run on data from 1st simulation

####





modelCasualgbm = train(result1 ~. ,data = trainData1[,13:ncol(trainData1)], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm)
summary(modelCasualgbm)

modelCasualgbm2 = train(result1 ~. ,data = trainData1[,3:4], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm2)
summary(modelCasualgbm2)

modelCasualgbm3 = train(result1 ~. ,data = trainData1[,c(3:4, 13:ncol(trainData1) )], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm3)
summary(modelCasualgbm3)

predict1a = predict.train(modelCasualgbm, newdata = testData1, type = "raw")

table(predict1a, real1)
accuracy1 = (39+14+292)/760

predict1b = predict.train(modelCasualgbm2, newdata = testData1, type = "raw")

table(predict1b, real1)
accuracy2 = (68+21+292)/760

predict1c = predict.train(modelCasualgbm3, newdata = testData1, type = "raw")

table(predict1c, real1)
accuracy3 = (70+28+276)/760


#####

## Run on data from 2nd simulation

#####
modelCasualgbm4 = train(result2 ~. ,data = trainData2[,13:ncol(trainData2)], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm4)
summary(modelCasualgbm4)

modelCasualgbm5 = train(result2 ~. ,data = trainData2[,3:4], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm5)
summary(modelCasualgbm5)

modelCasualgbm6 = train(result2 ~. ,data = trainData2[,c(3:4, 13:ncol(trainData2) )], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm6)
summary(modelCasualgbm6)

predict2a = predict.train(modelCasualgbm4, newdata = testData2, type = "raw")

table(predict2a, real2)
accuracy4 = (57+13+295)/760

predict2b = predict.train(modelCasualgbm5, newdata = testData2, type = "raw")

table(predict2b, real2)
accuracy5 = (74+16+281)/760

predict2c = predict.train(modelCasualgbm6, newdata = testData2, type = "raw")

table(predict2c, real2)
accuracy6 = (46+31+277)/760


#########

# Simulation 3 - 4

########

testDataSameElos2 = read.csv("testDataAllTeamSameElo2.csv", header = TRUE)
testDataSameElos2$X = NULL

View(testDataSameElos2)

testDataDifferentElos2 = read.csv("testDataDifferentEloTier2.csv", header = TRUE)
testDataDifferentElos2$X = NULL

View(testDataDifferentElos2)

trainData3 = testDataSameElos2[1:2660,]
result3 = trainData3$result
testData3 = testDataSameElos2[2661:3800,]
real3 = testData3$result


trainData4 = testDataDifferentElos2[1:2660,]
result4 = trainData4$result
testData4 = testDataDifferentElos2[2661:3800,]
real4 = testData4$result


objControl = trainControl(method = 'cv',number = 10,classProbs= TRUE)

#####

## Run on data from 1st simulation

####

par(mfrow = c(1,1))

modelCasualgbm7 = train(result3 ~. ,data = trainData3[,13:ncol(trainData3)], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm7)
summary(modelCasualgbm7)

modelCasualgbm8 = train(result3 ~. ,data = trainData3[,3:4], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm8)
summary(modelCasualgbm8)

modelCasualgbm9 = train(result3 ~. ,data = trainData3[,c(3:4, 13:ncol(trainData3) )], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm9)
summary(modelCasualgbm9)

predict3a = predict.train(modelCasualgbm7, newdata = testData3, type = "raw")

table(predict3a, real3)
accuracy7 = (83+5+485)/1140

predict3b = predict.train(modelCasualgbm8, newdata = testData3, type = "raw")

table(predict3b, real3)
accuracy8 = (121+4+450)/1140

predict3c = predict.train(modelCasualgbm9, newdata = testData3, type = "raw")

table(predict3c, real3)
accuracy9 = (101+3+493)/1140


#####

## Run on data from 4th simulation

#####

modelCasualgbm10 = train(result4 ~. ,data = trainData4[,13:ncol(trainData4)], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm10)
summary(modelCasualgbm10)

modelCasualgbm11 = train(result4 ~. ,data = trainData4[,3:4], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm11)
summary(modelCasualgbm11)

modelCasualgbm12 = train(result4 ~. ,data = trainData4[,c(3:4, 13:ncol(trainData4) )], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm12)
summary(modelCasualgbm12)

predict4a = predict.train(modelCasualgbm10, newdata = testData4, type = "raw")

table(predict4a, real4)

accuracy10 = (72+0+492)/1140

predict4b = predict.train(modelCasualgbm11, newdata = testData4, type = "raw")

table(predict4b, real4)
accuracy11 = (100+3+486)/1140

predict4c = predict.train(modelCasualgbm12, newdata = testData4, type = "raw")

table(predict4c, real4)
accuracy12 = (106+2+485)/1140

######

#########

# Simulation 5-6

########

testDataSameElos3 = read.csv("testDataAllTeamSameElo3.csv", header = TRUE)
testDataSameElos3$X = NULL

View(testDataSameElos3)

testDataDifferentElos3 = read.csv("testDataDifferentEloTier3.csv", header = TRUE)
testDataDifferentElos3$X = NULL

View(testDataDifferentElos3)

trainData5 = testDataSameElos3[1:6180,]
result5 = trainData5$result
testData5 = testDataSameElos3[6181:nrow(testDataSameElos3),]
real5 = testData5$result


trainData6 = testDataDifferentElos3[1:6180,]
result6 = trainData6$result
testData6 = testDataDifferentElos3[6181:nrow(testDataDifferentElos3),]
real6 = testData6$result


objControl = trainControl(method = 'cv',number = 10,classProbs= TRUE)

#####

## Run on data from 5th simulation

####

par(mfrow = c(1,1))

modelCasualgbm13 = train(result5 ~. ,data = trainData5[,13:ncol(trainData5)], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm13)
summary(modelCasualgbm13)

modelCasualgbm14 = train(result5 ~. ,data = trainData5[,3:4], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm14)
summary(modelCasualgbm14)

modelCasualgbm15 = train(result5 ~. ,data = trainData5[,c(3:4, 13:ncol(trainData5) )], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm15)
summary(modelCasualgbm15)

predict5a = predict.train(modelCasualgbm13, newdata = testData5, type = "raw")

table(predict5a, real5)
accuracy13 = (19+7+1072)/2362

predict5b = predict.train(modelCasualgbm14, newdata = testData5, type = "raw")

table(predict5b, real5)
accuracy14 = (13+19+1065)/2362

predict5c = predict.train(modelCasualgbm15, newdata = testData5, type = "raw")

table(predict5c, real5)
accuracy15 = (40+2+1069)/2362


#####

## Run on data from 6th simulation

#####
modelCasualgbm16 = train(result6 ~. ,data = trainData6[,13:ncol(trainData6)], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm16)
summary(modelCasualgbm16)

modelCasualgbm17 = train(result6 ~. ,data = trainData6[,3:4], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm17)
summary(modelCasualgbm17)

modelCasualgbm18 = train(result6 ~. ,data = trainData6[,c(3:4, 13:ncol(trainData6) )], method = "gbm",trControl = objControl, metric='Accuracy')

print(modelCasualgbm18)
summary(modelCasualgbm18)

predict6a = predict.train(modelCasualgbm16, newdata = testData6, type = "raw")

table(predict6a, real6)
accuracy16 = (8+5+1081)/2362

predict6b = predict.train(modelCasualgbm17, newdata = testData6, type = "raw")

table(predict6b, real6)
accuracy17 = (31+1+1064)/2362

predict6c = predict.train(modelCasualgbm18, newdata = testData6, type = "raw")

table(predict6c, real6)
accuracy18 = (59+15+1032)/2362

######







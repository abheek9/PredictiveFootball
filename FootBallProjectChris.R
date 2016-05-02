
#################  multinomial classification   #####################################################
library(glmnet)
library(randomForest) 
library(nnet)

data <- read.csv("testDataAllTeamSameElo.csv")

data <- data[,13:30]
names(data)
str(data)

View(data)

prop.table(table(data$result))

#barplot of result proportions
barplot(prop.table(table(data$result)))


Train<-data[1:1140, ]          # training data
Test<-data[1141:1900, ]     # testing data
str(Train)
dim(Train)
str(Test)
dim(Test)

############################### ELO - LASSO predictions 1 ############################################################

TrainX1<-model.matrix(~.,data=Train1[,-which(colnames(Train1)=="result")])[,-1]
View(TrainX1)
TrainY1<-Train1$result
length(TrainY1)
dim(TrainX1)
levels(TrainY1)

TestX1=model.matrix(~.,data=Test1[,-which(colnames(Train1)=="result")])[,-1]
dim(TestX1)

lasso.fit1=glmnet(x=TrainX1, y=TrainY1, family = "multinomial", lambda = 0, alpha=.99)

summary(lasso.fit1)
lasso.fit1$beta

lasso.pred1=predict(lasso.fit1, TestX1, type="response")   # gives us three prob's
lasso.pred.label1=predict(lasso.fit1, TestX1, type="class")

lasso.pred1

table(lasso.pred.label1, Test1[, 3])  
mean(lasso.pred.label1 != Test1[, 3])
####################################ELO- Random Forest 1 ################################################################

rf.mce=vector()
for (i in 1:12) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    # we only need to pull out the last error
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 3]) # confusion matrix
mean(rf.pred.label != Test[, 3])


############################### ELO with Tier - LASSO predictions 2 ############################################################

TrainX<-model.matrix(~.,data=Train[,-which(colnames(Train)=="result")])[,-1]
View(TrainX)
TrainY<-Train$result
length(TrainY)
dim(TrainX)
levels(TrainY)

TestX=model.matrix(~.,data=Test[,-which(colnames(Train)=="result")])[,-1]
dim(TestX)

lasso.fit=glmnet(x=TrainX, y=TrainY, family = "multinomial", lambda = 0, alpha=.99)

summary(lasso.fit)
lasso.fit$beta

lasso.pred=predict(lasso.fit, TestX, type="response")   # gives us three prob's
lasso.pred.label=predict(lasso.fit, TestX, type="class")

lasso.pred

table(lasso.pred.label, Test[, 1])  
mean(lasso.pred.label != Test[, 1])
####################################ELO with Tier - Random Forest 2################################################################

rf.mce=vector()
for (i in 1:12) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    # we only need to pull out the last error
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 1]) # confusion matrix
mean(rf.pred.label != Test[, 1])




############################### Teams  Prediction ############################################################ <- data[,]
data <- read.csv("testDataAllTeamSameElo.csv")
data1 <- data [, c(4,5,13)]

names(data1)
str(data1)

View(data1)

prop.table(table(data1$result))

#barplot of result proportions
barplot(prop.table(table(data1$result)))


Train1<-data1[1:1140, ]          
Test1<-data1[1141:1900, ] 


### Teams ELO #####################################
TrainX<-model.matrix(~.,data=Train1[,-which(colnames(Train1)=="result")])[,-1]
View(TrainX)
TrainY<-Train1$result
length(TrainY)
dim(TrainX)
levels(TrainY)
TestX=model.matrix(~.,data=Test1[,-which(colnames(Train1)=="result")])[,-1]
dim(TestX)

lasso.fit=glmnet(x=TrainX, y=TrainY, family = "multinomial", lambda = 0, alpha=.99)

summary(lasso.fit)
lasso.fit$beta

lasso.pred=predict(lasso.fit, TestX, type="response")   # gives us three prob's
lasso.pred.label=predict(lasso.fit, TestX, type="class")

lasso.pred

table(lasso.pred.label, Test1[, 3])  
mean(lasso.pred.label != Test1[, 3])


### Teams RF

rf.mce=vector()
for (i in 1:12) {
  rf.fit=randomForest(result~., data=Train1, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    # we only need to pull out the last error
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train1, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test1,type="prob")
rf.pred.label=predict(rf.fit,Test1,type="class")

table(rf.pred.label, Test1[, 3]) # confusion matrix
mean(rf.pred.label != Test1[, 3])


####################### 2010 - 2014 --  Elo With Tier ####################### 

new.data <- read.csv("testDataDifferentEloTier.csv")
str(new.data)


data <- new.data[,13:31]
names(data)
str(data)

View(new.data)

prop.table(table(data$result))

#barplot of result proportions
barplot(prop.table(table(data$result)))


Train<-data[1:1140, ]          # training data
Test<-data[1141:1900, ]     # testing data
str(Train)
dim(Train)
str(Test)
dim(Test)


#################################### 2010- 2014 - Random Forest ################################################################

rf.mce=vector()
for (i in 1:12) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    # we only need to pull out the last error
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 1]) # confusion matrix
mean(rf.pred.label != Test[, 1])



################----------ELO & Teams ----------################
data <- read.csv("testDataAllTeamSameElo.csv")

data3 <- data[, c(4,5,13:30)]
names(data3)
str(data3)

View(data3)

prop.table(table(data3$result))

#barplot of result proportions
barplot(prop.table(table(data3$result)))


Train<-data3[1:1140, ]          # training data
Test<-data3[1141:1900, ]     # testing data
str(Train)
dim(Train)
str(Test)
dim(Test)

#################################### 2010~2014 - ELO & Team - Random Forest ################################################################

rf.mce=vector()
for (i in 1:12) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    # we only need to pull out the last error
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 3]) # confusion matrix
mean(rf.pred.label != Test[, 3])



#################################### 2000~2010 - Names - Random Forest ################################################################

data <- read.csv("testDataAllTeamSameElo2.csv")
str(data)
data1 <- data[, c(4,5, 13)]
names(data1)
str(data1)

View(data1)

Train<-data1[1:2660, ]          # training data
Test<-data1[2661:3800, ]     # testing data
str(Train)
dim(Train)
str(Test)
dim(Test)

rf.mce=vector()
for (i in 1:10) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 3]) # confusion matrix
mean(rf.pred.label != Test[, 3])

####################################2000~2010 - ELO Same Seeds - Random Forest ################################################################

data <- read.csv("testDataAllTeamSameElo2.csv")
str(data)
data1 <- data[, c(13:31)]
names(data1)
str(data1)

View(data1)

Train<-data1[1:2660, ]          # training data
Test<-data1[2661:3800, ]     # testing data
str(Train)
dim(Train)
str(Test)
dim(Test)



rf.mce=vector()
for (i in 1:12) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 1]) # confusion matrix
mean(rf.pred.label != Test[, 1])

####################################2000~2010 - ELO Same Seeds & Names - Random Forest ################################################################

data <- read.csv("testDataAllTeamSameElo2.csv")
str(data)
data1 <- data[, c(4,5, 13:31)]
names(data1)
str(data1)

View(data1)

Train<-data1[1:2660, ]          # training data
Test<-data1[2661:3800, ]     # testing data
str(Train)
dim(Train)
str(Test)
dim(Test)

rf.mce=vector()
for (i in 1:10) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 3]) # confusion matrix
mean(rf.pred.label != Test[, 3])

############################ 2000~2010 ELO Different Seeds- Random Forest ####################################################

data <- read.csv("testDataDifferentEloTier2.csv")
str(data)
data1 <- data[, c(13:31)]
names(data1)
str(data1)

View(data1)

Train<-data1[1:2660, ]          # training data
Test<-data1[2661:3800, ]     # testing data
str(Train)
dim(Train)
str(Test)
dim(Test)


rf.mce=vector()
for (i in 1:10) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 1]) # confusion matrix
mean(rf.pred.label != Test[, 1])

####################################2000~2010 - ELO Different Seeds & Names- Random Forest ################################################################

data <- read.csv("testDataDifferentEloTier2.csv")
str(data)
data1 <- data[, c(4,5, 13:31)]
names(data1)
str(data1)

View(data1)

Train<-data1[1:2660, ]          # training data
Test<-data1[2661:3800, ]     # testing data
str(Train)
dim(Train)
str(Test)
dim(Test)

rf.mce=vector()
for (i in 1:10) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 3]) # confusion matrix
mean(rf.pred.label != Test[, 3])


#################################### 1980-2000 - Names - Random Forest ################################################################

data <- read.csv("testDataAllTeamSameElo3.csv")
str(data)
View(data)
data1 <- data[, c(4,5, 13)]
names(data1)
str(data1)

View(data1)

Train<-data1[1:6180, ]          # training data
Test<-data1[6181:8542, ]     # testing data
str(Train)
dim(Train)
str(Test)
dim(Test)

rf.mce=vector()
for (i in 1:10) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 3]) # confusion matrix
mean(rf.pred.label != Test[, 3])

####################################1980-2000 - ELO Same Seeds - Random Forest ################################################################

data <- read.csv("testDataAllTeamSameElo3.csv")
str(data)
data1 <- data[, c(13:31)]
names(data1)
str(data1)

View(data1)

Train<-data1[1:6180, ]          # training data
Test<-data1[6181:8542, ]      # testing data
str(Train)
dim(Train)
str(Test)
dim(Test)



rf.mce=vector()
for (i in 1:10) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    
}

plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 1]) # confusion matrix
mean(rf.pred.label != Test[, 1])

####################################1980-2000 - ELO Same Seeds & Names - Random Forest ################################################################

data <- read.csv("testDataAllTeamSameElo3.csv")
str(data)
data1 <- data[, c(4,5, 13:31)]
names(data1)
str(data1)

View(data1)

Train<-data1[1:6180, ]          # training data
Test<-data1[6181:8542, ]      # testing data
str(Train)
dim(Train)
str(Test)
dim(Test)

rf.mce=vector()
for (i in 1:10) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 3]) # confusion matrix
mean(rf.pred.label != Test[, 3])

############################ 1980-2000 ELO Different Seeds- Random Forest ####################################################

data <- read.csv("testDataDifferentEloTier3.csv")
str(data)
data1 <- data[, c(13:31)]
names(data1)
str(data1)

View(data1)

Train<-data1[1:6180, ]          # training data
Test<-data1[6181:8542, ]      # testing data
str(Train)
dim(Train)
str(Test)
dim(Test)


rf.mce=vector()
for (i in 1:10) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 1]) # confusion matrix
mean(rf.pred.label != Test[, 1])

#################################### 1980-2000 - ELO Different Seeds & Names- Random Forest ################################################################

data <- read.csv("testDataDifferentEloTier3.csv")
str(data)
data1 <- data[, c(4,5, 13:31)]
names(data1)
str(data1)

View(data1)

Train<-data1[1:6180, ]          # training data
Test<-data1[6181:8542, ]      # testing data
str(Train)
dim(Train)
str(Test)
dim(Test)

rf.mce=vector()
for (i in 1:10) {
  rf.fit=randomForest(result~., data=Train, mtry=i, ntree=100)
  #plot(rf.fit)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]    
}
plot(rf.mce)

rf.fit=randomForest(result~., data=Train, mtry=2, ntree=100)
rf.pred=predict(rf.fit,Test,type="prob")
rf.pred.label=predict(rf.fit,Test,type="class")

table(rf.pred.label, Test[, 3]) # confusion matrix
mean(rf.pred.label != Test[, 3])



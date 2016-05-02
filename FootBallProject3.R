
######

# Simulation 5

######

trainData = premData[premData$Season < 1980,]
trainData = trainData[trainData$Season>=1960,]

trainData = trainData[order(as.Date(as.character(trainData$Date))),]

#View(trainData)

testData = premData[premData$Season>=1980,]
testData = testData[testData$tier == 1,]
testData = testData[testData$Season < 2000,]
testData = testData[order(as.Date(as.character(testData$Date))),]

View(trainData)
View(testData)


#K factor of 5
elos <- new.env()
elos5Hist <- new.env()

#K factor of 10
elos10 <- new.env()
elos10Hist <- new.env()

#K factor of 15
elos15 <- new.env()
elos15Hist <- new.env()

#K factor of 20
elos20 <- new.env()
elos20Hist <- new.env()

#K factor of 25
elos25 <- new.env()
elos25Hist <- new.env()

#K factor of 50
elos50 <- new.env()
elos50Hist <- new.env()

#####


####Need to think about inital callibration
####Teams in lower tiers need lower starting scores


#####

for(i in 1:length(allTeams)){
  elos[[allTeams[i]]] = 100
  elos5Hist[[allTeams[i]]] = 100
  
  elos10[[allTeams[i]]] = 100
  elos10Hist[[allTeams[i]]] = 100
  
  elos15[[allTeams[i]]] = 100
  elos15Hist[[allTeams[i]]] = 100
  
  elos20[[allTeams[i]]] = 100
  elos20Hist[[allTeams[i]]] = 100
  
  elos25[[allTeams[i]]] = 100
  elos25Hist[[allTeams[i]]] = 100
  
  elos50[[allTeams[i]]] = 100
  elos50Hist[[allTeams[i]]] = 100
}

check1 = 0
check2 = 0
check3 = 0

for(i in 1:nrow(trainData)){
  
  teamA = as.character(trainData[i,3])
  teamB = as.character(trainData[i,4])
  
  scoreA = 0.5
  scoreB = 0.5
  
  #print(i)
  if(trainData[i,12]=="A"){
    scoreA = 0 
    scoreB = 1
    check1 = check1 + 1 
  }
  if(trainData[i,12]=="D"){
    scoreA = 0.5 
    scoreB = 0.5
    check2 = check2 + 1 
  }
  if(trainData[i,12]=="H"){
    scoreA = 1 
    scoreB = 0
    check3 = check3 + 1 
  }
  
  for(j in 1:6){
    #Calculate Elo Score with K factor 5
    if(j == 1){
      eloA = elos[[teamA]]
      eloB = elos[[teamB]]
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 5*(scoreA - expectA)
      newB = eloB + 5*(scoreB - expectB)
      elos5Hist[[teamA]] = c(elos5Hist[[teamA]], newA) 
      elos5Hist[[teamB]] = c(elos5Hist[[teamB]], newB) 
      elos[[teamA]] = newA
      elos[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 10
    if(j==2){
      eloA = elos10[[teamA]]
      eloB = elos10[[teamB]]
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 10*(scoreA - expectA)
      newB = eloB + 10*(scoreB - expectB)
      elos10Hist[[teamA]] = c(elos10Hist[[teamA]], newA) 
      elos10Hist[[teamB]] = c(elos10Hist[[teamB]], newB) 
      elos10[[teamA]] = newA
      elos10[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 15
    if(j==3){
      
      eloA = elos15[[teamA]]
      eloB = elos15[[teamB]]
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 15*(scoreA - expectA)
      newB = eloB + 15*(scoreB - expectB)
      elos15Hist[[teamA]] = c(elos15Hist[[teamA]], newA) 
      elos15Hist[[teamB]] = c(elos15Hist[[teamB]], newB) 
      elos15[[teamA]] = newA
      elos15[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 20
    if(j==4){
      
      eloA = elos20[[teamA]]
      eloB = elos20[[teamB]]
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 20*(scoreA - expectA)
      newB = eloB + 20*(scoreB - expectB)
      elos20Hist[[teamA]] = c(elos20Hist[[teamA]], newA) 
      elos20Hist[[teamB]] = c(elos20Hist[[teamB]], newB) 
      elos20[[teamA]] = newA
      elos20[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 25
    if(j==5){
      
      eloA = elos25[[teamA]]
      eloB = elos25[[teamB]]
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 25*(scoreA - expectA)
      newB = eloB + 25*(scoreB - expectB)
      elos25Hist[[teamA]] = c(elos25Hist[[teamA]], newA) 
      elos25Hist[[teamB]] = c(elos25Hist[[teamB]], newB) 
      elos25[[teamA]] = newA
      elos25[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 50
    if(j==6){
      
      eloA = elos50[[teamA]]
      eloB = elos50[[teamB]]
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 50*(scoreA - expectA)
      newB = eloB + 50*(scoreB - expectB)
      elos50Hist[[teamA]] = c(elos50Hist[[teamA]], newA) 
      elos50Hist[[teamB]] = c(elos50Hist[[teamB]], newB) 
      elos50[[teamA]] = newA
      elos50[[teamB]] = newB
    }
    
  }
  
}



testData$elo5Team1 = 0
testData$elo5Team2 = 0
testData$elo10Team1 = 0
testData$elo10Team2 = 0
testData$elo15Team1 = 0
testData$elo15Team2 = 0
testData$elo20Team1 = 0
testData$elo20Team2 = 0
testData$elo25Team1 = 0
testData$elo25Team2 = 0
testData$elo50Team1 = 0
testData$elo50Team2 = 0

#check that the testData is ordered by time
View(testData)

#from previous series as extra column, this is time series so must use all information
#up to the match
for(i in 1:nrow(testData)){
  teamA = as.character(testData[i,3])
  teamB = as.character(testData[i,4])
  
  scoreA = 0.5
  scoreB = 0.5
  
  if(testData[i,12]=="A"){
    scoreA = 0 
    scoreB = 1
    check1 = check1 + 1 
  }
  if(testData[i,12]=="D"){
    scoreA = 0.5 
    scoreB = 0.5
    check2 = check2 + 1 
  }
  if(testData[i,12]=="H"){
    scoreA = 1 
    scoreB = 0
    check3 = check3 + 1 
  }
  
  
  for(j in 1:6){
    #Calculate Elo Score with K factor 5
    if(j == 1){
      eloA = elos[[teamA]]
      eloB = elos[[teamB]]
      
      testData[i,13] = eloA
      testData[i,14] = eloB
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 5*(scoreA - expectA)
      newB = eloB + 5*(scoreB - expectB)
      elos5Hist[[teamA]] = c(elos5Hist[[teamA]], newA) 
      elos5Hist[[teamB]] = c(elos5Hist[[teamB]], newB) 
      elos[[teamA]] = newA
      elos[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 10
    if(j==2){
      eloA = elos10[[teamA]]
      eloB = elos10[[teamB]]
      
      testData[i,15] = eloA
      testData[i,16] = eloB
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 10*(scoreA - expectA)
      newB = eloB + 10*(scoreB - expectB)
      elos10Hist[[teamA]] = c(elos10Hist[[teamA]], newA) 
      elos10Hist[[teamB]] = c(elos10Hist[[teamB]], newB) 
      elos10[[teamA]] = newA
      elos10[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 15
    if(j==3){
      
      eloA = elos15[[teamA]]
      eloB = elos15[[teamB]]
      
      testData[i,17] = eloA
      testData[i,18] = eloB
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 15*(scoreA - expectA)
      newB = eloB + 15*(scoreB - expectB)
      elos15Hist[[teamA]] = c(elos15Hist[[teamA]], newA) 
      elos15Hist[[teamB]] = c(elos15Hist[[teamB]], newB) 
      elos15[[teamA]] = newA
      elos15[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 20
    if(j==4){
      
      eloA = elos20[[teamA]]
      eloB = elos20[[teamB]]
      
      testData[i,19] = eloA
      testData[i,20] = eloB
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 20*(scoreA - expectA)
      newB = eloB + 20*(scoreB - expectB)
      elos20Hist[[teamA]] = c(elos20Hist[[teamA]], newA) 
      elos20Hist[[teamB]] = c(elos20Hist[[teamB]], newB) 
      elos20[[teamA]] = newA
      elos20[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 25
    if(j==5){
      
      eloA = elos25[[teamA]]
      eloB = elos25[[teamB]]
      
      testData[i,21] = eloA
      testData[i,22] = eloB
      
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 25*(scoreA - expectA)
      newB = eloB + 25*(scoreB - expectB)
      elos25Hist[[teamA]] = c(elos25Hist[[teamA]], newA) 
      elos25Hist[[teamB]] = c(elos25Hist[[teamB]], newB) 
      elos25[[teamA]] = newA
      elos25[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 50
    if(j==6){
      
      eloA = elos50[[teamA]]
      eloB = elos50[[teamB]]
      
      
      testData[i,23] = eloA
      testData[i,24] = eloB
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 50*(scoreA - expectA)
      newB = eloB + 50*(scoreB - expectB)
      elos50Hist[[teamA]] = c(elos50Hist[[teamA]], newA) 
      elos50Hist[[teamB]] = c(elos50Hist[[teamB]], newB) 
      elos50[[teamA]] = newA
      elos50[[teamB]] = newB
    }
    
  }
  
}

testData$elo5Dif = testData$elo5Team1 - testData$elo5Team2
testData$elo10Dif = testData$elo10Team1 - testData$elo10Team2
testData$elo15Dif = testData$elo15Team1 - testData$elo15Team2
testData$elo20Dif = testData$elo20Team1 - testData$elo20Team2
testData$elo25Dif = testData$elo25Team1 - testData$elo25Team2
testData$elo50Dif = testData$elo50Team1 - testData$elo50Team2

write.csv(testData, "testDataAllTeamSameElo3.csv")


#########

#  Simulation 6

#########


startingElo5 = new.env()
startingElo10 = new.env()
startingElo15 = new.env()
startingElo20 = new.env()
startingElo25 = new.env()
startingElo50 = new.env()


for(i in 1:length(allTeams)){
  
  startingElo5[[allTeams[i]]] = 0
  startingElo10[[allTeams[i]]] = 0
  startingElo15[[allTeams[i]]] = 0
  startingElo20[[allTeams[i]]] = 0
  startingElo25[[allTeams[i]]] = 0
  startingElo50[[allTeams[i]]] = 0
  
  
}

for(i in 1:nrow(trainData)){
  
  teamA = as.character(trainData[i,3])
  teamB = as.character(trainData[i,4])
  
  tempTier = trainData[i,9]
  
  if(startingElo5[[teamA]] == 0){
    if(tempTier==1){
      startingElo5[[teamA]] = 100
      startingElo10[[teamA]] = 100
      startingElo15[[teamA]] = 100
      startingElo20[[teamA]] = 100
      startingElo25[[teamA]] = 100
      startingElo50[[teamA]] = 100
    }
    if(tempTier==2){
      startingElo5[[teamA]] = 80
      startingElo10[[teamA]] = 80
      startingElo15[[teamA]] = 80
      startingElo20[[teamA]] = 80
      startingElo25[[teamA]] = 80
      startingElo50[[teamA]] = 80
    }
    if(tempTier==3){
      startingElo5[[teamA]] = 60
      startingElo10[[teamA]] = 60
      startingElo15[[teamA]] = 60
      startingElo20[[teamA]] = 60
      startingElo25[[teamA]] = 60
      startingElo50[[teamA]] = 60
    }
    if(tempTier==4){
      startingElo5[[teamA]] = 40
      startingElo10[[teamA]] = 40
      startingElo15[[teamA]] = 40
      startingElo20[[teamA]] = 40
      startingElo25[[teamA]] = 40
      startingElo50[[teamA]] = 40
    }
  }
  
  if(startingElo[[teamB]] == 0){
    if(tempTier==1){
      startingElo5[[teamB]] = 100
      startingElo10[[teamB]] = 100
      startingElo15[[teamB]] = 100
      startingElo20[[teamB]] = 100
      startingElo25[[teamB]] = 100
      startingElo50[[teamB]] = 100
    }
    if(tempTier==2){
      startingElo5[[teamB]] = 80
      startingElo10[[teamB]] = 80
      startingElo15[[teamB]] = 80
      startingElo20[[teamB]] = 80
      startingElo25[[teamB]] = 80
      startingElo50[[teamB]] = 80
    }
    if(tempTier==3){
      startingElo5[[teamB]] = 60
      startingElo10[[teamB]] = 60
      startingElo15[[teamB]] = 60
      startingElo20[[teamB]] = 60
      startingElo25[[teamB]] = 60
      startingElo50[[teamB]] = 60
    }
    if(tempTier==4){
      startingElo5[[teamB]] = 40
      startingElo10[[teamB]] = 40
      startingElo15[[teamB]] = 40
      startingElo20[[teamB]] = 40
      startingElo25[[teamB]] = 40
      startingElo50[[teamB]] = 40
    }
  }
}

####
####
####



######  Simulate Elos from 1960 to 2010 with different starting scores based on league

#K factor of 5

elos5HistNew <- new.env()

#K factor of 10

elos10HistNew <- new.env()

#K factor of 15

elos15HistNew <- new.env()

#K factor of 20

elos20HistNew <- new.env()

#K factor of 25

elos25HistNew <- new.env()

#K factor of 50

elos50HistNew <- new.env()


check1 = 0
check2 = 0
check3 = 0

for(i in 1:nrow(trainData)){
  
  teamA = as.character(trainData[i,3])
  teamB = as.character(trainData[i,4])
  
  scoreA = 0.5
  scoreB = 0.5
  
  #print(i)
  if(trainData[i,12]=="A"){
    scoreA = 0 
    scoreB = 1
    check1 = check1 + 1 
  }
  if(trainData[i,12]=="D"){
    scoreA = 0.5 
    scoreB = 0.5
    check2 = check2 + 1 
  }
  if(trainData[i,12]=="H"){
    scoreA = 1 
    scoreB = 0
    check3 = check3 + 1 
  }
  
  for(j in 1:6){
    #Calculate Elo Score with K factor 5
    if(j == 1){
      eloA = startingElo5[[teamA]]
      eloB = startingElo5[[teamB]]
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 5*(scoreA - expectA)
      newB = eloB + 5*(scoreB - expectB)
      elos5HistNew[[teamA]] = c(elos5HistNew[[teamA]], newA) 
      elos5HistNew[[teamB]] = c(elos5HistNew[[teamB]], newB) 
      startingElo5[[teamA]] = newA
      startingElo5[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 10
    if(j==2){
      eloA = startingElo10[[teamA]]
      eloB = startingElo10[[teamB]]
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 10*(scoreA - expectA)
      newB = eloB + 10*(scoreB - expectB)
      elos10HistNew[[teamA]] = c(elos10HistNew[[teamA]], newA) 
      elos10HistNew[[teamB]] = c(elos10HistNew[[teamB]], newB) 
      startingElo10[[teamA]] = newA
      startingElo10[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 15
    if(j==3){
      
      eloA = startingElo15[[teamA]]
      eloB = startingElo15[[teamB]]
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 15*(scoreA - expectA)
      newB = eloB + 15*(scoreB - expectB)
      elos15HistNew[[teamA]] = c(elos15HistNew[[teamA]], newA) 
      elos15HistNew[[teamB]] = c(elos15HistNew[[teamB]], newB) 
      startingElo15[[teamA]] = newA
      startingElo15[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 20
    if(j==4){
      
      eloA = startingElo20[[teamA]]
      eloB = startingElo20[[teamB]]
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 20*(scoreA - expectA)
      newB = eloB + 20*(scoreB - expectB)
      elos20HistNew[[teamA]] = c(elos20HistNew[[teamA]], newA) 
      elos20HistNew[[teamB]] = c(elos20HistNew[[teamB]], newB) 
      startingElo20[[teamA]] = newA
      startingElo20[[teamB]] = newB
    }
    
    #Calculate Elo Score with K factor 25
    if(j==5){
      
      eloA = startingElo25[[teamA]]
      eloB = startingElo25[[teamB]]
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 25*(scoreA - expectA)
      newB = eloB + 25*(scoreB - expectB)
      elos25HistNew[[teamA]] = c(elos25HistNew[[teamA]], newA) 
      elos25HistNew[[teamB]] = c(elos25HistNew[[teamB]], newB) 
      startingElo25[[teamA]] = newA
      startingElo25[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 50
    if(j==6){
      
      eloA = startingElo50[[teamA]]
      eloB = startingElo50[[teamB]]
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 50*(scoreA - expectA)
      newB = eloB + 50*(scoreB - expectB)
      elos50HistNew[[teamA]] = c(elos50HistNew[[teamA]], newA) 
      elos50HistNew[[teamB]] = c(elos50HistNew[[teamB]], newB) 
      startingElo50[[teamA]] = newA
      startingElo50[[teamB]] = newB
    }
    
  }
  
}


testData$elo5Team1 = 0
testData$elo5Team2 = 0
testData$elo10Team1 = 0
testData$elo10Team2 = 0
testData$elo15Team1 = 0
testData$elo15Team2 = 0
testData$elo20Team1 = 0
testData$elo20Team2 = 0
testData$elo25Team1 = 0
testData$elo25Team2 = 0
testData$elo50Team1 = 0
testData$elo50Team2 = 0

#check that the testData is ordered by time
View(testData)

#from previous series as extra column, this is time series so must use all information
#up to the match
for(i in 1:nrow(testData)){
  teamA = as.character(testData[i,3])
  teamB = as.character(testData[i,4])
  
  scoreA = 0.5
  scoreB = 0.5
  
  if(testData[i,12]=="A"){
    scoreA = 0 
    scoreB = 1
    check1 = check1 + 1 
  }
  if(testData[i,12]=="D"){
    scoreA = 0.5 
    scoreB = 0.5
    check2 = check2 + 1 
  }
  if(testData[i,12]=="H"){
    scoreA = 1 
    scoreB = 0
    check3 = check3 + 1 
  }
  
  for(j in 1:6){
    #Calculate Elo Score with K factor 5
    if(j == 1){
      eloA = startingElo5[[teamA]]
      eloB = startingElo5[[teamB]]
      
      testData[i,13] = eloA
      testData[i,14] = eloB
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 5*(scoreA - expectA)
      newB = eloB + 5*(scoreB - expectB)
      elos5HistNew[[teamA]] = c(elos5HistNew[[teamA]], newA) 
      elos5HistNew[[teamB]] = c(elos5HistNew[[teamB]], newB) 
      startingElo5[[teamA]] = newA
      startingElo5[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 10
    if(j==2){
      eloA = startingElo10[[teamA]]
      eloB = startingElo10[[teamB]]
      
      testData[i,15] = eloA
      testData[i,16] = eloB
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 10*(scoreA - expectA)
      newB = eloB + 10*(scoreB - expectB)
      elos10HistNew[[teamA]] = c(elos10HistNew[[teamA]], newA) 
      elos10HistNew[[teamB]] = c(elos10HistNew[[teamB]], newB) 
      startingElo10[[teamA]] = newA
      startingElo10[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 15
    if(j==3){
      
      eloA = startingElo15[[teamA]]
      eloB = startingElo15[[teamB]]
      
      testData[i,17] = eloA
      testData[i,18] = eloB
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 15*(scoreA - expectA)
      newB = eloB + 15*(scoreB - expectB)
      elos15HistNew[[teamA]] = c(elos15HistNew[[teamA]], newA) 
      elos15HistNew[[teamB]] = c(elos15HistNew[[teamB]], newB) 
      startingElo15[[teamA]] = newA
      startingElo15[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 20
    if(j==4){
      
      eloA = startingElo20[[teamA]]
      eloB = startingElo20[[teamB]]
      
      testData[i,19] = eloA
      testData[i,20] = eloB
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 20*(scoreA - expectA)
      newB = eloB + 20*(scoreB - expectB)
      elos20HistNew[[teamA]] = c(elos20HistNew[[teamA]], newA) 
      elos20HistNew[[teamB]] = c(elos20HistNew[[teamB]], newB) 
      startingElo20[[teamA]] = newA
      startingElo20[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 25
    if(j==5){
      
      eloA = startingElo25[[teamA]]
      eloB = startingElo25[[teamB]]
      
      testData[i,21] = eloA
      testData[i,22] = eloB
      
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 25*(scoreA - expectA)
      newB = eloB + 25*(scoreB - expectB)
      elos25HistNew[[teamA]] = c(elos25HistNew[[teamA]], newA) 
      elos25HistNew[[teamB]] = c(elos25HistNew[[teamB]], newB) 
      startingElo25[[teamA]] = newA
      startingElo25[[teamB]] = newB
    }
    #Calculate Elo Score with K factor 50
    if(j==6){
      
      eloA = startingElo50[[teamA]]
      eloB = startingElo50[[teamB]]
      
      testData[i,23] = eloA
      testData[i,24] = eloB
      
      #this is the elo estimate of the score, it will always produce
      #an output from 0 to 1 since it maps elo scores with the logistic function
      expectA = 1/(1+exp(eloB-eloA))
      expectB = 1/(1+exp(eloA-eloB))
      
      #Update the Elo estimate #I will use a kfactor of 5 for test purposes
      newA = eloA + 50*(scoreA - expectA)
      newB = eloB + 50*(scoreB - expectB)
      elos50HistNew[[teamA]] = c(elos50HistNew[[teamA]], newA) 
      elos50HistNew[[teamB]] = c(elos50HistNew[[teamB]], newB) 
      startingElo50[[teamA]] = newA
      startingElo50[[teamB]] = newB
    }
  }
}

testData$elo5Dif = testData$elo5Team1 - testData$elo5Team2
testData$elo10Dif = testData$elo10Team1 - testData$elo10Team2
testData$elo15Dif = testData$elo15Team1 - testData$elo15Team2
testData$elo20Dif = testData$elo20Team1 - testData$elo20Team2
testData$elo25Dif = testData$elo25Team1 - testData$elo25Team2
testData$elo50Dif = testData$elo50Team1 - testData$elo50Team2

write.csv(testData, "testDataDifferentEloTier3.csv")

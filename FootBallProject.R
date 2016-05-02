
####  FootBall Project ####

setwd(dir = "/Users/abheekbasu/desktop/sports betting/Football/engsoccerdata/data-raw")

rm(list=ls())

bundData = read.csv("bundesliga.csv", header = TRUE)
#View(bundData)

premData = read.csv("engsoccerdata2.csv", header = TRUE)
View(premData)

faData = read.csv("facup.csv", header = TRUE)
View(faData)

orangeData = read.csv("holland1.csv", header = TRUE)
#View(orangeData)

italyData = read.csv("italycalcio.csv", header = TRUE)
#View(italyData)

spainData = read.csv("spainliga.csv", header = TRUE)
#View(spainData)

playData = read.csv("playoffs.csv", header = TRUE)
#View(playData)

premTeam = read.csv("engsoccerteams.csv", header = TRUE)
#View(premTeam)

faTeam = read.csv("facupteams.csv", header = TRUE)
#View(faTeam)



####


##   We will start of working with premier league data first and 
##   then move onto other if we have time

which(premData$tier != premData$division)

str(premData)
any(is.na(premData))


View(a)

draws = premData[premData$Season==1973,]
draws = nrow(draws[draws$goaldif==0,])
totgame = nrow(premData[premData$Season == 1973,])


######

### Score Distribution Breakdown

######

#  First lets make a hashmap of all scores

######

draws = c()
wins = c()
index = c()
startYear = c()
endYear = c()
div = c()

##First aggregate over all divisions over time frames from 1 to 50 years
for(i in 1:50){
  
  x  = 2014
  
  tDraws = c()
  tWins = c()
  timeFrame = c()
  syear = c()
  eyear = c()
  tDiv = c()
  
  while((x-i)>1887){
    
    #On a per tier basis
    for(j in 0:4){
      #calculate for whole year all division
      zediv = j
      
      if(j == 0){
        zediv = "All"
        a = premData[premData$Season > (x-i),]
        a = a[a$Season<=x,]
        #How many games in the time frame
        numGames = nrow(a)
        numDraws = length(which(a$goaldif==0))
        tDraws = c(tDraws, (numDraws/numGames))
        tWins = c(tWins, (1-(numDraws/numGames)))
        start = (x - i) + 1
        #print(paste( "?", start, "to", (x), " with time ", i))
        timeFrame = c(timeFrame, i)
        syear = c(syear, start )
        eyear = c(eyear, x)
        tDiv = c(tDiv, zediv)
        #timeFrame = c(timeFrame, paste("?",start, "to", (x), " with time ", i))
        
      }
      else{
        a = premData[premData$Season > (x-i),]
        a = a[a$Season<=x,]
        a = a[a$tier==zediv,]
        #How many games in the time frame
        numGames = nrow(a)
        numDraws = length(which(a$goaldif==0))
        tDraws = c(tDraws, (numDraws/numGames))
        tWins = c(tWins, (1-(numDraws/numGames)))
        start = (x - i) + 1
        #print(paste( "?", start, "to", (x), " with time ", i))
        timeFrame = c(timeFrame, i)
        syear = c(syear, start )
        eyear = c(eyear, x)
        tDiv = c(tDiv, zediv)
        #timeFrame = c(timeFrame, paste("?",start, "to", (x), " with time ", i))
      }
    }
    x = x -1
  }
  
  draws = c(draws, tDraws)
  wins = c(wins, tWins)
  index = c(index, timeFrame )
  startYear = c(startYear, syear)
  endYear = c(endYear, eyear)
  div = c(div, tDiv)
  
}

sportsDistribution = data.frame(index, wins, draws, startYear, endYear, div)
write.csv(sportsDistribution, "distributionOfScores.csv")

sportsDistribution = read.csv("distributionOfScores.csv", header = TRUE)
View(sportsDistribution)
sportsDistribution$X = NULL


scoresAllTiers = sportsDistribution[sportsDistribution$div=="All",]
View(scoresAllTiers)

par(mfrow = c(3,1))


###Across all Tiers
plot(main = "1 year Window",x = rev(scoresAllTiers$wins[scoresAllTiers$index==1]), xlab = "Years From 1889", ylab = "Win Percentage", col = "blue", pch = 16)
plot(main = "5 year Window",x = rev(scoresAllTiers$wins[scoresAllTiers$index==5]), xlab = "Years From 1894", ylab = "Win Percentage",col = "blue", pch = 16)
plot(main = "10 year Window",x = rev(scoresAllTiers$wins[scoresAllTiers$index==10]), xlab = "Years From 1899", ylab = "Win Percentage", col = "blue", pch = 16)

plot(main = "20 year Window",x = rev(scoresAllTiers$wins[scoresAllTiers$index==20]), xlab = "Years From 1909", ylab = "Win Percentage", col = "blue", pch = 16)
plot(main = "35 year Window",x = rev(scoresAllTiers$wins[scoresAllTiers$index==35]), xlab = "Years From 1924", ylab = "Win Percentage",col = "blue", pch = 16)
plot(main = "50 year Window",x = rev(scoresAllTiers$wins[scoresAllTiers$index==50]), xlab = "Years From 1939", ylab = "Win Percentage", col = "blue", pch = 16)

scoresTierOne = sportsDistribution[sportsDistribution$div==1,]
View(scoresTierOne)
scoresTierTwo = sportsDistribution[sportsDistribution$div==2,]
View(scoresTierTwo)
scoresTierThree = sportsDistribution[sportsDistribution$div==3,]
View(scoresTierThree)
scoresTierFour = sportsDistribution[sportsDistribution$div==4,]
View(scoresTierFour)

###Each Tier is show
plot(main = "1 year Window",x = rev(scoresAllTiers$wins[scoresAllTiers$index==1]), xlab = "Years From 1889", ylab = "Win Percentage", col = "blue", pch = 16)

lines(main = "1 year Window",x = rev(scoresAllTiers$wins[scoresAllTiers$index==1]), xlab = "Years From 1889", ylab = "Win Percentage", col = "blue", pch = 16)
lines(x = rev(scoresTierOne$wins[scoresTierOne$index==1]), col = "cyan4", pch = 16)

points(x = rev(scoresTierTwo$wins[scoresTierTwo$index==1]))
points(x = rev(scoresTierThree$wins[scoresTierThree$index==1]))
plot(main = "5 year Window",x = rev(scoresAllTiers$wins[scoresAllTiers$index==5]), xlab = "Years From 1889", ylab = "Win Percentage",col = "blue", pch = 16)
plot(main = "10 year Window",x = rev(scoresAllTiers$wins[scoresAllTiers$index==10]), xlab = "Years From 1889", ylab = "Win Percentage", col = "blue", pch = 16)

plot(main = "20 year Window",x = rev(scoresAllTiers$wins[scoresAllTiers$index==20]), xlab = "Years From 1889", ylab = "Win Percentage", col = "blue", pch = 16)
plot(main = "35 year Window",x = rev(scoresAllTiers$wins[scoresAllTiers$index==35]), xlab = "Years From 1889", ylab = "Win Percentage",col = "blue", pch = 16)
plot(main = "50 year Window",x = rev(scoresAllTiers$wins[scoresAllTiers$index==50]), xlab = "Years From 1889", ylab = "Win Percentage", col = "blue", pch = 16)

####

## We can see that the draw percentage has increased over time

####

View(sportsDistribution)


#####

### Thinking about ELO

#####

premData$TeamOne = 0

premData$TeamTwo = 0

View(premData)

for(i in 1:nrow(premData)){
  if(premData[i,11]>=0){
    premData[i,13] = 1
    premData[i,14] = 0
    
  }
  else if(premData[i,11]<=0){
    premData[i,13] = 0
    premData[i,14] = 1
  }
}

## To speed up Computation we start with 1960 data onwards and only use the same tier
#78385 to nrow, start with div 1

modernPremData = premData[78385:nrow(premData),]
View(modernPremData)
modDivOne = modernPremData[modernPremData$tier==1,]
View(modDivOne)

teams = read.csv("engsoccerteams.csv", header = TRUE)
View(teams)


write.csv(modernPremData, "ModernPremData.csv")

####

#### THE START OR BEAUTIFUL PERFORMANT CODE #####

####

###Check of the teams
homelevels = levels(premData$home)
visitorlevels = levels(premData$visitor)

allTeams = union(homelevels,visitorlevels) 
class(allTeams)

allScoreTypes = levels(premData$FT)


## Create relevant hash maps
## Must say this is some beautiful code!

teamMatchUps <- new.env()
teamTiers <- new.env()
teamTiersOverTime <- new.env()
teamYears <- new.env()
competitors <- new.env()

allScoreHash <- new.env()


for(i in 1:length(allScoreTypes)){
  allScoreHash[[allScoreTypes[i]]] = c(0)
}

for(i in 1:length(allTeams)){
  #teamMatchUps[[allTeams[i]]] = c(0)
  #teamTiers[[allTeams[i]]] = c(0)
  teamTiersOverTime[[allTeams[i]]] = c(0)
  #teamYears[[allTeams[i]]] = c(0)
  #competitors[[allTeams[i]]] = c(0)
}

for(i in 1:nrow(premData)){
  
  homeTeam = as.character(premData[i,3])
  awayTeam = as.character(premData[i,4])
  #tempYear = premData[i,2]
  tempTier = premData[i,9]
  
  #scoreType = as.character(premData[i,5])
  
  #teamMatchUps[[homeTeam]] = c(teamMatchUps[[homeTeam]], i)
  #teamMatchUps[[awayTeam]] = c(teamMatchUps[[awayTeam]], i)
  #competitors[[awayTeam]] = c(competitors[[awayTeam]], homeTeam)
  #competitors[[homeTeam]] = c(competitors[[homeTeam]], awayTeam)
  #teamYears[[awayTeam]] = c(teamYears[[awayTeam]], tempYear)
  #teamYears[[homeTeam]] = c(teamYears[[homeTeam]], tempYear)
  #teamTiers[[homeTeam]] = c(teamTiers[[homeTeam]], tempTier)
  #teamTiers[[awayTeam]] = c(teamTiers[[awayTeam]], tempTier)
  teamTiersOverTime[[homeTeam]] = c(teamTiersOverTime[[homeTeam]], tempTier)
  teamTiersOverTime[[awayTeam]] = c(teamTiersOverTime[[awayTeam]], tempTier)
  
  #allScoreHash[[scoreType]] = c( allScoreHash[[scoreType]], i)

}

for(i in 1:length(allTeams)){
  teamMatchUps[[allTeams[i]]] = unique(teamMatchUps[[allTeams[i]]])
  teamTiers[[allTeams[i]]] = unique(teamTiers[[allTeams[i]]])
  teamYears[[allTeams[i]]] = unique(teamYears[[allTeams[i]]])
  competitors[[allTeams[i]]] = unique(competitors[[allTeams[i]]])
}

sink("teamTiersOverTime.txt")
for(i in 1:length(allTeams)){
  print(allTeams[i])
  #print(teamMatchUps[[allTeams[i]]])
  #print(teamTiers[[allTeams[i]]])
  #print(teamYears[[allTeams[i]]])
  print(teamTiersOverTime[[allTeams[i]]])
  #print(competitors[[allTeams[i]]])
}
sink()


# reorder scores in an way that makes sense
sortedScores = allScoreTypes[c(1:2, 4:11, 3, 12:21, 36:95, 22:35)]
ss = lapply(sortedScores, function(var){
  length(allScoreHash[[var]])
})

ss = unlist(ss)

barplot(sortedScores,ss)

scoreDist = as.data.frame(t(ss))
names(scoreDist) = sortedScores
View(scoreDist)

write.csv(scoreDist, "EnglishPremLeagueScoreDist")

#######

######

### Break up into train and test sets


######

#
# Rerun with new train and test data put into a for loop for different k factors
#

######

trainData = premData[premData$Season < 2010,]
trainData = trainData[trainData$Season>=1960,]

trainData = trainData[order(as.Date(as.character(trainData$Date))),]
write.csv(trainData, "orderedTrainingData.csv")

View(trainData)


testData = premData[premData$Season>2009,]
testData = testData[testData$tier == 1,]
testData = testData[order(as.Date(as.character(testData$Date))),]

View(trainData)
View(testData)

teamsInTest = unique(union(levels(trainData$home), levels(trainData$visitor)))

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

### Save the Elos and Elos Histories over the simulation period
### Create Dataframe of Elos
efives = c()
etens = c()
efifteens = c()
etwenties = c()
etwentyfives = c()
efifties = c()
histfive = c()
histten = c()
histfifteen = c()
histtwenty = c()
histtwentyfive = c()
histfifty = c()

for(i in 1:length(allTeams)){
  print(allTeams[[i]])
  #K 5
  print(elos[[allTeams[i]]])
  efives = c(efives, elos[[allTeams[i]]])
  histfive = c(histfive, elos5Hist[[allTeams[i]]])
  #K 10
  print(elos10[[allTeams[i]]])
  etens = c(etens, elos10[[allTeams[i]]])
  histten = c(histten, elos10Hist[[allTeams[i]]])
  #K 15
  print(elos15[[allTeams[i]]])
  efifteens = c(efifteens, elos15[[allTeams[i]]])
  histfifteen = c(histfifteen, elos15Hist[[allTeams[i]]])
  #K 20
  print(elos20[[allTeams[i]]])
  etwenties = c(etwenties, elos20[[allTeams[i]]])
  histtwenty = c(histtwenty, elos20Hist[[allTeams[i]]])
  #K 25
  print(elos25[[allTeams[i]]])
  etwentyfives = c(etwentyfives, elos25[[allTeams[i]]])
  histtwentyfive = c(histtwentyfive, elos25Hist[[allTeams[i]]])
  #K 50
  print(elos50[[allTeams[i]]])
  efifties = c(efifties, elos50[[allTeams[i]]])
  histfifty = c(histfifty, elos50Hist[[allTeams[i]]])
  print(" ")
}

## We'll figure out what the problem is later 
#save this dataframe
#SAVE HIST OF SCORES TOO

eloScores = as.data.frame(t(round(efives, digits=2)),t(round(etens, digits=2)), t(round(efifteens, digits=2)), t(round(etwenties, digits=2)), t(round(etwentyfives, digits=2)), t(round(efifties, digits=2)))

testest = round(efives, digits=2)

sink("EloTimeSeriesAllSame.txt")
for(i in 1:length(allTeams)){
  
  print(allTeams[i])
  
  print("kfactor 5")
  print(histfive[i])
  
  print("kfactor 10")
  print(histten[i])
  
  print("kfactor 15")
  print(histfifteen[i])
  
  print("kfactor 20")
  print(histtwenty[i])
  
  print("kfactor 25")
  print(histtwentyfive[i])
  
  print("kfactor 50")
  print(histfifty[i])
}
sink()  



#write.csv(testData, "testoutputsmallrun.csv")

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

write.csv(testData, "testDataAllTeamSameElo.csv")

for(i in 1:length(allTeams)){
  sink("teamPredictedElos.txt")
  print(allTeams[i])
  print("kfactor 5")
  print(elos5Hist[[allTeams[i]]])
  print("kfactor 10")
  print(elos10Hist[[allTeams[i]]])
  print("kfactor 15")
  print(elos15Hist[[allTeams[i]]])
  print("kfactor 20")
  print(elos20Hist[[allTeams[i]]])
  print("kfactor 25")
  print(elos25Hist[[allTeams[i]]])
  print("kfactor 50")
  print(elos50Hist[[allTeams[i]]])
  sink()  
}


View(testData)



#####
##### Now we do it with different starting elo scores based on the tier
#####



View(trainData)

View(modernPremData)

### The following code snippet finds the first 
### instance that a team appears post 1960, and assigns a starting Elo
### based on the tier the team first appears


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

sink("StartingElosAfterSimulationDifferentEloTiers")
for(i in 1:length(allTeams)){
  print(allTeams[[i]])
  print("kfactor 5")
  print(startingElo5[[allTeams[i]]])
  
  print("kfactor 10")
  print(startingElo10[[allTeams[i]]])
  
  print("kfactor 15")
  print(startingElo15[[allTeams[i]]])
  
  print("kfactor 20")
  print(startingElo20[[allTeams[i]]])
  
  print("kfactor 25")
  print(startingElo25[[allTeams[i]]])
  
  print("kfactor 50")
  print(startingElo50[[allTeams[i]]])
  print(" ")
}
sink()

sink("ElosTimeSeriesDuringSimulationDifferentEloTiers")
for(i in 1:length(allTeams)){
  print(allTeams[[i]])
  print("kfactor 5")
  print(startingElo5[[allTeams[i]]])
  
  print("kfactor 10")
  print(startingElo10[[allTeams[i]]])
  
  print("kfactor 15")
  print(startingElo15[[allTeams[i]]])
  
  print("kfactor 20")
  print(startingElo20[[allTeams[i]]])
  
  print("kfactor 25")
  print(startingElo25[[allTeams[i]]])
  
  print("kfactor 50")
  print(startingElo50[[allTeams[i]]])
  print(" ")
}
sink()


#lets see the predicted elos after 4 seasons, not as simple as putting elo score
### Create Dataframe of Elos
efivesNew = c()
etensNew = c()
efifteensNew = c()
etwentiesNew = c()
etwentyfivesNew = c()
efiftiesNew = c()
histfiveNew = c()
histtenNew = c()
histfifteenNew = c()
histtwentyNew = c()
histtwentyfiveNew = c()
histfiftyNew = c()

## Saving the starting Elo for all teams in a single vector
## Saving the Elo score history in a single vector (vector of vectors)

for(i in 1:length(allTeams)){
  print(allTeams[[i]])
  #K 5
  print(startingElo5[[allTeams[i]]])
  efivesNew = c(efivesNew, startingElo5[[allTeams[i]]])
  histfiveNew = c(histfiveNew, elos5HistNew[[allTeams[i]]])
  #K 10
  print(startingElo10[[allTeams[i]]])
  etensNew = c(etensNew, startingElo10[[allTeams[i]]])
  histtenNew = c(histtenNew, elos10HistNew[[allTeams[i]]])
  #K 15
  print(startingElo15[[allTeams[i]]])
  efifteensNew = c(efifteensNew, startingElo15[[allTeams[i]]])
  histfifteenNew = c(histfifteenNew, elos15HistNew[[allTeams[i]]])
  #K 20
  print(startingElo20[[allTeams[i]]])
  etwentiesNew = c(etwentiesNew, startingElo20[[allTeams[i]]])
  histtwentyNew = c(histtwentyNew, elos20HistNew[[allTeams[i]]])
  #K 25
  print(startingElo25[[allTeams[i]]])
  etwentyfivesNew = c(etwentyfivesNew, startingElo25[[allTeams[i]]])
  histtwentyfiveNew = c(histtwentyfiveNew, elos25HistNew[[allTeams[i]]])
  #K 50
  print(startingElo50[[allTeams[i]]])
  efiftiesNew = c(efiftiesNew, startingElo50[[allTeams[i]]])
  histfiftyNew = c(histfiftyNew, elos50HistNew[[allTeams[i]]])
  print(" ")
}

## We'll figure out what the problem is later 
#save this dataframe
#SAVE HIST OF SCORES TOO

eloScores = as.data.frame(t(efivesNew), t(etensNew), t(efifteensNew), t(etwentiesNew), t(etwentyfivesNew), t(efiftiesNew))


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

write.csv(testData, "testDataDifferentEloTier.csv")

View(testData)


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



## Stat Learning Final Project

############################## CLEANING/ORDERING NCAA TOURNEY DETAILED RESULTS ############################## 
NCAATourneyDetailedResults <- read.csv("~/Desktop/School/Spring 2019/Stat Learning/Final Project/mens-machine-learning-competition-2019/DataFiles/NCAATourneyDetailedResults.csv")
tourneydetailed = NCAATourneyDetailedResults
colorder = sort(colnames(tourneydetailed))
tourney = tourneydetailed[, colorder]
colnames(tourney)
dim(tourney)
unique(tourney$WLoc) # can remove this, its all the same
which(colnames(tourney) == 'WLoc')
tourney = tourney[,-c(28)] 
colnames(tourney)
tourney$gamenumber = rev(seq(1,nrow(tourney),1))

winners = tourney[, c(1,17,18,19:33,34)]
colnames(winners)
losers = tourney[, c(1,17,18,2:16,34)]
colnames(losers)

colnames(winners) = gsub('W', '',colnames(winners))
colnames(losers) = gsub('L', '',colnames(losers))
colnames(winners)
colnames(losers) 
length(colnames(winners))
length(colnames(winners))
intersect(colnames(winners), colnames(losers))
length(intersect(colnames(winners), colnames(losers))) # all columns match for winners and losers

winners$win = rep(1, length(nrow(winners)))
losers$win = rep(0, length(nrow(losers)))
head(winners)
head(losers)

tourneyfresh = rbind(winners, losers)
head(tourneyfresh)
tail(tourneyfresh)
gameorder = order(tourneyfresh$gamenumber)
tourneyorder = tourneyfresh[gameorder,]
head(tourneyorder)
tail(tourneyorder)

tourneyorder$seedkey = paste(tourneyorder$Season, '-', tourneyorder$TeamID)

NCAAdetailedtourney = tourneyorder
head(NCAAdetailedtourney)



####################### CLEANIGN COMPACT RESULTS ##################################################
NCAATourneyCompactResults <- read.csv("~/Desktop/School/Spring 2019/Stat Learning/Final Project/mens-machine-learning-competition-2019/DataFiles/NCAATourneyCompactResults.csv")
tourneycompact = NCAATourneyCompactResults
head(tourneycompact)
tail(tourneycompact)
tourneycompact$gamenumber = rev(seq(1, nrow(tourneycompact),1))
colnames(tourneycompact)
tourneycompact = tourneycompact[,-7]
wincomp = tourneycompact[,c(1,2,3,4,7,8)]
losecomp = tourneycompact[,c(1,2,5,6,7,8)]

colnames(wincomp) = gsub('W', '',colnames(wincomp))
colnames(losecomp) = gsub('L', '',colnames(losecomp))
colnames(wincomp)
colnames(losecomp)

compactfresh = rbind(wincomp, losecomp)
gameorder = order(compactfresh$gamenumber)
compactordered = compactfresh[gameorder, ]
head(compactordered)
tail(compactordered)

compactordered$seedkey = paste(compactordered$Season, '-', compactordered$TeamID)

NCAAcompacttourney = compactordered
head(NCAAcompacttourney)

####################### Importing Seed Data ##################################################
NCAATourneySeeds <- read.csv("~/Desktop/School/Spring 2019/Stat Learning/Final Project/mens-machine-learning-competition-2019/DataFiles/NCAATourneySeeds.csv")
seeds = NCAATourneySeeds
dim(seeds)
head(seeds)

seeds$Seed = gsub('[[:alpha:]]', '', seeds$Seed)
seeds$Seed = as.numeric(seeds$Seed)
seeds$Seed
head(seeds)
seeds$seedkey = paste(seeds$Season, '-', seeds$TeamID)

head(seeds)
# Probably use match function to match TeamID and Year to impute the teams seed for each year

####################### Regular Season Compact Data ##################################################
RegularSeasonCompactResults <- read.csv("~/Desktop/School/Spring 2019/Stat Learning/Final Project/mens-machine-learning-competition-2019/DataFiles/RegularSeasonCompactResults.csv")
regcompact = RegularSeasonCompactResults
colnames(regcompact)
regcompact$gamenumber = rev(seq(1, nrow(regcompact),1))
colnames(regcompact)
regcompact = regcompact[,-7] # Remove, wont be used for tournament prediction
winregcomp = regcompact[,c(1,2,3,4,7,8)]
loseregcomp = regcompact[,c(1,2,5,6,7,8)]

colnames(winregcomp) = gsub('W', '',colnames(winregcomp))
colnames(loseregcomp) = gsub('L', '',colnames(loseregcomp))
colnames(winregcomp)
colnames(loseregcomp)

regcompactfresh = rbind(winregcomp, loseregcomp)
reggameorder = order(regcompactfresh$gamenumber)
regcompactordered = regcompactfresh[reggameorder, ]
head(regcompactordered)
tail(regcompactordered)

regcompact = regcompactordered
head(regcompact)

####################### Regular Season Full Data ##################################################
RegularSeasonDetailedResults <- read.csv("~/Desktop/School/Spring 2019/Stat Learning/Final Project/mens-machine-learning-competition-2019/DataFiles/RegularSeasonDetailedResults.csv")
regfull = RegularSeasonDetailedResults
colorder = sort(colnames(regfull))
regorder = regfull[, colorder]
colnames(regorder)
dim(regorder)
unique(regorder$WLoc) # can remove this, its all the same
which(colnames(regorder) == 'WLoc')
regorder = regorder[,-c(28)] 
colnames(regorder)
regorder$gamenumber = rev(seq(1,nrow(regorder),1))

winners = regorder[, c(1,17,18,19:33,34)]
colnames(winners)
losers = regorder[, c(1,17,18,2:16,34)]
colnames(losers)

colnames(winners) = gsub('W', '',colnames(winners))
colnames(losers) = gsub('L', '',colnames(losers))
colnames(winners)
colnames(losers) 
length(colnames(winners))
length(colnames(winners))
intersect(colnames(winners), colnames(losers))
length(intersect(colnames(winners), colnames(losers))) # all columns match for winners and losers

winners$win = rep(1, length(nrow(winners)))
losers$win = rep(0, length(nrow(losers)))
head(winners)
tail(winners)
head(losers)

regfresh = rbind(winners, losers)
head(regfresh)
tail(regfresh)
gameorder = order(regfresh$gamenumber)
regfinal = regfresh[gameorder,]
head(regfinal)
tail(regfinal)

regdetail = regfinal
head(regdetail)

####################### Team Number/Name Data ##################################################
Teams <- read.csv("~/Desktop/School/Spring 2019/Stat Learning/Final Project/mens-machine-learning-competition-2019/DataFiles/Teams.csv")

####################### Imputing Seed to detailed data and compact data ##################################################
head(NCAAdetailedtourney)
which(NCAAdetailedtourney$seedkey[1] == seeds$seedkey)
seeds$Seed[2151]
which(NCAAdetailedtourney$seedkey[2] == seeds$seedkey)
seeds$Seed[2204]

which(NCAAcompacttourney$seedkey[1] == seeds$seedkey)
seeds$Seed[2151]
which(NCAAcompacttourney$seedkey[2] == seeds$seedkey)
seeds$Seed[2204]

NCAAdetailedtourney$seed = rep(0, times = length(NCAAdetailedtourney$seedkey))
NCAAcompacttourney$seed = rep(0, times = length(NCAAcompacttourney$seedkey))

for(i in 1:length(NCAAdetailedtourney$seedkey)){
  detailedseedstorage = which(NCAAdetailedtourney$seedkey[i] == seeds$seedkey)
  NCAAdetailedtourney$seed[i] = seeds$Seed[detailedseedstorage]
}

for(i in 1:length(NCAAcompacttourney$seedkey)){
  compactseedstorage = which(NCAAcompacttourney$seedkey[i] == seeds$seedkey)
  NCAAcompacttourney$seed[i] = seeds$Seed[compactseedstorage]
}

head(NCAAdetailedtourney)
head(NCAAcompacttourney)

########################## INITIAL ANALYSIS ###############################################
# plot(win ~ . , data = NCAAdetailedtourney)
colnames(NCAAdetailedtourney)
str(NCAAdetailedtourney)
NCAAdetailedtourney = NCAAdetailedtourney[, -21] ## Remove seed key so the data is all numeric, plus we dont need it anymore
colnames(NCAAdetailedtourney)
str(NCAAdetailedtourney)

predictors = NCAAdetailedtourney[, c(20,2,19,4:16,18,21)]
cor(predictors)

##### To reduce multicollinearity
NCAAdetailedtourney$FG3perc = NCAAdetailedtourney$FGM3/NCAAdetailedtourney$FGA3
NCAAdetailedtourney$FGperc = NCAAdetailedtourney$FGM/NCAAdetailedtourney$FGA
NCAAdetailedtourney$FTperc =  NCAAdetailedtourney$FTM/NCAAdetailedtourney$FTA
colnames(NCAAdetailedtourney)
predictset1 = NCAAdetailedtourney[, c(20, 17, 19, 3, 1,2,4,5,6, 13:16, 18, 21:24)]
colnames(predictset1)
cor(predictset1)

##### Subset into train and test sets
full = predictset1
pre2018train = predictset1[predictset1$Season != 2018, ]
test2018 = predictset1[predictset1$Season == 2018, ]

head(pre2018train)
head(test2018)

##### Initial Model
logmodalldetail = glm(win ~ . - TeamID - DayNum - Season - gamenumber, data = pre2018train, family = 'binomial')
logmodalldetail

##### Predict and view results
mod.1.vars = test2018 ## Keep mod.1.vars so we can use the same set later
preds = predict(logmodalldetail, newdata = as.data.frame(mod.1.vars), type = 'response')
head(preds)

test2018$preds = preds
head(test2018)
tail(test2018)

test2018$winpredict = c(0, times = length(test2018$winpredict))
length(test2018$winpredict)/2

for(i in 1:(length(test2018$winpredict)/2)) {
  if(test2018$preds[(2*(i+1)-3)] > test2018$preds[(2*(i))]){
    test2018$winpredict[(2*(i+1)-3)] = 1
    test2018$winpredict[(2*(i))] = 0
  } else {
    test2018$winpredict[(2*(i+1)-3)] = 0
    test2018$winpredict[(2*(i))] = 1
  }
}

head(test2018)
tail(test2018)
colnames(test2018)

test2018$rightvec = rep(0, times = length(test2018$win))

for(i in 1:length(test2018$win)){
  if (test2018$win[i] == test2018$winpredict[i]){
    test2018$rightvec[i] = 1
  }
}

sum(test2018$rightvec)/nrow(test2018)
nrow(test2018)
sum(test2018$rightvec)/2
nrow(test2018)/2
sum(test2018$rightvec)/2
61/67

logmodalldetail

##### Finding season averages for tournament teams
head(regdetail)
regdetail$seedkey = paste(regdetail$Season, '-', regdetail$TeamID)
tail(seeds)
which(regdetail$seedkey[1] == seeds$seedkey)
which(regdetail$seedkey[2] == seeds$seedkey)

regdetail$seed = rep(0, times = nrow(regdetail))
which(regdetail$seedkey[1] == seeds$seedkey)
length(which(regdetail$seedkey[2] == seeds$seedkey))

for(i in 1:length(regdetail$seedkey)){
  if(length(which(regdetail$seedkey[i] == seeds$seedkey) != 0)){
    placeholder = which(regdetail$seedkey[i] == seeds$seedkey)
    regdetail$seed[i] = seeds$Seed[placeholder]
  }
}

head(regdetail)

regdetail2018stats = regdetail[regdetail$Season == 2018, ] 
head(regdetail2018stats)
regdetail2018stats = regdetail2018stats[regdetail2018stats$seed != 0,] 
head(regdetail2018stats)
colnames(regdetail2018stats)
regdetail2018use = regdetail2018stats[, c(17,2,4,5,6,13,14,15,16,18,22)]
regdetail2018use$FG3perc = regdetail2018stats$FGM3/regdetail2018stats$FGA3
regdetail2018use$FGperc = regdetail2018stats$FGM/regdetail2018stats$FGA
regdetail2018use$FTperc = regdetail2018stats$FTM/regdetail2018stats$FTA
colnames(regdetail2018use)
head(regdetail2018use)

fix = which(regdetail2018use$FTperc == 'NaN')
fix
teamnumfix = regdetail2018use$TeamID[fix]
teamnumfix
fixteam1 = regdetail2018use[regdetail2018use$TeamID == 1395,]
FTfix1 = mean(fixteam1$FTperc, na.rm = T)
fixteam2 = regdetail2018use[regdetail2018use$TeamID == 1438,]
FTfix2 = mean(fixteam2$FTperc, na.rm = T)


regdetail2018use$FTperc[806] = FTfix1
regdetail2018use$FTperc[806]
regdetail2018use$FTperc[1810] = FTfix2
regdetail2018use$FTperc[1810]
which(regdetail2018use$FTperc == 'NaN')


avg2018season = aggregate(regdetail2018use[, c(2:14)], list(regdetail2018use$TeamID), mean)
colnames(avg2018season)[1] = "TeamID"
regdetail2018use$seedkey = paste('2018', '-', regdetail2018use$TeamID)
head(avg2018season)
tail(avg2018season)

colnames(avg2018season) # need to make columns to match data frame for the predict funciton
colnames(pre2018train)

avg2018season$win = rep(0, times = nrow(avg2018season))
avg2018season$gamenumber = rep(0, times = nrow(avg2018season))
avg2018season$Season = rep(2018, times = nrow(avg2018season))
avg2018season$DayNum = rep(0, times = nrow(avg2018season))

avg2018season = avg2018season[, c(15,1,16,17,18,2:14)]
avg2018season[avg2018season$TeamID == 1438,]
head(avg2018season)
head(pre2018train)

##### Predict based on team season average
logmodalldetail
seasonavgpredict = predict(logmodalldetail, newdata = avg2018season, type = 'response')
mod1.predictions = cbind(seasonavgpredict, avg2018season$TeamID)    
colnames(mod1.predictions)[2] = "TeamID"
colnames(mod1.predictions)
mod1.predictions = as.data.frame(mod1.predictions)
head(mod1.predictions)
seeds[seeds$TeamID == 1438,]

##### compare new predictions to actual results
head(test2018)

rownumber = which(mod1.predictions$TeamID == test2018$TeamID[44]) # team from row 44 is found in the 68th row of our new preds
rownumber
mod1.predictions$seasonavgpredict[rownumber]

test2018$seasonavgpreds = rep(0, times = nrow(test2018))

for(i in 1:nrow(test2018)) {
  rownumber = which(mod1.predictions$TeamID == test2018$TeamID[i])
  probability = mod1.predictions$seasonavgpredict[rownumber]
  test2018$seasonavgpreds[i] = probability
}

head(test2018)
head(test2018[order(test2018$TeamID),])

test2018$mod.1.winpredict = rep(0, times = nrow(test2018))

for(i in 1:(length(test2018$seasonavgpreds)/2)) {
  if(test2018$seasonavgpreds[(2*(i+1)-3)] > test2018$seasonavgpreds[(2*(i))]){
    test2018$mod.1.winpredict[(2*(i+1)-3)] = 1
    test2018$mod.1.winpredict[(2*(i))] = 0
  } else {
    test2018$mod.1.winpredict[(2*(i+1)-3)] = 0
    test2018$mod.1.winpredict[(2*(i))] = 1
  }
}

test2018$mod.1.win = rep(0, times = nrow(test2018))

for(i in 1:nrow(test2018)){
  if(test2018$win[i] == test2018$mod.1.winpredict[i]) {
    test2018$mod.1.win[i] = 1
  }
}

dim(test2018)
sum(test2018$mod.1.win)/2
nrow(test2018)/2        

sum(test2018$mod.1.win)/nrow(test2018)  

################################ Tree Based Model ################################ 
library(tree)
treemod.1 = tree(win ~ . - TeamID - DayNum - Season - gamenumber, data = pre2018train)
treemod.1
summary(treemod.1)
plot(treemod.1)
text(treemod.1, pretty = 0)

rfmod.1 = randomForest(win ~ . - TeamID - DayNum - Season - gamenumber, data = pre2018train)
rfmod.1

treemod.2 = tree(win ~ . - TeamID - DayNum - Season - gamenumber, data = full)
treemod.2
summary(treemod.2)
plot(treemod.2)
text(treemod.2, pretty = 0)

rfmod.2 = randomForest(win ~ . - TeamID - DayNum - Season - gamenumber, data = full)
rfmod.2

tree.pred = predict(treemod.1, newdata = as.data.frame(avg2018season))
tree.pred

treedf = cbind(TeamID = avg2018season$TeamID, tree.pred)
treedf = as.data.frame(treedf)
head(treedf)

rownumber = which(treedf$TeamID == test2018$TeamID[44]) # team from row 44 is found in the 68th row of our new preds
rownumber
treedf$tree.pred[rownumber]

test2018$tree.pred = rep(0, times = nrow(test2018))

for(i in 1:nrow(test2018)) {
  rownumber = which(treedf$TeamID == test2018$TeamID[i])
  probability = treedf$tree.pred[rownumber]
  test2018$tree.pred[i] = probability
}

tail(test2018)
head(test2018[order(test2018$TeamID),])

test2018$tree.win.predict = rep(0, times = nrow(test2018))

for(i in 1:(length(test2018$tree.pred)/2)) {
  
  if(test2018$tree.pred[(2*(i+1)-3)] > test2018$tree.pred[(2*(i))]) {
    
    test2018$tree.win.predict[(2*(i+1)-3)] = 1
    test2018$tree.win.predict[(2*(i))] = 0
    
  } else if(test2018$tree.pred[(2*(i+1)-3)] < test2018$tree.pred[(2*(i))]) {
    
    test2018$tree.win.predict[(2*(i+1)-3)] = 0
    test2018$tree.win.predict[(2*(i))] = 1
    
  } else if(test2018$tree.pred[(2*(i+1)-3)] == test2018$tree.pred[(2*(i))]){
    
    if(test2018$Score[(2*(i+1)-3)] > test2018$Score[(2*(i))]){
      
      test2018$tree.win.predict[(2*(i+1)-3)] = 1
      test2018$tree.win.predict[(2*(i))] = 0
      
    } else if (test2018$Score[(2*(i+1)-3)] < test2018$Score[(2*(i))]) {
      
      test2018$tree.win.predict[(2*(i+1)-3)] = 0
      test2018$tree.win.predict[(2*(i))] = 1
      
    } else if(test2018$Score[(2*(i+1)-3)] == test2018$Score[(2*(i))]){
      
      if(test2018$DR[(2*(i+1)-3)] > test2018$DR[(2*(i))]) {
        
        test2018$tree.win.predict[(2*(i+1)-3)] = 1
        test2018$tree.win.predict[(2*(i))] = 0
        
      } else if (test2018$DR[(2*(i+1)-3)] == test2018$DR[(2*(i))]){
        
        test2018$tree.win.predict[(2*(i+1)-3)] = 0
        test2018$tree.win.predict[(2*(i))] = 1
        
      } else if(test2018$DR[(2*(i+1)-3)] == test2018$DR[(2*(i))]) {
        
        if(runif(1) > .5){
          
          test2018$tree.win.predict[(2*(i+1)-3)] = 1
          test2018$tree.win.predict[(2*(i))] = 0
          
        } else {
          
          test2018$tree.win.predict[(2*(i+1)-3)] = 0
          test2018$tree.win.predict[(2*(i))] = 1
          
        }
      }
    }
  }
}

test2018$tree.win = rep(0, times = nrow(test2018))

for(i in 1:nrow(test2018)){
  if(test2018$win[i] == test2018$tree.win.predict[i]) {
    test2018$tree.win[i] = 1
  }
}

sum(test2018$tree.win)/2
nrow(test2018)/2        

sum(test2018$tree.win)/nrow(test2018)  

colnames(pre2018train)

################################ Building a 2019 model from the data #########################


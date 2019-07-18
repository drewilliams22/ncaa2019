Prelim2019_MasseyOrdinals <- read.csv("~/Desktop/School/Spring 2019/Stat Learning/Final Project/mens-machine-learning-competition-2019/DataFiles/Prelim2019_MasseyOrdinals.csv")
teamrank = Prelim2019_MasseyOrdinals

Prelim2019_RegularSeasonDetailedResults <- read.csv("~/Desktop/School/Spring 2019/Stat Learning/Final Project/mens-machine-learning-competition-2019/DataFiles/Prelim2019_RegularSeasonDetailedResults.csv")
seasonstats = Prelim2019_RegularSeasonDetailedResults

NCAATourneyDetailedResults <- read.csv("~/Desktop/School/Spring 2019/Stat Learning/Final Project/mens-machine-learning-competition-2019/DataFiles/NCAATourneyDetailedResults.csv")
tourneydetail = NCAATourneyDetailedResults

NCAATourneySeeds <- read.csv("~/Desktop/School/Spring 2019/Stat Learning/Final Project/mens-machine-learning-competition-2019/DataFiles/NCAATourneySeeds.csv")
seeds = NCAATourneySeeds
seeds$seedkey = paste(seeds$Season, '-', seeds$TeamID)

RegularSeasonDetailedResults <- read.csv("~/Desktop/School/Spring 2019/Stat Learning/Final Project/mens-machine-learning-competition-2019/DataFiles/RegularSeasonDetailedResults.csv")
regdetail = RegularSeasonDetailedResults

TeamConferences <- read.csv("~/Desktop/School/Spring 2019/Stat Learning/Final Project/mens-machine-learning-competition-2019/DataFiles/TeamConferences.csv")
teamconferences = TeamConferences
teamconferences$seedkey = paste(teamconferences$Season, '-', teamconferences$TeamID)

Teams <- read.csv("~/Desktop/School/Spring 2019/Stat Learning/Final Project/mens-machine-learning-competition-2019/DataFiles/Teams.csv")
teams = Teams
teams$yearsd1 = teams$LastD1Season-teams$FirstD1Season


head(teamrank)
head(seasonstats)
head(tourneydetail)
head(seeds)
head(teamconferences)
head(teams)
head(regdetail)

##### FINDING THE RANK OF EACH TEAM GOING INTO THE TOURNEMENT (NOT SEED) ########################################
##### Also find rank at start of the season, and potentially mid season rank
#install.packages('sqldf')
require(sqldf)
teamrank$seedkey = paste(teamrank$Season, '-', teamrank$TeamID)

##### Finding season rank at start, 1st quartile, median, 3rd quartile, and end of season
endseasonrank = sqldf("SELECT Season, TeamID, max(RankingDayNum) AS endday, OrdinalRank AS endrank, seedkey
                      FROM teamrank
                      Where Season >= 2003
                      GROUP BY Season, TeamID;")

initialseasonrank = sqldf("SELECT Season, TeamID, min(RankingDayNum) AS startday, OrdinalRank as startrank, seedkey
                          FROM teamrank
                          Where Season >= 2003
                          GROUP BY Season, TeamID;")

Q1seasonrank = sqldf("SELECT Season, TeamID, RankingDayNum AS q1day , OrdinalRank as q1rank, seedkey
                     FROM teamrank
                     Where Season >= 2003 AND RankingDayNum = 64
                     GROUP BY Season, TeamID;")

Q2seasonrank = sqldf("SELECT Season, TeamID, RankingDayNum AS q2day, OrdinalRank as q2rank, seedkey
                     FROM teamrank
                     Where Season >= 2003 AND RankingDayNum = 91
                     GROUP BY Season, TeamID;")

Q3seasonrank = sqldf("SELECT Season, TeamID, RankingDayNum AS q3day, OrdinalRank as q3rank, seedkey
                     FROM teamrank
                     Where Season >= 2003 AND RankingDayNum = 114
                     GROUP BY Season, TeamID;")

seasonranks = sqldf("SELECT endseasonrank.Season, endseasonrank.TeamID, startrank, 
                    q1rank, q2rank, q3rank, endrank, endseasonrank.seedkey
                    FROM endseasonrank
                    LEFT JOIN initialseasonrank ON initialseasonrank.seedkey = endseasonrank.seedkey
                    LEFT JOIN Q1seasonrank ON Q1seasonrank.seedkey = endseasonrank.seedkey
                    LEFT JOIN Q2seasonrank ON Q2seasonrank.seedkey = endseasonrank.seedkey
                    LEFT JOIN Q3seasonrank ON Q3seasonrank.seedkey = endseasonrank.seedkey
                    ")

##### Fininding which conference each team played in
seasonrankcompiled = sqldf("SELECT seasonranks.*, teamconferences.ConfAbbrev
                           FROM seasonranks
                           LEFT JOIN teamconferences
                           ON teamconferences.seedkey = seasonranks.seedkey")
head(seasonrankcompiled)

##### Adding Wseed and Lseed to the tourney data
tourneydetail$Wseedkey = paste(tourneydetail$Season, '-', tourneydetail$WTeamID)
tourneydetail$Lseedkey = paste(tourneydetail$Season, '-', tourneydetail$LTeamID)

tourneydetail = sqldf("SELECT tourneydetail.*,  seeds.Seed as Wseed
                       FROM tourneydetail
                       LEFT JOIN seeds
                       ON seeds.seedkey = tourneydetail.Wseedkey")

 

tourneydetail = sqldf("SELECT tourneydetail.*,  seeds.Seed as Lseed
                       FROM tourneydetail
                       LEFT JOIN seeds
                       ON seeds.seedkey = tourneydetail.Lseedkey")

##### Making seed numeric
tourneydetail$Wseed = as.numeric(gsub('[[:alpha:]]', '', tourneydetail$Wseed ))
tourneydetail$Lseed = as.numeric(gsub('[[:alpha:]]', '', tourneydetail$Lseed))

##### REMOVING BUY IN GAMES (16 seed vs 16 seed)
head(tourneydetail)

storage = vector('numeric', nrow(tourneydetail))
for(i in 1:nrow(tourneydetail)){ 
  if((tourneydetail$Lseed[i] == 16 && tourneydetail$Wseed[i] == 16) |
     (tourneydetail$Lseed[i] == 14 && tourneydetail$Wseed[i] == 14) |
     (tourneydetail$Lseed[i] == 13 && tourneydetail$Wseed[i] == 13) |
     (tourneydetail$Lseed[i] == 12 && tourneydetail$Wseed[i] == 12) |
     (tourneydetail$Lseed[i] == 11 && tourneydetail$Wseed[i] == 11)){
    storage[i] = nrow(tourneydetail[i,])
  } else {
    storage[i] = 0
  }
} 
storage
nrow(tourneydetail) - length(which(storage == 1))

buyingames = which(storage == 1)
tourneydetail = tourneydetail[-buyingames, ]
dim(tourneydetail)

tourneydetail$Wwin = rep(1)
tourneydetail$Lwin = rep(0)
tourneydetail$Wgamenumber = seq(from = 1, to = nrow(tourneydetail), by = 1)
tourneydetail$Lgamenumber = seq(1, nrow(tourneydetail), by = 1)

##### Splitting into a winners and losers df
tourneydetail = tourneydetail[, order(colnames(tourneydetail))]
which(colnames(tourneydetail) == 'WLoc')
tourneydetail$roundnumber = rep(c(rep(1, 32), rep(2, 16), rep(3, 8), rep(4, 4), rep(5,2), rep(6,1)), times = 16)
tourneydetail = tourneydetail[, -33] # remove win location
colnames(tourneydetail)
ncol(tourneydetail)

winners = tourneydetail[, c(23:40 , ncol(tourneydetail))] # take out Wwin
losers = tourneydetail[, c(2:19,42)] # take out Lwin
dim(winners)
dim(losers)
colnames(winners) = gsub('W', '',colnames(winners))
colnames(losers) = gsub('L', '',colnames(losers))

##### Adding in shooting percentages
winners$FGperc = winners$FGM/winners$FGA
winners$FTperc = winners$FTM/winners$FTA
winners$FG3perc = winners$FGM3/winners$FGA3

losers$FGperc = losers$FGM/losers$FGA
losers$FTperc = losers$FTM/losers$FTA
losers$FG3perc = losers$FGM3/losers$FGA3

##### Adding in regular season rank and conference
winners = sqldf("SELECT winners.*, seasonrankcompiled.startrank, seasonrankcompiled.q1rank, seasonrankcompiled.q2rank,
                 seasonrankcompiled.q3rank, seasonrankcompiled.endrank, seasonrankcompiled.ConfAbbrev
                 FROM winners
                 LEFT JOIN seasonrankcompiled
                 ON winners.seedkey = seasonrankcompiled.seedkey")
losers = sqldf("SELECT losers.*, seasonrankcompiled.startrank, seasonrankcompiled.q1rank, seasonrankcompiled.q2rank,
                seasonrankcompiled.q3rank, seasonrankcompiled.endrank, seasonrankcompiled.ConfAbbrev
                FROM losers
                LEFT JOIN seasonrankcompiled
                ON losers.seedkey = seasonrankcompiled.seedkey")

which(is.na(winners$startrank) == 1)
which(is.na(winners$q1rank) == 1)
which(is.na(winners$q2rank) == 1)
which(is.na(winners$q3rank) == 1)
which(is.na(winners$endrank) == 1)

##### Get rid of q1 and q2 rank, too many missing values, imputation would introduce too much bias
##### Making conference numeric
require(fastDummies)

winners = dummy_cols(winners, "ConfAbbrev")
losers = dummy_cols(losers, "ConfAbbrev")
### take care of missing columns in winners
sort(gsub('ConfAbbrev_', '',colnames(winners)[26:ncol(winners)]))
sort(gsub('ConfAbbrev_', '',colnames(losers)[26:ncol(losers)]))
winners$ConfAbbrev_mid_cont = rep(0)
winners$ConfAbbrev_nec = rep(0)
winners$ConfAbbrev_swac = rep(0)

winsortcol = sort(colnames(winners))
losesortcol = sort(colnames(losers))

winners = winners[, winsortcol]
losers = losers[, losesortcol]

##### Removing non numeric data (Besides seedkey) and q1 and q2 rank data 
colnames(winners)
winners = winners[, -c(52,53,3,7)] #7 is acc, which will be our comparison conf
losers = losers[, -c(52,53,3,7)]

##### Checking how many games have been played by teams of the same seed
length(which(winners$seed == losers$seed)) ## 13 games where the seed difference is 0, take them out later
colnames(winners)
head(winners)

##### So, in the seeddiff df underdogwin is a 1 if the underdog won, 0 if not, the rest is pretty self explanitory

##### Preping input data (Season averages) and test and training data sets
head(seasonstats)
head(seasonrankcompiled)
head(seeds)
head(tourneydetail)
head(winners)
head(losers)
#head(seeddiff)

##### Taking season stats and splitting it by team
seasonstats = seasonstats[, order(colnames(seasonstats))]
colnames(seasonstats)
seasonstats = seasonstats[, -c(1, 17, 28)] 
seasonwin = seasonstats[, c(16, 17:ncol(seasonstats))]
seasonlose = seasonstats[, c(16, 1:15)] 

##### Making name uniform
colnames(seasonwin) = gsub('W', '',colnames(seasonwin))
colnames(seasonlose) = gsub('L', '',colnames(seasonlose))

##### Adding some features
seasonwin$FGperc = seasonwin$FGM/seasonwin$FGA
seasonwin$FTperc = seasonwin$FTM/seasonwin$FTA
seasonwin$FG3perc = seasonwin$FGM3/seasonwin$FGA3

seasonlose$FGperc = seasonlose$FGM/seasonlose$FGA
seasonlose$FTperc = seasonlose$FTM/seasonlose$FTA
seasonlose$FG3perc = seasonlose$FGM3/seasonlose$FGA3

##### Joining back together
seasonrejoin = rbind(seasonwin, seasonlose) 
colnames(seasonrejoin)
colnames(seasonrejoin)[11] = "OReb" ### AVOID OR IN SQL STATEMENTS
colnames(seasonrejoin)[16] = "TOut" ### AVOID TO IN SQL STATEMENTS (ACTUALLY TURN OVERS BUT IT DOESNT MATTER)
columnnames = colnames(seasonrejoin)
columnnames

##### Taking averages
seasonavgs = sqldf("SELECT Season, AVG(Ast), AVG(BLK), AVG(DR), AVG(FGA), AVG(FGA3), AVG(FGM), AVG(FGM3), AVG(FTA),  
                     AVG(FTM), AVG(OReb), AVG(PF),  AVG(Score), AVG(Stl), TeamID,  AVG(TOut), AVG(FGperc),
                     AVG(FTperc), AVG(FG3perc)
                     FROM seasonrejoin
                     GROUP BY TeamID, Season
                     ORDER BY TeamID, Season")

colnames(seasonavgs) = columnnames

##### Seed key
seasonavgs$seedkey = paste(seasonavgs$Season, '-', seasonavgs$TeamID)

##### Numeric Seed
seeds$Seed = as.numeric(gsub('[[:alpha:]]', '', seeds$Seed))

alldata = sqldf("SELECT seasonavgs.*, seeds.Seed, seasonrankcompiled.startrank, seasonrankcompiled.q3rank, 
                 seasonrankcompiled.endrank, seasonrankcompiled.ConfAbbrev
                 FROM seasonavgs
                 LEFT JOIN seeds
                 ON seasonavgs.seedkey = seeds.seedkey
                 LEFT JOIN seasonrankcompiled
                 ON seasonavgs.seedkey = seasonrankcompiled.seedkey")

##### Only teams that made the tournement
alldata = alldata[is.na(alldata$Seed) == 0 ,]
head(alldata)

# MAKE DUMMY VARIABLES
alldata = dummy_cols(alldata, select_columns = "ConfAbbrev")
colnames(alldata)
alldata = alldata[, -which(colnames(alldata) == "ConfAbbrev_acc")]
##### Remove acc dummy to avoid multicollinearity

##### Dropping columns that wont be used in prediction
which(colnames(alldata) == 'ConfAbbrev')
alldata = alldata[, -which(colnames(alldata) == 'ConfAbbrev')]
alldata = alldata[,order(colnames(alldata))]

alldata = alldata[, order(colnames(alldata))]
alldata = alldata[alldata$Season >= 2003,]

##### Reneaming alldata headers to then join back into winners and losers dfs
colnames(alldata) <- paste("SeasonStats", colnames(alldata), sep = "_")
colnames(alldata)

winnersandseason = sqldf("SELECT winners.*, alldata.*
                           From winners
                           LEFT JOIN alldata
                           ON winners.seedkey = alldata.SeasonStats_seedkey")

losersandseason = sqldf("SELECT losers.*, alldata.*
                           From losers
                           LEFT JOIN alldata
                           ON losers.seedkey = alldata.SeasonStats_seedkey")

colnames(winnersandseason)
colnames(losersandseason)
which(colnames(winnersandseason) == colnames(losersandseason))

underdogwin = NA
for(i in 1:nrow(winnersandseason)){
  if(winnersandseason$seed[i] > losersandseason$seed[i]){
    underdogwin[i] = 1
  } else {
    underdogwin[i] = 0
  }
}
underdogwin

seeddiff = winnersandseason$seed - losersandseason$seed
##################################### MODEL 2 BASED ON SEASON AVERAGE DATA #####################################
colnames(winnersandseason)
mod2diffseason = winnersandseason[, c(59:108,112,113,115)] - losersandseason[, c(59:108,112,113,115)]

mod2diffseason$seeddiff = seeddiff
for(i in 1:nrow(mod2diffseason)){
  if(mod2diffseason$seeddiff[i] < 0){
    mod2diffseason[i, ] = -mod2diffseason[i, ] ### if seeddiff is positive it means underdog won
    # Thus, to give us the format underdog - favorite, we need to flip the dataframe
    # so it is of the form underdog - favorite instead of winner - loser
  }
}

for(i in 1:nrow(mod2diffseason)){
  mod2diffseason$favoriteseed[i] = min(winnersandseason$seed[i], losersandseason$seed[i]) 
  mod2diffseason$underdogseed[i] = max(winnersandseason$seed[i], losersandseason$seed[i])
}

colnames(winnersandseason)
mod2diffseason$round = winnersandseason[, 51]
mod2diffseason$Season = winnersandseason$SeasonStats_Season
mod2diffseason$underdogwin = underdogwin

colnames(mod2diffseason)

##### REMOVING WHEN THE MATCHUP IS SAME SEED
storage = vector('numeric', nrow(mod2diffseason))
for(i in 1:nrow(mod2diffseason)){
  if(mod2diffseason$favoriteseed[i] == mod2diffseason$underdogseed[i]){
    storage[i] = 1
  } else {
    storage[i] = 0
  }
}
which(storage == 1)
sameseed = which(storage == 1)
mod2diffseason = mod2diffseason[-sameseed,]
underdogwin = underdogwin[-sameseed]
length(underdogwin)
mod2diffseason$underdogwin = underdogwin

head(mod2diffseason)

##### Fixing columns and removing some
mod2train17 = mod2diffseason[mod2diffseason$Season < 2017, -which(colnames(mod2diffseason) == "Season")] # subsetting and removing season
mod2train18 = mod2diffseason[mod2diffseason$Season < 2018, -which(colnames(mod2diffseason) == "Season")]
mod2train19 = mod2diffseason[mod2diffseason$Season < 2019, -which(colnames(mod2diffseason) == "Season")]
colnames(mod2train17)

colnames(mod2train17) = gsub("SeasonStats_", "", colnames(mod2train17)) # removing seasonstats header
colnames(mod2train18) = gsub("SeasonStats_", "", colnames(mod2train18))
colnames(mod2train19) = gsub("SeasonStats_", "", colnames(mod2train19))
colnames(mod2train17) = gsub("ConfAbbrev_", "", colnames(mod2train17))
colnames(mod2train18) = gsub("ConfAbbrev_", "", colnames(mod2train18))
colnames(mod2train19) = gsub("ConfAbbrev_", "", colnames(mod2train19))
colnames(mod2train17)

which(colnames(mod2train17) == "OReb") # renaming to avoid predict errors
which(colnames(mod2train17) == "TOut")
colnames(mod2train17)[47] = "OR"
colnames(mod2train17)[53] = "TO"
colnames(mod2train18)[47] = "OR"
colnames(mod2train18)[53] = "TO"
colnames(mod2train19)[47] = "OR"
colnames(mod2train19)[53] = "TO"

colnames(mod2diffseason)
test17 = mod2diffseason[mod2diffseason$Season == 2017, -58] # subsetting and removing season
test18 = mod2diffseason[mod2diffseason$Season == 2018, -58]
test19 = mod2diffseason[mod2diffseason$Season == 2019, -58]
colnames(test17)

colnames(test17) = gsub("SeasonStats_", "", colnames(test17)) # removing header
colnames(test18) = gsub("SeasonStats_", "", colnames(test18))
colnames(test19) = gsub("SeasonStats_", "", colnames(test19))
colnames(test17) = gsub("ConfAbbrev_", "", colnames(test17))
colnames(test18) = gsub("ConfAbbrev_", "", colnames(test18))
colnames(test19) = gsub("ConfAbbrev_", "", colnames(test19))
colnames(test17)

which(colnames(test17) == "OReb") # renaming and removing unwanted columns
which(colnames(test17) == "TOut")
which(colnames(test17) == "underdogwin")

colnames(test17)[47] = "OR"
colnames(test17)[53] = "TO"
test17 = test17[,-58]
colnames(test18)[47] = "OR"
colnames(test18)[53] = "TO"
test18 = test18[,-58]
colnames(test19)[47] = "OR"
colnames(test19)[53] = "TO"
test19 = test19[,-58]

head(mod2diffseason)
colnames(mod2diffseason)
upsets17 = mod2diffseason$underdogwin[mod2diffseason$Season == 2017] # to double check predictions
upsets18 = mod2diffseason$underdogwin[mod2diffseason$Season == 2018]

upsets17
upsets18
#View(mod2diffseason)

head(mod2train18)
########################### MODELS ###########################
#####
'''
Building a few different models. For each year (2017, 2018, 2019) I will construct 4 models:
A Tree
A Random Forest
A Logistic Regression
A SVM

'''
##### TREE
require(tree)
colnames(mod2train17)
set.seed(22)

mod2tree17 = tree(underdogwin ~ ., data = mod2train17)
mod2tree18 = tree(underdogwin ~ ., data = mod2train18)

predsmod2tree17 = predict(mod2tree17, newdata = test17)
predsmod2tree18 = predict(mod2tree18, newdata = test18)

par(mfrow = c(1,1))
require(pROC)
plot.roc(upsets17, predsmod2tree17, print.thres = T, print.auc = T)
plot.roc(upsets18, predsmod2tree18, print.thres = T, print.auc = T)

plot(mod2tree17)
text(mod2tree17, pretty = 0)

# Most upsets ever in a tourney was 13 upsets total, look for about 8 predicted upsets (avg since 2003)
mod2roc17 = predsmod2tree17
mod2roc17[mod2roc17 >= .5] = 1 
mod2roc17[mod2roc17 < .5] = 0 

mod2roc172 = predsmod2tree17
mod2roc172[mod2roc172 >= 1-.292] = 1 
mod2roc172[mod2roc172 < 1-.292] = 0

table(truth = upsets17, preds = mod2roc17)
table(truth = upsets17, preds = mod2roc172)

mod2roc18 = predsmod2tree18
mod2roc18[mod2roc18 >= .5] = 1 
mod2roc18[mod2roc18 < .5] = 0

mod2roc182 = predsmod2tree18
mod2roc182[mod2roc182 >= .667] = 1 
mod2roc182[mod2roc182 < .667] = 0

mod2roc183 = predsmod2tree18
mod2roc183[mod2roc183 >= .7] = 1
mod2roc183[mod2roc183 < .7] = 0

table(truth = upsets18, preds = mod2roc18)
table(truth = upsets18, preds = mod2roc182)
table(truth = upsets18, preds = mod2roc183)

############### RANDOM FOREST ###############
#install.packages("randomForest")
require(randomForest)
set.seed(22)
mod2forest17 = randomForest(underdogwin ~ ., data = mod2train17)
mod2forest18 = randomForest(underdogwin ~ ., data = mod2train18)

par(mfrow = c(1,2))
plot(mod2forest17, main = "MSE 2017 RF Model")
plot(mod2forest18, main = "MSE 2018 RF Model")

mod2preds17 = predict(mod2forest17, newdata = test17)
mod2preds18 = predict(mod2forest18, newdata = test18)

sum(mod2forest17$mse)
sum(mod2forest18$mse)

plot.roc(upsets17, mod2preds17, print.thres = T, print.auc = T)
plot.roc(upsets18, mod2preds18, print.thres = T, print.auc = T)

mod2roc17 = mod2preds17
mod2roc17[mod2roc17 >= .708] = 1 
mod2roc17[mod2roc17 < .708] = 0 

mod2roc172 = mod2preds17
mod2roc172[mod2roc172 >= 1-.355] = 1 
mod2roc172[mod2roc172 < 1-.355] = 0

mod2roc173 = mod2preds17
mod2roc173[mod2roc172 >= .5] = 1 
mod2roc173[mod2roc172 < .5] = 0

table(truth = upsets17, preds = mod2roc17)
table(truth = upsets17, preds = mod2roc172)
table(truth = upsets17, preds = mod2roc173)

mod2roc18 = mod2preds18
mod2roc18[mod2roc18 >= .7] = 1 
mod2roc18[mod2roc18 < .7] = 0

mod2roc182 = mod2preds18
mod2roc182[mod2roc182 >= 1-.595] = 1 
mod2roc182[mod2roc182 < 1-.595] = 0

mod2roc183 = mod2preds18
mod2roc183[mod2roc183 >= .5] = 1
mod2roc183[mod2roc183 < .5] = 0

table(truth = upsets18, preds = mod2roc18)
table(truth = upsets18, preds = mod2roc182)
table(truth = upsets18, preds = mod2roc183)


################################ SVM ################################
#install.packages("e1071")
require(e1071)
set.seed(22)
##### need to do cross validation to find a good cost function
tune.out = tune(svm, underdogwin ~ ., data = mod2train17,
                ranges=list(cost=seq(0.01,5,length.out=100)))
fit.svmr <- tune.out$best.model
fit.svmr
mod2svm17 = svm(underdogwin ~ ., data = mod2train17, kernal = "radial",
                cost = fit.svmr$cost, gamma = fit.svmr$gamma, epsilon = fit.svmr$epsilon)

tune.out = tune(svm, underdogwin ~ ., data = mod2train18,
                ranges=list(cost=seq(0.01,5,length.out=100)))
fit.svmr <- tune.out$best.model
fit.svmr
mod2svm18 = svm(underdogwin ~ ., data = mod2train18, kernal = "radial",
                cost = fit.svmr$cost, gamma = fit.svmr$gamma, epsilon = fit.svmr$epsilon)]

sum(mod2svm17$residuals^2)
sum(mod2svm18$residuals^2)


predsmod2svm17 = predict(mod2svm17, newdata = test17)
predsmod2svm18 = predict(mod2svm18, newdata = test18)

plot.roc(upsets17, predsmod2svm17, print.thres = T) # .558 
plot.roc(upsets18, predsmod2svm18, print.thres = T) # .569

mod2roc17 = predsmod2svm17
mod2roc17[mod2roc17 >= .5] = 1 
mod2roc17[mod2roc17 < .5] = 0 

mod2roc172 = predsmod2svm18
mod2roc172[mod2roc172 >= 1- .708] = 1 
mod2roc172[mod2roc172 < 1- .708] = 0

table(truth = upsets17, preds = mod2roc17)
table(truth = upsets17, preds = mod2roc172)


mod2roc18 = predsmod2svm18
mod2roc18[mod2roc18 >= .5] = 1 
mod2roc18[mod2roc18 < .5] = 0

mod2roc182 = predsmod2svm18
mod2roc182[mod2roc182 >= 1-.690] = 1 
mod2roc182[mod2roc182 < 1-.690] = 0

table(truth = upsets18, preds = mod2roc18)
table(truth = upsets18, preds = mod2roc182)

################################ Logistic ################################
mod2log17 = glm(underdogwin ~ ., data = mod2train17, family = 'binomial')
mod2log18 = glm(underdogwin ~ ., data = mod2train18, family = 'binomial')

mod2log17
colnames(mod2train17)
predsmod2log17 = predict(mod2log17, newdata = test17, type = 'response')
predsmod2log18 = predict(mod2log18, newdata = test18, type = 'response')

plot.roc(upsets17, predsmod2log17, print.thres = T, print.auc = T) 
plot.roc(upsets18, predsmod2log18, print.thres = T, print.auc = T) 

mod2roc17 = predsmod2log17
mod2roc17[mod2roc17 >= .5] = 1 
mod2roc17[mod2roc17 < .5] = 0 

mod2roc172 = predsmod2log18
mod2roc172[mod2roc172 >= 1-.417] = 1 
mod2roc172[mod2roc172 < 1-.417] = 0

table(truth = upsets17, preds = mod2roc17)
table(truth = upsets17, preds = mod2roc172)

mod2roc18 = predsmod2log18
mod2roc18[mod2roc18 >= .5] = 1 
mod2roc18[mod2roc18 < .5] = 0

mod2roc182 = predsmod2log18
mod2roc182[mod2roc182 >= .4] = 1 
mod2roc182[mod2roc182 < .4] = 0

table(truth = upsets18, preds = mod2roc18)
table(truth = upsets18, preds = mod2roc182)

################################ Neural Net ################################
require(neuralnet)
neuralnet

nn17 = neuralnet(underdogwin ~ ., data = mod2train17, hidden = 3)
plot(nn17)

##### Setting up first round for 2017, 2018, 2019 to fill out a bracket:
head(winnersandseason)
head(losersandseason)
head(NCAATourneySeeds)
seeds = NCAATourneySeeds
seeds$seedkey = paste(seeds$Season, '-', seeds$TeamID)
tail(seeds)

seeds17 = seeds[seeds$Season == 2017,]
seeds18 = seeds[seeds$Season == 2018,]

seeds17
colnames(winnersandseason)
colnames(mod2diffseason)

keepcolumns = intersect(colnames(winnersandseason), colnames(mod2diffseason))
keep = NA
for(i in 1:length(keepcolumns)){
  keep[i] = which(colnames(winnersandseason) == keepcolumns[i])
}
keep


winseasonavg = winnersandseason[, c(54, 109, keep)]
winseasonavg = winseasonavg[winseasonavg$SeasonStats_Season >= 2017,]
loseseasonavg = losersandseason[, c(54, 109, keep)]
loseseasonavg = loseseasonavg[loseseasonavg$SeasonStats_Season >= 2017,]
head(winseasonavg)

winseason17 = winseasonavg[winseasonavg$SeasonStats_Season == 2017,]
winseason18 = winseasonavg[winseasonavg$SeasonStats_Season == 2018,]
winseason19 = winseasonavg[winseasonavg$SeasonStats_Season == 2019,]

loseseason17 = loseseasonavg[loseseasonavg$SeasonStats_Season == 2017,]
loseseason18 = loseseasonavg[loseseasonavg$SeasonStats_Season == 2018,]
loseseason19 = loseseasonavg[loseseasonavg$SeasonStats_Season == 2019,]
dim(seeds17)

round1ws17 = sqldf("SELECT distinct seeds17.*, winseason17.*
                  FROM seeds17
                  INNER JOIN winseason17
                  ON seeds17.seedkey = winseason17.seedkey")
round1ls17 = sqldf("SELECT distinct seeds17.*, loseseason17.*
                  FROM seeds17
                  INNER JOIN loseseason17
                  ON seeds17.seedkey = loseseason17.seedkey")

seedsfull17 = sqldf("SELECT * FROM round1ws17
                     UNION 
                     SELECT * FROM round1ls17
                     ORDER BY round1ws17.Seed")

round1ws18 = sqldf("SELECT distinct seeds18.*, winseason18.*
                  FROM seeds18
                  INNER JOIN winseason18
                  ON seeds18.seedkey = winseason18.seedkey")
round1ls18 = sqldf("SELECT distinct seeds18.*, loseseason18.*
                  FROM seeds18
                  INNER JOIN loseseason18
                  ON seeds18.seedkey = loseseason18.seedkey")

seedsfull18 = sqldf("SELECT * FROM round1ws18
                     UNION 
                     SELECT * FROM round1ls18
                     ORDER BY round1ws18.Seed")

#View(seedsfull17)
#View(seedsfull18)
require(stringr)

seedsfull17$Seed = substr(seedsfull17$Seed, 1, 3)
seedsfull18$Seed = substr(seedsfull18$Seed, 1, 3)

round117setup = c('W01','W16','W02','W15','W03','W14','W04','W13',
                  'W05','W12','W06','W11','W07','W10','W08','W09',
                  'X01','X16','X02','X15','X03','X14','X04','X13',
                  'X05','X12','X06','X11','X07','X10','X08','X09',
                  'Y01','Y16','Y02','Y15','Y03','Y14','Y04','Y13',
                  'Y05','Y12','Y06','Y11','Y07','Y10','Y08','Y09',
                  'Z01','Z16','Z02','Z15','Z03','Z14','Z04','Z13',
                  'Z05','Z12','Z06','Z11','Z07','Z10','Z08','Z09'
                  )
round117setup = as.data.frame(round117setup)
colnames(round117setup) = 'Seed'
round117setup
head(seedsfull17)

round118setup = round117setup

round117 = sqldf("SELECT round117setup.*, seedsfull17.*
                 FROM round117setup
                 LEFT JOIN seedsfull17
                 ON round117setup.Seed = seedsfull17.Seed")

round118 = sqldf("SELECT round118setup.*, seedsfull18.*
                 FROM round118setup
                 LEFT JOIN seedsfull18
                 ON round118setup.Seed = seedsfull18.Seed")
colnames(round117)

colnames(round117) = gsub("SeasonStats_", "", colnames(round117))
colnames(round117) = gsub("ConfAbbrev_", "", colnames(round117))
colnames(round118) = gsub("SeasonStats_", "", colnames(round118))
colnames(round118) = gsub("ConfAbbrev_", "", colnames(round118))

colnames(round117)[54] = "OR"
colnames(round117)[60] = "TO"

colnames(round118)[54] = "OR"
colnames(round118)[60] = "TO"

colnames(round117)
colnames(mod2train17)





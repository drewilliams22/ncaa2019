## Stat Learning Final Project
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
seq(1,nrow(tourney),1)
tourney$gamenumber = seq(1,nrow(tourney),1)

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

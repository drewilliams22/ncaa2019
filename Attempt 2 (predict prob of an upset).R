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

##### MAKING A WINNING AND LOSING TEAM DF's TO CALCULATE AVG AND DIFF LATER #####################################
##### Need to do so for seasonstats and tourneydetail, and then put reg season rank in, as well as seed and stuff
colnames(tourneydetail)
colnames(seasonstats)
colkeep = intersect(colnames(tourneydetail), colnames(seasonstats))
colkeep = colkeep[-7]
colkeep
sortedcolumns = sort(colkeep)
sortedcolumns

tourneydetail = tourneydetail[, sortedcolumns]
seasonstats = seasonstats[, sortedcolumns]

tourneywinners = tourneydetail[, c(1, 17, 18, 19:33)]
seasonwinners = seasonstats[, c(1, 17, 18, 19:33)]

tourneylosers = tourneydetail[, c(1, 17, 18, 2:16 )]
seasonlosers = seasonstats[, c(1, 17, 18, 2:16 )]

##### Making all headers the same:
colnames(tourneywinners) = gsub('W', '',colnames(tourneywinners))
colnames(seasonwinners) = gsub('W', '',colnames(seasonwinners))

colnames(tourneylosers) = gsub('L', '',colnames(tourneylosers))
colnames(seasonlosers) = gsub('L', '',colnames(seasonlosers))
colnames(tourneywinners)

##### Computing shooting percentages
tourneywinners$FGperc = tourneywinners$FGM/tourneywinners$FGA
tourneywinners$FGTperc = tourneywinners$FTM/tourneywinners$FTA
tourneywinners$FG3perc = tourneywinners$FGM3/tourneywinners$FGA3

tourneylosers$FGperc = tourneylosers$FGM/tourneylosers$FGA
tourneylosers$FGTperc = tourneylosers$FTM/tourneylosers$FTA
tourneylosers$FG3perc = tourneylosers$FGM3/tourneylosers$FGA3

seasonwinners$FGperc = seasonwinners$FGM/seasonwinners$FGA
seasonwinners$FGTperc = seasonwinners$FTM/seasonwinners$FTA
seasonwinners$FG3perc = seasonwinners$FGM3/seasonwinners$FGA3

seasonlosers$FGperc = seasonlosers$FGM/seasonlosers$FGA
seasonlosers$FGTperc = seasonlosers$FTM/seasonlosers$FTA
seasonlosers$FG3perc = seasonlosers$FGM3/seasonlosers$FGA3

##### Creating Seed key to join on
seasonwinners$seedkey = paste(seasonwinners$Season, '-', seasonwinners$TeamID)
tourneywinners$seedkey = paste(tourneywinners$Season, '-', tourneywinners$TeamID)

seasonlosers$seedkey = paste(seasonlosers$Season, '-', seasonlosers$TeamID)
tourneylosers$seedkey = paste(tourneylosers$Season, '-', tourneylosers$TeamID)

head(seasonwinners)
head(seasonlosers)
head(tourneywinners)
head(tourneylosers)

##### Put tournament seed, season rank, conferance, and years as NCAA team into tourney data (this one is hard), 
##### also put zero vectors in the regular
##### season data to avoid problems with dataframes in R down the road
head(seasonrankcompiled)
head(seasonwinners)
tourneywinners = sqldf("SELECT tourneywinners.*, startrank, q1rank, q2rank, q3rank, endrank, ConfAbbrev
                        FROM tourneywinners
                        LEFT JOIN seasonrankcompiled
                        ON tourneywinners.seedkey = seasonrankcompiled.seedkey")

tourneylosers = sqldf("SELECT tourneylosers.*, startrank, q1rank, q2rank, q3rank, endrank, ConfAbbrev
                       FROM tourneylosers
                       LEFT JOIN seasonrankcompiled
                       ON tourneylosers.seedkey = seasonrankcompiled.seedkey")

##### Adding Tournament Seed to DFs
head(seeds)
tourneywinners = sqldf("SELECT tourneywinners.*, Seed
                        FROM tourneywinners
                        LEFT JOIN seeds
                        ON tourneywinners.seedkey = seeds.seedkey")

tourneylosers = sqldf("SELECT tourneylosers.*, Seed
                       FROM tourneylosers
                       LEFT JOIN seeds
                       ON tourneylosers.seedkey = seeds.seedkey")

##### Removing seedkey column to avoid computation errors
which(colnames(tourneywinners) == 'seedkey')

tourneywinners = tourneywinners[, -16]
tourneylosers = tourneylosers[, -16]

head(tourneywinners)
head(tourneylosers)

##### Checking for NA's
as.numeric(apply(tourneywinners, 2, function(x) any(is.na(x))))
apply(tourneywinners, 2, function(x) any(is.na(x))) ### Check q1rank and q2rank

as.numeric(apply(tourneylosers, 2, function(x) any(is.na(x))))
apply(tourneylosers, 2, function(x) any(is.na(x))) ### Check q1rank and q2rank

checkq1 = which(is.na(tourneywinners$q1rank) == 1)
checkq2 = which(is.na(tourneywinners$q2rank) == 1)

length(checkq1)
nrow(tourneywinners)
nrow()

tourneywinners[checkq1,]
tourneywinners[checkq2,]

##### Not sure what is going on, just take out the two columsn for q1 and q2 rank
which(colnames(tourneywinners) == "q1rank")
which(colnames(tourneywinners) == "q2rank")
tourneywinners = tourneywinners[, -c(17,18)]
tourneylosers = tourneylosers[, -c(17,18)]

##### Trimming seed to be a number
tourneywinners$Seed = as.numeric(gsub('[[:alpha:]]', '', tourneywinners$Seed))
tourneylosers$Seed = as.numeric(gsub('[[:alpha:]]', '', tourneylosers$Seed))
head(tourneywinners)
head(tourneylosers)

##### Adding in conference
tourneywinners$confnumeric = rep(0)

for(i in 1:nrow(tourneywinners)){
  if(tourneywinners$ConfAbbrev[i] == "acc" | tourneywinners$ConfAbbrev[i] == "sec" | tourneywinners$ConfAbbrev[i] == "big_ten" | tourneywinners$ConfAbbrev[i] == "big_east" | tourneywinners$ConfAbbrev[i] == "big_twelve" | tourneywinners$ConfAbbrev[i] == "pac_twelve"){
    tourneywinners$confnumeric[i] = 1
  } else {
    tourneywinners$confnumeric[i] = 0
  }
}

##### Basically, assign a one if the team is in the above "Historically good" divisions, else, zero
tourneylosers$confnumeric = rep(0)

for(i in 1:nrow(tourneylosers)){
  if(tourneylosers$ConfAbbrev[i] == "acc" | tourneylosers$ConfAbbrev[i] == "sec" | tourneylosers$ConfAbbrev[i] == "big_ten" | tourneylosers$ConfAbbrev[i] == "big_east" | tourneylosers$ConfAbbrev[i] == "big_twelve" | tourneylosers$ConfAbbrev[i] == "pac_twelve"){
    tourneylosers$confnumeric[i] = 1
  } else {
    tourneylosers$confnumeric[i] = 0
  }
}

upsetvec = rep(0, times = nrow(tourneywinners))

for(i in 1:length(upsetvec)) {
  if(tourneywinners$Seed[i] > tourneylosers$Seed[i]){
    upsetvec[i] = 1
  } else {
    upsetvec[i] = 0
  }
} 
head(upsetvec)

##### Taking the difference for one DF, and combing them for another
tourneycombined = as.data.frame(rbind(tourneywinners, tourneylosers))
tourneycombined$win = rep(c(1,0))
tourneycombined$upset = rep(0)

for(i in 1:(length(tourneycombined$upset)/2)) {
  if(tourneycombined$Seed[(2*(i+1)-3)] > tourneycombined$Seed[(2*(i))]){
    tourneycombined$upset[(2*(i+1)-3)] = 1
    tourneycombined$upset[(2*(i))] = -1
  } else {
    tourneycombined$upset[(2*(i+1)-3)] = 0
    tourneycombined$upset[(2*(i))] = 0
  }
}

head(tourneycombined)

which(colnames(tourneywinners) == 'ConfAbbrev')
tourneydiff = tourneywinners[, c(4:18, 20, 21)] - tourneylosers[, c(4:18, 20, 21)]
tourneydiff = as.data.frame(tourneydiff)
tourneydiff$confnumeric = 
tourneydiff$upsetvec = upsetvec
tourneydiff$Season = tourneywinners$season

head(tourneydiff)
head(tourneywinners)

##### Make training and test set.
##### Potentially make 14 or 15 regressions and then make an average of them
train = tounreydiff[]

logmod.1 = glm(upsetvec ~ ., data = tourneydiff, family = 'binomial')
logmod.1
summary(logmod.1)

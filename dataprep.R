playoffs = read.csv("data/EM2004.2012.publ.txt", header=TRUE, sep="\t")

## Change attributes to factors
playoffs$host = as.factor(playoffs$host)
playoffs$host_opponent = as.factor(playoffs$host_opponent)
playoffs$vicinity = as.factor(playoffs$vicinity)
playoffs$vicinity_opponent = as.factor(playoffs$vicinity_opponent)
playoffs$nationality_coach = as.factor(playoffs$nationality_coach)
playoffs$nationality_coach_opponent = as.factor(playoffs$nationality_coach_opponent)

## Add the data from 2016
teams = as.character(levels(playoffs$team))
res2016 = read.csv("data/EM2016.csv", header=TRUE, sep=",")
odds = read.csv("data/QuotenEM2016.txt", header=TRUE, sep="\t")
value = read.csv("data/Wert2016.txt", header=TRUE, sep="\t")
fifa16 = read.csv("data/FIFA2016.txt", header=TRUE, sep="\t")
uefa16 = read.csv("data/UEFA2016.txt", header=TRUE, sep=",")
neues16 = read.csv("data/Neues2016.txt", header=TRUE, sep="\t")
names(fifa16) = c("Fifa_rank", "team", "FIFA_points")
names(neues16) = c("team", "CL_players", "UEFA_players", "vicinity", "age_coach", "nationality_coach", "foreigners", "max1", "max2")
names(odds) = c("team", "odds")
names(value) = c("team", "age", "market_value")
value$market_value = as.numeric(gsub(",", ".", as.character(value$market_value)))

res2016$team = as.character(res2016$team)
res2016$opponent = as.character(res2016$opponent)
res2016$german_team = as.character(res2016$german_team)
res2016$german_opponent = as.character(res2016$german_opponent)
uefa16$team = as.character(uefa16$team)
uefa16[uefa16$team == "Republik Irland",]$team = "Irland"
uefa16$UEFA_points = uefa16$UEFA_points / 1000
fifa16$team = as.character(fifa16$team)
fifa16[fifa16$team == "Republik Irland",]$team = "Irland"
neues16$team = as.character(neues16$team)
odds$team = as.character(odds$team)
value$team = as.character(value$team)

res2016$id = res2016$id + max(playoffs$id)
res2016$max2 = NA
res2016$odds = NA
res2016$market_value = NA
res2016$FIFA_points = NA
res2016$UEFA_rank = NA
res2016$UEFA_points = NA
res2016$CL_players = NA
res2016$max2_opponent = NA
res2016$odds_opponent = NA
res2016$market_value_opponent = NA
res2016$FIFA_points_opponent = NA
res2016$UEFA_rank_opponent = NA
res2016$UEFA_points_opponent = NA
res2016$CL_players_opponent = NA

for (i in 1:nrow(res2016)) {
  team = res2016[i,]$german_team
  opponent = res2016[i,]$german_opponent
  res2016[i,]$max2 = neues16[neues16$team == team, "max2"]
  res2016[i,]$odds = odds[odds$team == team, "odds"]
  res2016[i,]$market_value = value[value$team == team, "market_value"]
  res2016[i,]$FIFA_points = fifa16[fifa16$team == team, "FIFA_points"]
  res2016[i,]$UEFA_rank = uefa16[uefa16$team == team, "UEFA_rank"]
  res2016[i,]$UEFA_points = uefa16[uefa16$team == team, "UEFA_points"]
  res2016[i,]$CL_players = neues16[neues16$team == team, "CL_players"]
  res2016[i,]$max2_opponent = neues16[neues16$team == opponent, "max2"]
  res2016[i,]$odds_opponent = odds[odds$team == opponent, "odds"]
  res2016[i,]$market_value_opponent = value[value$team == opponent, "market_value"]
  res2016[i,]$FIFA_points_opponent = fifa16[fifa16$team == opponent, "FIFA_points"]
  res2016[i,]$UEFA_rank_opponent = uefa16[uefa16$team == opponent, "UEFA_rank"]
  res2016[i,]$UEFA_points_opponent = uefa16[uefa16$team == opponent, "UEFA_points"]
  res2016[i,]$CL_players_opponent = neues16[neues16$team == opponent, "CL_players"]
}

# reorder goals column
goals16 = res2016$goals
res2016$goals = NULL
res2016$goals = goals16

## remove unwanted columns
## This features were determined to be insignificant
playoffs$year = NULL
playoffs$max1 = NULL
playoffs$max1_opponent = NULL
playoffs$vicinity = NULL
playoffs$vicinity_opponent = NULL
playoffs$foreigners = NULL
playoffs$foreigners_opponent = NULL
playoffs$age_coach = NULL
playoffs$age_coach_opponent = NULL
playoffs$EL_players = NULL
playoffs$EL_players_opponent = NULL
playoffs$GDP = NULL
playoffs$GDP_opponent = NULL
playoffs$population = NULL
playoffs$population_opponent = NULL
playoffs$phase = NULL
playoffs$stage = NULL
playoffs$age = NULL
playoffs$age_opponent = NULL
playoffs$host = NULL
playoffs$host_opponent = NULL
playoffs$FIFA_rank = NULL
playoffs$FIFA_rank_opponent = NULL
playoffs$nationality_coach = NULL
playoffs$nationality_coach_opponent = NULL

# reorder goals
goals04_12 = playoffs$goals
playoffs$goals = NULL
playoffs$goals = goals04_12


## Add 2016 data to the table
res2016$german_team = NULL
res2016$german_opponent = NULL
playoffs = rbind(playoffs, res2016)

## discretize the result of a game to a factor of 3 levels
## win, loss, draw
playoffs$result = NA
for(team1 in playoffs$team) {
  for(team2 in playoffs$opponent) {
    goals1 = playoffs[playoffs$team == team1 & playoffs$opponent == team2,]$goals
    goals2 = playoffs[playoffs$team == team2 & playoffs$opponent == team1,]$goals
    playoffs[playoffs$team == team1 & playoffs$opponent == team2,]$result = (goals1 - goals2)
    playoffs[playoffs$team == team2 & playoffs$opponent == team1,]$result = (goals2 - goals1)
  }
}

# install.packages("arules")
library(arules)
playoffs$result = discretize(playoffs$result, method="fixed", categories = c(-Inf, -0.5, 0.5, +Inf))
levels(playoffs$result) = c("loss", "draw", "win")

# ## add column with scores accoring to the performance in the previous three years
# scores = read.csv("data/Team_scores_08-12.txt", header=TRUE, sep="\t")
# scores = as.data.frame(scores)
# colnames(scores) = c("team", "score_04", "score_08", "score_12")
# playoffs$past_scores = rep(0, length(playoffs$team))
# playoffs$past_scores_opponent = rep(0, length(playoffs$team))
# for (i in 1:length(scores[["team"]])) {
#   team = as.character(scores[i,"team"])
#   sc4 = scores[i,"score_04"]
#   sc8 = scores[i,"score_08"]
#   sc12 = scores[i,"score_12"]
#   print(team)
#   for (j in 1:length(playoffs$team)){
#     if(as.character(playoffs[j,]$team) == team){
#       if(playoffs[j,]$year == 2004){
#         playoffs[j,"past_scores"] = sc4
#       }else if(playoffs[j,]$year == 2008){
#         playoffs[j,"past_scores"] = sc8
#       }else{
#         playoffs[j,"past_scores"] = sc12
#       }
#     }
#   }
#   for (j in 1:length(playoffs$team)){
#     if(as.character(playoffs[j,]$opponent) == team){
#       if(playoffs[j,]$year == 2004){
#         playoffs[j,"past_scores_opponent"] = sc4
#       }else if(playoffs[j,]$year == 2008){
#         playoffs[j,"past_scores_opponent"] = sc8
#       }else{
#         playoffs[j,"past_scores_opponent"] = sc12
#       }
#     }
#   }
# }

append_info = function(df, new_df, col_name){
  for (i in 1:length(new_df[["team"]])) {
    team = as.character(new_df[i,"team"])
    for (j in 1:length(df$team)){
      if(as.character(df[j,]$team) == team){
        df[j, col_name] = new_df[i, col_name]
      }
    }
    for (j in 1:length(df$opponent)){
      if(as.character(df[j,]$opponent) == team){
        df[j, paste(col_name, "opponent", sep="_")] = new_df[i, col_name]
      }
    }
  }
  return(df)
}

## add column with nation's GDP, population and #won_medals in oly. games 
nat_info = read.csv("data/additional_info_countries.csv", header=TRUE, sep=",")
nat_info = as.data.frame(nat_info)
colnames(nat_info) = c("team", "population", "GDP", "olymedals")

playoffs$GDP = rep(0, length(playoffs$team))
playoffs$GDP_opponent = rep(0, length(playoffs$team))
playoffs$population = rep(0, length(playoffs$team))
playoffs$population_opponent = rep(0, length(playoffs$team))

playoffs = append_info(playoffs, nat_info, "GDP")
playoffs = append_info(playoffs, nat_info, "population")

# this one is very tricky so better not use it:
# playoffs$olymedals = rep(0, length(playoffs$team))
# playoffs$olymedals_opponent = rep(0, length(playoffs$team))
# playoffs = append_info(playoffs, nat_info, "olymedals")

## normalization of all not factor-features
for (col_name in names(playoffs)) {
  if (!is.factor(playoffs[,col_name]) & col_name != "goals"){
    playoffs[,col_name] = apply(t(playoffs[, col_name]), 2, 
                FUN = function(x){(x-min(playoffs[, col_name]))/(max(playoffs[, col_name])-min(playoffs[, col_name]))})
  }
}

write.csv(playoffs, file="data/playoffs.csv", row.names = FALSE)

filtered_match_duplicates = playoffs[-(seq(2,to=nrow(playoffs),by=2)),]
write.csv(filtered_match_duplicates, file="data/playoffs_wo_duplicates.csv", row.names = FALSE)


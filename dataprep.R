playoffs = read.csv("data/EM2004.2012.publ.txt", header=TRUE, sep="\t")

# discretize the result of a game to a factor of 3 levels
playoffs$result = NA
for(team1 in playoffs$team) {
  for(team2 in playoffs$opponent) {
    goals1 = playoffs[playoffs$team == team1 & playoffs$opponent == team2,]$goals
    goals2 = playoffs[playoffs$team == team2 & playoffs$opponent == team1,]$goals
    playoffs[playoffs$team == team1 & playoffs$opponent == team2,]$result = (goals1 - goals2)
    playoffs[playoffs$team == team2 & playoffs$opponent == team1,]$result = (goals2 - goals1)
  }
}

library(arules)
playoffs$result = discretize(playoffs$result, method="fixed", categories = c(-Inf, -0.5, 0.5, +Inf))
levels(playoffs$result) = c("loss", "draw", "win")

playoffs$id = NULL
playoffs$max1 = NULL
playoffs$max1_opponent = NULL
playoffs$max2_opponent = NULL
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
playoffs$goals = NULL
playoffs$age = NULL
playoffs$age_opponent = NULL
playoffs$host = NULL
playoffs$host_opponent = NULL
playoffs$FIFA_rank = NULL
playoffs$FIFA_rank_opponent = NULL
playoffs$UEFA_rank_opponent = NULL
playoffs$nationality_coach = NULL
playoffs$nationality_coach_opponent = NULL

# transform the year to the distance of the current competition
# playoffs$year = NULL # sorry but I need it. I moved the removal at the end.

write.csv(playoffs, file="data/playoffs.csv", row.names = FALSE)

# add column with scores accoring to the performance in the previous three years
scores = read.csv("data/Team_scores_08-12.txt", header=TRUE, sep="\t")
scores = as.data.frame(scores)
colnames(scores) = c("team", "score_04", "score_08", "score_12")
playoffs$past_scores = rep(0, length(playoffs$team))
playoffs$past_scores_opponent = rep(0, length(playoffs$team))
for (i in 1:length(scores[["team"]])) {
  team = as.character(scores[i,"team"])
  sc4 = scores[i,"score_04"]
  sc8 = scores[i,"score_08"]
  sc12 = scores[i,"score_12"]
  print(team)
  for (j in 1:length(playoffs$team)){
    if(as.character(playoffs[j,]$team) == team){
      if(playoffs[j,]$year == 2004){
        playoffs[j,"past_scores"] = sc4
      }else if(playoffs[j,]$year == 2008){
        playoffs[j,"past_scores"] = sc8
      }else{
        playoffs[j,"past_scores"] = sc12
      }
    }
  }
  for (j in 1:length(playoffs$team)){
    if(as.character(playoffs[j,]$opponent) == team){
      if(playoffs[j,]$year == 2004){
        playoffs[j,"past_scores_opponent"] = sc4
      }else if(playoffs[j,]$year == 2008){
        playoffs[j,"past_scores_opponent"] = sc8
      }else{
        playoffs[j,"past_scores_opponent"] = sc12
      }
    }
  }
}


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

# add column with nation's GDP, population and #won_medals in oly. games 
nat_info = read.csv("data/additional_info_countries.txt", header=TRUE, sep="\t")
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

# normalize the features GDP and populations since they yield super large numbers in comaprison
# to the other features.
# playoffs[, "population"] = 
#   apply(t(playoffs[, "population"]), 2, 
#         FUN = function(x){(x-min(playoffs[, "population"]))/(max(playoffs[, "population"])-min(playoffs[, "population"]))})
# playoffs[, "population_opponent"] = 
#   apply(t(playoffs[, "population_opponent"]), 2, 
#         FUN = function(x){(x-min(playoffs[, "population_opponent"]))/(max(playoffs[, "population_opponent"])-min(playoffs[, "population_opponent"]))})
# playoffs[, "GDP"] = 
#   apply(t(playoffs[, "GDP"]), 2, 
#         FUN = function(x){(x-min(playoffs[, "GDP"]))/(max(playoffs[, "GDP"])-min(playoffs[, "GDP"]))})
# playoffs[, "GDP_opponent"] = 
#   apply(t(playoffs[, "GDP_opponent"]), 2, 
#         FUN = function(x){(x-min(playoffs[, "GDP_opponent"]))/(max(playoffs[, "GDP_opponent"])-min(playoffs[, "GDP_opponent"]))})

# normalization of all not factor-features
for (col_name in names(playoffs)) {
  if (!is.factor(playoffs[,col_name])){
    playoffs[,col_name] = apply(t(playoffs[, col_name]), 2, 
                FUN = function(x){(x-min(playoffs[, col_name]))/(max(playoffs[, col_name])-min(playoffs[, col_name]))})
  }
}

# playoffs$team = NULL
# playoffs$opponent = NULL
playoffs$year = NULL



write.csv(playoffs, file="data/playoffs.csv", row.names = FALSE)


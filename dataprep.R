playoffs2 = read.csv("data/EM2004.2012.publ.txt", header=TRUE, sep="\t")

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
playoffs$year = NULL

write.csv(playoffs, file="data/playoffs.csv", row.names = FALSE)

# add column with scores accoring to the performance in the previous three years
scores = read.csv("data/Team_scores_08-12.txt", header=TRUE, sep="\t")
scores = as.data.frame(scores)
playoffs$past_scores = rep(0, length(playoffs$team))
playoffs$past_scores_opponent = rep(0, length(playoffs$team))
for (i in 1:length(scores[["Team"]])) {
  team = as.character(scores[i,"Team"])
  sc4 = scores[i,"Score_04"]
  sc8 = scores[i,"Score_08"]
  sc12 = scores[i,"Score_12"]
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

# playoffs$team = NULL
# playoffs$opponent = NULL
write.csv(playoffs, file="data/playoffs.csv", row.names = FALSE)


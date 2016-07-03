playoffs = read.csv("data/EM2004.2012.publ.txt", header=TRUE, sep="\t")

# discretize the result of a game to a factor of 3 levels
# playoffs$result = NA
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
playoffs$max2 = NULL
playoffs$max1_opponent = NULL
playoffs$max2_opponent = NULL
playoffs$vicinity = NULL
playoffs$vicinity_opponent = NULL
playoffs$foreigners = NULL
playoffs$foreigners_opponent = NULL
playoffs$age_coach = NULL
playoffs$age_coach_opponent = NULL

# thos are going to be replaced by our values
playoffs$GDP = NULL
playoffs$GDP_opponent = NULL
playoffs$population = NULL
playoffs$population_opponent = NULL

# remove some atributes to simplify for now
# those will not be removed in the final model
playoffs$team = NULL
playoffs$opponent = NULL
playoffs$phase = NULL
playoffs$stage = NULL
playoffs$goals = NULL
playoffs$X = NULL

# transform the year to the distance of the current competition
playoffs$year = 2016 - playoffs$year

write.csv(playoffs, file="data/playoffs.csv", row.names = FALSE)

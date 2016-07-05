# poisson baseline
playoffs = read.csv("data/EM2004.2012.publ.txt", header=TRUE, sep="\t")
playoffs = playoffs[,c("team", "opponent", "goals")]

team = levels(playoffs$team)
valid_indices = sample(1:186, 30)
valid_data = playoffs[valid_indices,]
valid_data_goals = valid_data$goals
valid_data$goals = NULL

train_data = playoffs[-valid_indices,]

model = glm(goals ~ team + opponent, data = train_data, family = poisson())

p = 22
w = model$coefficients
attack = w[1:p]
attack[-1] = attack[-1] + attack[1]

# the reason is that one team can only by defended by p - 1 (21)
# other teams, i.e. not by itself ... hence the p - 1 degrees of freedom
defence = rep(NA, p)
defence[1] = 0 # note that we do not need to add an intercept here, 
# since the defence of the first team is arbitrary (since a team cannot play against itself)

# minus cause we used + in the formula above
defence[-1] = -w[(p+1) : (2*p - 1)]

names(defence) = names(attack) = team

plot(exp(attack), exp(defence), pch=".")
text(exp(attack), exp(defence), labels=team, cex=0.5)

grid_scores = function(att) {
  return(exp(att - defence))
}

res = data.frame(NA, NA, NA)
names(res) = c("team", "opponent", "result")
scores = lapply(attack, FUN = grid_scores)

for(team in names(scores)){
  opponents = scores[[team]]
  for(opp in names(opponents)) {
    if(opp != team) {
      score1 = opponents[[opp]]
      score2 = scores[[opp]][[team]]
      res = rbind(res, c(team, opp, score1 - score2))
    }
  }
}

res = res[-1,]
reject = 0.2

res$res = NA
res$result = as.numeric(res$result)
res[res$result > reject, ]$res = "win"
res[res$result < (-reject), ]$res = "loss"
res[res$result <= reject & res$result >= -reject, ]$res = "draw"
res$result = res$res
res$res = NULL
res$result = as.factor(res$result)

playoffs2 = read.csv("data/playoffs.csv", header=TRUE, sep=",")
playoffs2 = data.frame(playoffs$team, playoffs$opponent, playoffs2$result)
names(playoffs2) = c("team", "opponent", "result")

playoffs2$pred = NA
for(i in 1:length(playoffs2$team)) {
    match = playoffs2[i,]
    team = as.character(match$team)
    opponent = as.character(match$opponent)
    index1 = (res$team == team & res$opponent == opponent)
    index2 = (playoffs2$team == team & playoffs2$opponent == opponent)
    playoffs2[index2, ]$pred = as.character(res[index1, ]$result)
}
playoffs2$pred = as.factor(playoffs2$pred)

# take only the validation data
playoffs2 = playoffs2[valid_indices,]

sum(playoffs2$result != playoffs2$pred)/length(playoffs2$team)

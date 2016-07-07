p_goals_given_team_opoonent  = function(goals, team, opponent, model){
  loglambda_hat = predict(model, newdata=data.frame(team=team, opponent=opponent))
  dpois(goals, exp(loglambda_hat))
}

# poisson baseline
# prediction of win, draw, loss probabilities for 
# "pred_matches", based on training on "data".
# The first two columns are expected to be "team" and "opponent".
# There should be a column result, with the result of the first team.
predictProbsPoissonBasic <- function (pred_matches, data, score=expand.grid(goals1=0:15, goals2=0:15)){
  model = glm(goals ~ team + opponent, family=poisson, data=data)
  probs = sapply(
    1:nrow(pred_matches),
    function(i){
      team = as.character(pred_matches[i,1])
      opponent = as.character(pred_matches[i,2])
      ## for a match with a new team, equal probs of outcomes
      ## (feel free to make this more clever)
      if(!((team %in% levels(factor(data$team))) & (opponent %in% levels(factor(data$opponent))))) {
        return (c(draw=1/3, loss=1/3, win=1/3))
      }
      phat = with(score,
                  p_goals_given_team_opoonent(goals1, team, opponent, model) *
                    p_goals_given_team_opoonent(goals2, opponent, team, model)
      )
      c(
        draw = sum(phat[score$goals1==score$goals2]),
        loss = sum(phat[score$goals1<score$goals2]),
        win = sum(phat[score$goals1>score$goals2])
      )
    }
  )
  t(probs)
}

main <- function() {
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
}

# main()

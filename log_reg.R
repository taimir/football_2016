
# prediction of win, draw, loss probabilities for 
# "pred_matches", based on training on "data".
# The first two columns are expected to be the team names.
predictProbsMultinomHeuristic <- function (pred_matches, data){
  library(nnet)
  # remove team names from training
  train_data = data[-c(1,2)]
  model <- multinom(result ~ ., data = train_data)
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
      # predict without the team names
      return(predict(model, type="probs",newdata=pred_matches[i, -c(1,2)]))
    }
  )
  t(probs)
}

# prediction of win, draw, loss probabilities for 
# "pred_matches", based on training on "data".
# The first two columns are expected to be the team names.
predictProbsMultinom <- function (pred_matches, data){
  library(nnet)
  # remove team names from training
  train_data = data[-c(1,2)]
  pred_matches = pred_matches[-c(1,2)]
  model <- multinom(result ~ ., data = train_data)
  probs = predict(model, type="probs",newdata=pred_matches)
  #evt transpose??
}

# prediction of win, draw, loss probabilities for 
# "pred_matches", based on training on "data".
# The first two columns are expected to be the team names.
predictProbsHard <- function(pred_matches, data, cutoff = 0.71){
  draw_data = data[-c(1,2)]
  draw_data$result = as.character(draw_data$result)
  draw_data[draw_data == "win"] = "not_draw"
  draw_data[draw_data == "loss"] = "not_draw"
  draw_data$result = factor(as.character(draw_data$result), levels=c("draw", "not_draw"))
  draw_model = glm(result ~ ., data = draw_data, family=binomial())
  
  wl_data = data[data$result != "draw", ]
  wl_data = wl_data[-c(1,2)]
  wl_data$result = factor(wl_data$result, levels=c("win", "loss"))
  wl_model = glm(result ~ ., data = wl_data, family=binomial())
  
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
      not_draw_pred = predict(draw_model, type="response", newdata=pred_matches[i, -c(1,2)])
      if ((1 - not_draw_pred) > cutoff) {
        return (c(draw=(1-not_draw_pred), loss=0, win=0))
      } else {
        wl_pred = predict(wl_model, type="response", newdata=pred_matches[i, -c(1,2)])
        return (c(draw=0, loss=(1 - wl_pred), win=wl_pred))
      }
    }
  )
  t(probs)
}

run_log_reg = function(playoffs){
  valid_indices = sample(1:186, 20)
  valid_data = playoffs[valid_indices,]
  valid_data_res = valid_data$result
  valid_data$result = NULL
  
  train_data = playoffs[-valid_indices,]

  library(nnet)
  model <- multinom(result ~ ., data = train_data)
  pred <- predict(model, type="probs",newdata=valid_data)
  print(pred)
  
  predictions = apply(pred, 1, FUN = function(args){colnames(pred)[which.max(args)]})
  predictions = factor(predictions, levels=c("win", "draw", "loss"))
  sum(predictions == valid_data_res) / length(predictions)
}

main <- function() {
  # Multi-variate logistic regression
  playoffs = read.csv("data/playoffs.csv", header=TRUE, sep=",")
  playoffs$team = NULL
  playoffs$opponent = NULL
  playoffs$goals = NULL
  playoffs$id = NULL
  
  num_tests = 100
  avg_performance = rep(-1, num_tests)
  iteration_wo_errors = -1
  for (i in 1:num_tests) {
    avg_performance[i] = run_log_reg(playoffs)
    iteration_wo_errors = iteration_wo_errors + 1
  }
  print(sum(avg_performance[avg_performance!=-1])/length(avg_performance[avg_performance!=-1]))
  print(iteration_wo_errors)
}

# main()



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
  model <- multinom(result ~ ., data = train_data)
  probs = sapply(
    1:nrow(pred_matches),
    function(i){
      team = as.character(pred_matches[i,1])
      opponent = as.character(pred_matches[i,2])
      # predict without the team names
      return(predict(model, type="probs",newdata=pred_matches[i, -c(1,2)]))
    }
  )
  t(probs)
}

run_log_reg = function(playoffs){
  valid_indices = sample(1:186, 20)
  valid_data = playoffs[valid_indices,]
  valid_data_res = valid_data$result
  valid_data$result = NULL
  
  # draws = playoffs[playoffs$result == "draw",]
  # draws_win = draws
  # draws_win$result = as.factor("win")
  # draws_loss = draws
  # draws_loss$result = as.factor("loss")
  # playoffs = rbind(playoffs[playoffs$result != "draw",], draws_win, draws_loss)
  # playoffs$result = factor(playoffs$result)
  
  train_data = playoffs[-valid_indices,]
  
  
  
  
  library(nnet)
  model <- multinom(result ~ ., data = train_data)
  
  # model <- glm(result ~.,family=binomial(link='logit'),data=train_data)
  # pred <- predict(model, type="response",newdata=valid_data)
  
  pred <- predict(model, type="probs",newdata=valid_data)
  print(pred)
  # do a reject region below 50% certainty
  # pick_pred <- function(arg) {
  #   if (0.45 < arg & arg < 0.55) {
  #     return("draw")
  #   }
  #   
  #   if (arg > 0.5) {
  #     return("win")
  #   }
  #   
  #   return("loss")
  # }
  # predictions <- as.factor(as.character(lapply(pred, FUN = pick_pred)))
  
  predictions = apply(pred, 1, FUN = function(args){colnames(pred)[which.max(args)]})
  predictions = factor(predictions, levels=c("win", "draw", "loss"))
  sum(predictions == valid_data_res) / length(predictions)
  #check_result = data.frame(pred, valid_data_res)
}

main <- function() {
  # Multi-variate logistic regression
  playoffs = read.csv("data/playoffs.csv", header=TRUE, sep=",")
  playoffs$team = NULL
  playoffs$opponent = NULL
  
  num_tests = 1
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


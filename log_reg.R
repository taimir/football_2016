# Multi-variate logistic regression
playoffs = read.csv("data/playoffs.csv", header=TRUE, sep=",")
playoffs$team = NULL
playoffs$opponent = NULL

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
predictions = as.factor(predictions)
sum(predictions == valid_data_res) / length(predictions)

check_result = data.frame(pred, valid_data_res)

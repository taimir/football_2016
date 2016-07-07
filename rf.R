# Random forest

predictProbsRandomForest <- function (pred_matches, data){
  train_data = data[-c(1,2)]
  to_predict = pred_matches[-c(1,2)]
  library(caret)
  rf_model <- train(result ~ ., data=train_data, 
                    method="rf", 
                    trControl=trainControl(method="cv", number=20), 
                    prox=TRUE, 
                    allowParallel=TRUE, 
                    ntree=1000)
  
  pred <- predict(rf_model, type="prob",newdata=to_predict)
}

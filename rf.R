# Random forest

playoffs = read.csv("data/playoffs.csv", header=TRUE, sep=",")

# format as factors
playoffs$host = as.factor(playoffs$host)
playoffs$host_opponent = as.factor(playoffs$host_opponent)
playoffs$vicinity = as.factor(playoffs$vicinity)
playoffs$vicinity_opponent = as.factor(playoffs$vicinity_opponent)
playoffs$nationality_coach = as.factor(playoffs$nationality_coach)
playoffs$nationality_coach_opponent = as.factor(playoffs$nationality_coach_opponent)

library(caret)
rf_model <- train(result ~ ., data=playoffs_selected, 
                  method="rf", 
                  trControl=trainControl(method="cv", number=20), 
                  prox=TRUE, 
                  allowParallel=TRUE, 
                  ntree=1000)
rf_model
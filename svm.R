# SVM
predictSupportVectorMachine <- function (pred_matches, data){
  train_data = data[-c(1,2)]
  pred_matches = pred_matches[-c(1,2)]
  
  library(e1071)
  model_svm = svm(result ~ ., data=train_data, 
                  scale=TRUE, 
                  type="C-classification",
                  kernel="radial",
                  cost=1,
                  probability=TRUE)
  
  pred = predict(model_svm, pred_matches, probability=TRUE)
  pred = attr(pred,"probabilities")
  return(pred[,order(colnames(pred)), drop=FALSE])
}









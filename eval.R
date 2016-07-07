playoffs = read.csv("data/playoffs.csv", header=TRUE, sep=",")

## predictions of next day match from minday to last day
outcomeConfidence = function(minday, data, predictProbs){
  probs = as.data.frame(matrix(NA, nrow=nrow(data), ncol=3))
  
  for(d in minday:max(data$id)){
    past_data = subset(data, id<d)
    past_data$id = NULL
    pred_row = which(data$id==d)
    pred_match = data[pred_row,,drop=FALSE]
    pred_match$id = NULL
    if("result" %in% colnames(pred_match)) {
      pred_match$result = NULL
    }
    if("goals" %in% colnames(pred_match)) {
      pred_match$goals = NULL
    }
    probs[pred_row, ] = predictProbs(pred_match, past_data)  
  }
  colnames(probs) = c("draw", "loss", "win")
  tmp = colnames(probs) [apply(probs, 1, function(x) if(any(is.na(x))) {NA} else {which.max(x)})]
  tmp = factor(tmp, levels=c("draw", "loss", "win"))
  data.frame(
    pred = tmp,
    confidence = apply(probs, 1, max)
  )
}

#################
# fitting and evaluation
#################
## predictions and confidence scores for different models
## for the last championship (30 days)
minday = max(playoffs$id) - 60

# temporary, until the last match is out
playoffs = playoffs[1:(nrow(playoffs) - 2),]
playoffs_cat = playoffs
playoffs_cat$goals = NULL

playoffs_goals = playoffs
playoffs_goals$result = NULL

source("log_reg.R")
resMultinomHeuristic = outcomeConfidence(minday, playoffs_cat, predictProbsMultinomHeuristic)
resMultinom = outcomeConfidence(minday, playoffs_cat, predictProbsMultinom)

source("poisson.R")
resPoisson = outcomeConfidence(minday, playoffs_goals, predictProbsPoissonBasic)

source("rf.R")
#resRandomForest = outcomeConfidence(minday, playoffs_cat, predictProbsRandomForest)

source("svm.R")
resSupportVectorMachine = outcomeConfidence(minday, playoffs_cat, predictSupportVectorMachine)

## number of correct predictions: this is the value to perform best on
# sum(res$pred == playoffs$result, na.rm=TRUE)
## getting more insights: confusion table
# table(playoffs$result, res$pred)


## getting more insights: precisions  (proportion of true positives) vs ranked predictions
getPrecision = function(res, data){
  res$tp = (data$result == res$pred)
  prec_res = res[order(res$confidence, decreasing=TRUE), ]
  prec_res$pr = cumsum(prec_res$tp)/(1:nrow(prec_res))
  prec_res
}

precisionMultinomHeuristic = getPrecision(resMultinomHeuristic, playoffs)
precisionMultinom = getPrecision(resMultinom, playoffs)
precisionPoisson = getPrecision(resPoisson, playoffs)
precisionSupportVectorMachine = getPrecision(resSupportVectorMachine, playoffs)
#precisionRandomForest = getPrecision(resRandomForest, playoffs)

plot(precisionMultinomHeuristic$pr, ylim=c(0,1), type="l", xlab="prediction ranked by decreasing confidence score", ylab="Precision (correct proportion of guesses)")
lines(precisionMultinom$pr, col="red")
lines(precisionPoisson$pr, col="green")
#lines(precisionRandomForest$pr, col="blue")
lines(precisionSupportVectorMachine$pr, col="orange")
abline(h=0.33, lty = 2, col="grey")
legend("topright", col=c("black", "red", "green", "blue", "orange", "grey"), lty=c(1,1,1,1,1,2), legend=c("Multinom + heuristic", "Multinom", "Poisson", "Random Forest", "Support Vector Machine", "Three-Sided Coin"))

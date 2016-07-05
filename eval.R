# setwd("/project/smml/WS1415")
#################
# Preparing data
#################
tab = read.csv("dataset/11-football.csv")

# number the days
tab$day = as.integer(factor(with(tab, paste(season, sprintf("%02d", matchday), sep="_"))))

# we focus on predicting the outcome: win, draw, or lose
tab$outcome = with(tab, ifelse(homegoals>guestgoals, "win", ifelse(homegoals==guestgoals, "draw", "lose") ))

# predictions will be made from minday on
minday = 1 # max(tab$day) - 

#################
# GLM fitting functions
#################
# create data frame for GLM fit 
makeDf = function(data){
  y = c(data$homegoals, data$guestgoals)
  x1 = c(as.character(data$home), as.character(data$guest))
  x2 = c(as.character(data$guest), as.character(data$home))
  data.frame(y, x1, x2)
}

# prediction of goals for GLM fit
py_given_x1_x2  = function(y, x1, x2, fit){
  loglambda_hat = predict(fit, newdata=data.frame(x1=x1, x2=x2))
  dpois(y, exp(loglambda_hat))
}

# prediction of win, draw, lose probabilities for GLM fit
predictProbs <- function (pred_match, data, model=2, score=expand.grid(y1=0:15, y2=0:15)){
  df = makeDf(data)
  if(model==0) fit = glm(y ~ 1, family=poisson, data=df)
  if(model==1) fit = glm(y ~ x1, family=poisson, data=df)
  if(model==2) fit = glm(y ~ x1 + x2, family=poisson, data=df)
  probs = sapply(
    1:nrow(pred_match),
    function(i){
      x1 = as.character(pred_match[i,1])
      x2 = as.character(pred_match[i,2])
      ## for a match with a new team, equal probs of outcomes
      ## (feel free to make this more clever)
      if(!((x1 %in% levels(factor(df$x1))) & (x2 %in% levels(factor(df$x2))))) {
        return (c(win=1/3, draw=1/3, lose=1/3))
      }
      phat = with(score,
                  py_given_x1_x2(y1, x1, x2, fit) *
                    py_given_x1_x2(y2, x2, x1, fit)
      )
      c(
        win = sum(phat[score$y1>score$y2]),
        draw = sum(phat[score$y1==score$y2]),
        lose = sum(phat[score$y1<score$y2])
      )
    }
  )
  t(probs)
}

## predictions of next day match from minday to last day
outcomeConfidence = function(model, minday, tab){
  probs = as.data.frame(matrix(NA, nrow=nrow(tab), ncol=3))
  
  for(d in minday:max(tab$day)){
    tab_past = subset(tab, day<d)
    pred_row = which(tab$day==d)
    pred_match = tab[pred_row, c("home", "guest"), drop=FALSE]
    probs[pred_row, ] = predictProbs(pred_match, tab_past, model=model)  
  }
  colnames(probs) = c("win", "draw", "lose")
  
  data.frame(
    pred_outcome = colnames(probs) [apply(probs, 1, function(x) if(any(is.na(x))) {NA} else {which.max(x)})],
    pred_confidence = apply(probs, 1, max)
  )
}

#################
# fitting and evaluation
#################
## predictions and confidence scores for model 1 and model 2 
oc1 = outcomeConfidence(1, minday, tab)
oc2 = outcomeConfidence(2, minday, tab)

## number of correct predictions: this is the value to perform best on
sum(oc1$pred_outcome==tab$outcome, na.rm=TRUE)
sum(oc2$pred_outcome==tab$outcome, na.rm=TRUE)


## getting more insights: confusion table
table(tab$outcome, oc1$pred_outcome)
table(tab$outcome, oc2$pred_outcome)


## getting more insights: precisions  (proportion of true positives) vs ranked predictions
getPrecision = function(oc, tab){
  oc$tp = tab$outcome == oc$pred_outcome
  oco = oc[order(oc$pred_confidence, decreasing=TRUE), ]
  oco$pr = cumsum(oco$tp)/1:nrow(oco)
  oco
}

oc1o = getPrecision (oc1, tab)
oc2o = getPrecision (oc2, tab)

plot(oc1o$pr, ylim=c(0,1), type="l", xlab="prediction ranked by decreasing confidence score", ylab="Precision (proportion of won games)")
lines(oc2o$pr, col="red")
legend("topright", col=c("black", "red"), lty=1, legend=c("Model 1", "Model 2"))
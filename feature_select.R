# ======================================
# Data visualization & Feature selection
# ======================================

# Load the preprocessed data
playoffs = read.csv("data/playoffs.csv", header=TRUE, sep=",")

## Do basic feature selection based on the results of an lssvmRadial kernel
## using the mlbench package
# install.packages("mlbench")
# install.packages("caret")
library(mlbench)
library(caret)

control <- trainControl(method="cv", number=10, repeats=3)
model <- train(result~., data=playoffs, method="lssvmRadial", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
print(importance)
# plot importance
plot(importance)


## Plot the features of two teams against each other
## Trying to identify meaningful features (visually)
## For example:
no_names = playoffs2[c(10,28)] 
plot(as.matrix(no_names), t='n')
text(no_names, 
     labels=paste(substr(filtered_match_duplicates$team, 1,3), 
                  substr(filtered_match_duplicates$opponent, 1,3), sep="-"), 
     col=filtered_match_duplicates$color)



## Dimensionality reduction of the data using tSNE
## Trying to identify meaningful features by visual inspection
library(tsne)
filtered_match_duplicates = playoffs#[-(seq(2,to=nrow(playoffs),by=2)),]
filtered_match_duplicates$color = rep(NA, length(filtered_match_duplicates$team))
for (i in 1:length(filtered_match_duplicates$team)) {
  if(as.character(filtered_match_duplicates[i,"result"]) == "win"){
    filtered_match_duplicates[i,"color"] = "green"
  }else if(as.character(filtered_match_duplicates[i,"result"]) == "draw"){
    filtered_match_duplicates[i,"color"] = "yellow"
  }else{
    filtered_match_duplicates[i,"color"] = "red"
  }
}

no_names =  filtered_match_duplicates[-c(1, 2, 17, 24)]
tsne_playoffs = tsne(as.matrix(no_names), 
                     k = 2, 
                     initial_dims = length(names(no_names)), 
                     perplexity = 3, 
                     max_iter = 2000)

# install.packages("zoom")
library(zoom)
plot(tsne_playoffs, t='n')
text(tsne_playoffs, labels=paste(substr(filtered_match_duplicates$team, 1,3), 
                                 substr(filtered_match_duplicates$opponent, 1,3), sep="-"), 
     col=filtered_match_duplicates$color)
zm()

## Results of the visual analysis:
# odds, odds_opponent are good
# CL_players opponent are good
# market_value is good as well
# UEFA_rank against UEFA_rank opponent / UEFA_points opponent is good
# GDP vs GDP_opponent seems good
# population seems sort of OK
# max2 seems OK

# max1 is messy
# past_scores vs past_scores opponent is somewhat messy
# FIFA_Points are somewhat messy
# age is very messy, very slight tendencies ...

## Add the tSNE reduced features to the data, in an effort to use them as predictors.
## attach t-sne columns to the data file
playoffs$t_sne_1 = tsne_playoffs[,1]
playoffs$t_sne_2 = tsne_playoffs[,2]
playoffs$result = factor(playoffs$result, levels=c("loss", "draw", "win"), ordered = TRUE)

# update csv file
write.csv(playoffs, file="data/playoffs.csv", row.names = FALSE)

## Now do dimensionality reduction for the individual teams as
## data points, instead of for the matches as data points
team_data_only = playoffs[c(1,3,4,5,6,7,8,9,16,18,20)]
team_data_only_unique = team_data_only[!duplicated(team_data_only$team),]
#opp_data_only = playoffs[c(2,10,11,12,13,14,17,19)]

tsne_team = tsne(as.matrix(team_data_only_unique[-1]), 
                     k = 2, 
                     initial_dims = length(names(team_data_only_unique)), 
                     perplexity = 2, 
                     max_iter = 500)

plot(tsne_team, t='n')
text(tsne_team, labels=substr(team_data_only_unique$team, 1,2))

## We wanted to use FSelector for the categorical features,
## but we found them to be of insignificant importance

# library(FSelector)
# weights_info_gain = information.gain(result ~ ., data=playoffs)
# weights_info_gain[order(-weights_info_gain$attr_importance), , drop = FALSE]

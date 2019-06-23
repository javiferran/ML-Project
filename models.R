library(magrittr)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)

setwd("/Users/JaviFerrando/Desktop/ML-Project/")

dir <- 'input/'


#####################################################################################################
#                            OBTAIN TRAIN SET                                                       #
#####################################################################################################

# Load all necessary data
dseeds_tournament <- fread(paste(dir,'NCAATourneySeeds.csv',sep=''))
# make seeds 1-16 without letters (except for certain seed)
dseeds_tournament <- dseeds_tournament %>%
  mutate(ranking = as.factor((str_replace(Seed, "[A-Z]",""))),
         rank_num = as.numeric(str_replace(ranking, ".[a-z]","")))
names(dseeds_tournament) <- tolower(names(dseeds_tournament))

dg_tournment <- fread(paste(dir,'NCAATourneyCompactResults.csv',sep=''))
season_elos <- read.csv(paste(dir,'season_elos.csv',sep='')) %>% rename(teamid = team_id)
seas_enrich <- fread(paste(dir,'NCAASeasonDetailedResultsEnriched.csv',sep=''))


source("pre_processing.R")

# Preprocess all the thata with the aim to obtain a unique dataframe named outcome_tournament.
outcome_tournament <- get.outcome_tournament(dg_tournment, dseeds_tournament) # 1st step load data + clean
stats_season <- get.stats_season(seas_enrich) # 2nd get advanced statistics
outcome_tournament <- add.season_elos(outcome_tournament, season_elos) # 3rd add season elos
outcome_tournament <- add.advanced_Feature(stats_season, outcome_tournament,'MPIE') # 4th add MPIE feature
outcome_tournament <- add.advanced_Feature(stats_season, outcome_tournament, 'MNetRTG') # 5th add Netrtg feature



#####################################################################################################
#                            OBTAIN VALIDATION SET                                                  #
#####################################################################################################


#sample_submission <- read.csv(paste(dir,'SampleSubmissionStage2.csv',sep=''))# 2019 every possible matchup -> can only check by submitting to Kaggle
sample_submission <- read.csv(paste(dir,'SampleSubmissionStage1.csv',sep=''))# 2014-2018 every possible matchup 

#d_ss -> same as outcome_tournament but with sample_submission format (every possible matchup)

### Join team data and ranking data
d_ss <- get.sample_submission(sample_submission, dseeds_tournament)# 1st step load data + clean
d_ss <- add.season_elos(d_ss, season_elos) # 3rd add season elos
d_ss <- add.advanced_Feature(stats_season, d_ss,'MPIE') # 4th add MPIE feature
d_ss <- add.advanced_Feature(stats_season, d_ss, 'MNetRTG') # 5th add Netrtg feature

#####################################################################################################





### Make predictions based on model 
d_ss$t1_rank <- NULL
d_ss$t2_rank <- NULL
outcome_tournament$t1_rank <- NULL
outcome_tournament$t2_rank <- NULL
train <- outcome_tournament %>% filter(season <= 2013) #Takes occurred tournament games results (team1win)
test_outcome_tournament <- outcome_tournament %>% filter(season > 2013) #Test sample, target team1win

#Train model with train data
#Add predictions to dss
#Merge d_ss with test_outcome_tournament (games that occurred) -> validation
#validation has target and Pred for every game that occurered 2014-2018
#Apply LogLoss to validation$Pred and validation$team1win

train$elo_diff <- NULL

train$diff_rank <- NULL

#Regularized Logistic Regression
training_set <- train[,-(1:4)]
col_order <- colnames(training_set)

test_set<- d_ss[,-1][,-(2:4)]
colnames(test_set)[1] <- "team1win"
test_set$elo_diff <- NULL
test_set$diff_rank <- NULL
test_set <- test_set[, col_order]



######################################################################################
#logistic regression model: differences

model <- glm(team1win ~ .
             ,
             
             data = training_set, family = binomial)

summary(model)
#Predict on every possible matchup
predict <- data.frame(Pred = predict(model, newdata = test_set, type = 'response'))
d_ss <- d_ss %>% mutate(Pred = predict$Pred)# %>% dplyr::select(ID, Pred) Change sample submission pred=0.5 to model predicition

d_ss_fin <- sample_submission %>% mutate(Pred = d_ss$Pred) #only matchup and prediction -> Results for kaggle
#write.csv(d_ss_fin, "submission_stage_2.csv", row.names = FALSE)

######################################################################################
#Model test
library(MLmetrics)
test_result <- merge(x = test_outcome_tournament, y = d_ss[2:5], by=c("team1id","team2id","season"), all = FALSE)

LogLoss(y_pred = test_result$Pred, y_true = test_result$team1win)

# if Accuracy
#test_result$Pred<- factor(test_result$Pred, labels=c(0,1))
#Accuracy(y_pred = test_result$Pred, y_true = test_result$team1win)

######################################################################################
# Neural network
library(neuralnet)


train_nnet_scaled <- as.data.frame(scale(training_set[-1]))
train_nnet_scaled <- cbind(training_set$team1win,train_nnet_scaled)
colnames(train_nnet_scaled)[1] <- "team1win"


names <- colnames(train_nnet_scaled)[-1] #choose the names you want
a <- as.formula(paste('team1win ~ ' ,paste(names,collapse='+')))
a
#neuralnet DOESN'T need factors as target
nn <- neuralnet(a, data=train_nnet_scaled, hidden=c(1), linear.output=FALSE, threshold=0.01)
nn$result.matrix
plot(nn)

test_set_scaled <- scale(test_set)

test_set_scaled <- subset(test_set_scaled, select = colnames(test_set_scaled)[-1])
nn.results <- compute(nn, test_set_scaled)

d_ss <- d_ss %>% mutate(Pred = as.numeric(nn.results$net.result))

######################################################################################
#Model test
test_result <- merge(x = test_outcome_tournament, y = d_ss[2:5], by=c("team1id","team2id","season"), all = FALSE)

LogLoss(y_pred = test_result$Pred, y_true = test_result$team1win)

# if Accuracy
#test_result$Pred<- factor(test_result$Pred, labels=c(0,1))
#Accuracy(y_pred = test_result$Pred, y_true = test_result$team1win)

######################################################################################


#Decision tree
library(rpart)
train_tree <- training_set
train_tree$team1win<- factor(train_tree$team1win, labels=c(0,1))#not for neuralnet

DecisionTree = rpart(team1win ~ ., data=train_tree,control=rpart.control(cp=0.001, xval=10),method='class')
printcp(DecisionTree)

treeSize = DecisionTree$cptable[,2]+1 #nsplit
treeImpurity = DecisionTree$cptable[,3] #rel error
cvImpurity = DecisionTree$cptable[,4] #xerror

plot(treeSize, treeImpurity, main="R(T)", xlab="size of the tree", ylab="Relativity Impurity", type="o", col='red') 
lines(treeSize, cvImpurity ,type="o", col='blue')
legend("topright", c("All training data","CV training data"), col=c('red', 'blue'), lty=1)

DecisionTree$cptable = as.data.frame(DecisionTree $cptable)
ind = which.min(DecisionTree$cptable$xerror)
xerr <-DecisionTree$cptable$xerror[ind]
xstd <-DecisionTree$cptable$xstd[ind]

i = 1
while (DecisionTree$cptable$xerror[i] > xerr+xstd){
  i = i+1
}
alfa = DecisionTree$cptable$CP[i]
#alfa = DecisionTree$cptable$CP[3]

optimal <- prune(DecisionTree, cp=alfa)
par(mfrow = c(1,1), xpd = NA)
plot(optimal)
text(optimal, use.n=T,cex=0.8,col="blue")

#Tree prediction
rpart_pred <- predict(DecisionTree,test_set,type='prob')[,1]
rpart_pred_class <- predict(DecisionTree,test_set,type='class')
d_ss <- d_ss %>% mutate(Pred = predict(DecisionTree,test_set,type='prob')[,1])

######################################################################################
#Model test
test_result <- merge(x = test_outcome_tournament, y = d_ss[2:5], by=c("team1id","team2id","season"), all = FALSE)

LogLoss(y_pred = test_result$Pred, y_true = test_result$team1win)

# if Accuracy
#test_result$Pred<- factor(test_result$Pred, labels=c(0,1))
#Accuracy(y_pred = test_result$Pred, y_true = test_result$team1win)

######################################################################################
#Random Forest
library(randomForest)
train_tree <- training_set
train_tree$team1win<- factor(train_tree$team1win, labels=c(0,1))#not for neuralnet

#Convert d_ss_tree$team1win to categorical values
test_set_rf <- test_set
test_set_rf$team1win <- NULL
test_set_rf$team1win <- sample(c(0, 1), nrow(test_set_rf), replace=TRUE)

colnames(test_set_rf)
colnames(train_tree)

test_set_rf <- test_set_rf[, col_order]
test_set_rf$team1win<- factor(test_set_rf$team1win, labels=c(0,1))

random_forest <- randomForest(formula = team1win ~.,
                              data=train_tree,
                              mtry=3,      # three predictor-vars selected randomly at each split
                              xtest=test_set_rf[-1],
                              ytest=test_set_rf$team1win,
                              #ytest=as.factor(audit_imp$Adjusted[testRows]),
                              importance=T,
                              ntree=500,   # acceptably large value to ensure each sample row is predicted
                              # at least 2-digit nbr of times on average
                              nodesize = 50,
                              maxnodes = 40,
                              norm.votes=T,
                              keep.forest=TRUE)

rf_predictions_prob <- predict(random_forest, test_set_rf, type='prob')
#rf_predictions_class <- predict(random_forest, test_set_rf, type='class')
d_ss <- d_ss %>% mutate(Pred = rf_predictions_prob[,2])#For prob
#d_ss <- d_ss %>% mutate(Pred = rf_predictions_class)

######################################################################################
#Model test
test_result <- merge(x = test_outcome_tournament, y = d_ss[2:5], by=c("team1id","team2id","season"), all = FALSE)

LogLoss(y_pred = test_result$Pred, y_true = test_result$team1win)

# if Accuracy
#test_result$Pred<- factor(test_result$Pred, labels=c(0,1))
#Accuracy(y_pred = test_result$Pred, y_true = test_result$team1win)

######################################################################################
# Example of Stacking algorithms
# create submodels
library(caret)
library(caretEnsemble)
library(knitr)
train_ensemble <- training_set
train_ensemble$diff_rank <- NULL
train_ensemble$elo_diff <- NULL
train_ensemble$team1win<- factor(train_ensemble$team1win, labels=c("win","loss"))#not for neuralnet


control <- trainControl(method="repeatedcv", number=10, repeats=10, savePredictions='all', classProbs=TRUE,summaryFunction = mnLogLoss)
algorithmList <- c('lda', 'glm', 'svmRadial')#knn disaster
#algorithmList <- c('rpart', 'glm','svmRadial')
set.seed(7)
metric <- "logLoss"
models <- caretList(team1win~., data=train_ensemble, trControl=control, methodList=algorithmList, metric=metric)



greedy_ensemble <- caretEnsemble(
  models, 
  metric="logLoss",
  trControl=control)
summary(greedy_ensemble)

kable(modelCor(resamples(models)))

summary(greedy_ensemble)
results <- resamples(models)
summary(results)
dotplot(results)
ensemble_pred <- predict(greedy_ensemble, newdata=test_set,type='prob')
d_ss <- d_ss %>% mutate(Pred = ensemble_pred)
######################################################################################
#Model test
test_result <- merge(x = test_outcome_tournament, y = d_ss[2:5], by=c("team1id","team2id","season"), all = FALSE)

LogLoss(y_pred = test_result$Pred, y_true = test_result$team1win)

d_ss_fin <- sample_submission %>% mutate(Pred = d_ss$Pred) #only matchup and prediction -> Results for kaggle
write.csv(d_ss_fin, "submission_stage_2.csv", row.names = FALSE)




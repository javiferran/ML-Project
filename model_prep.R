library(magrittr)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)

setwd("/Users/JaviFerrando/Desktop/MLProject/")
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


#Regularized Logistic Regression
#Total fail-> predictions wrong

train_glmnet <- train[,-(1:4)]
col_order <- colnames(train_glmnet)

set.seed(123)
library(glmnet)
x <- model.matrix(team1win~., train_glmnet)
y <- train_glmnet$team1win
cv.lasso <- cv.glmnet(x, y , alpha = 1, family = "binomial")
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
plot(cv.lasso)
# Display regression coefficients
coef(model)

#### glmnet test
d_ss_glmnet <- d_ss[,-1][,-(2:4)]
colnames(d_ss_glmnet)[1] <- "team1win"
head(d_ss_glmnet)


col_order
colnames(d_ss_glmnet)
d_ss_glmnet <- d_ss_glmnet[, col_order]

x.test <- model.matrix(team1win~., d_ss_glmnet)

x.test
head(x.test)

head(x)
head(d_ss)
head(train)
probabilities <- model %>% predict(newx = x.test)


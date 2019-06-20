library(magrittr)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggplot2)


#setwd("/Users/JaviFerrando/Desktop/NCAA Data-2018")



#logistic regression model: differences
model <- glm(team1win ~ 
               diff_rank +
               t1_rank_n +
               t1_season_elo +
               t2_season_elo +
               elo_prob_1 +
               t1_mpie + 
               t2_mpie +
               t1_netrtg +
               t2_netrtg
             ,
             
             data = train, family = binomial)
summary(model)


# Predict on every possible matchup
predict <- data.frame(Pred = predict(model, newdata = d_ss, type = 'response'))#

d_ss <- d_ss %>% mutate(Pred = predict$Pred)# %>% dplyr::select(ID, Pred) Change sample submission pred=0.5 to model predicition

# use original sample 
d_ss_fin <- sample_submission %>% mutate(Pred = d_ss$Pred) #only matchup and prediction

#sample_submission <- read.csv(paste(dir,'SampleSubmissionStage1.csv',sep=''))

# output 
write.csv(d_ss_fin, "submission_stage_2.csv", row.names = FALSE)

#test_outcome_tournament <- outcome_tournament %>% filter(season > 2013) #Test sample, target team1win

validation <- merge(x = test_outcome_tournament, y = d_ss[2:5], by=c("team1id","team2id","season"), all = FALSE)
validation$Pred

library(MLmetrics)
LogLoss(y_pred = validation$Pred, y_true = validation$team1win)

final_df <- data.frame(x = validation$Pred, y =validation$team1win)
colnames(final_df) <- c("Prediction","Result")


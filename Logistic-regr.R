library(magrittr)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggplot2)

#Shift + cmd + c
setwd("/Users/JaviFerrando/Desktop/MLProject")

# Get data
dseeds_tournament <- fread(paste(dir,'NCAATourneySeeds.csv',sep=''))
dg_tournment <- fread(paste(dir,'NCAATourneyCompactResults.csv',sep=''))


# keep only win loss and team ids for the dg_tournament data
outcome_tournament <- dg_tournment %>% select(Season, DayNum, WTeamID, LTeamID)
names(outcome_tournament) <- tolower(names(outcome_tournament))

# randomize winning and losing team into team 1 and team 2 (necessary for probabilities later) and drop other ids
outcome_tournament <- outcome_tournament %>% 
  mutate(rand = runif(dim(outcome_tournament)[1]), 
         team1id = ifelse(rand >= 0.5, wteamid, lteamid),
         team2id = ifelse(rand <0.5, wteamid, lteamid),
         team1win = ifelse(team1id == wteamid, 1, 0)) %>% 
  select(-rand, -wteamid,-lteamid)

# Add seeding information to games: 

# make seeds 1-16 without letters (except for certain seed)
dseeds_tournament <- dseeds_tournament %>% 
  mutate(ranking = as.factor((str_replace(Seed, "[A-Z]",""))), 
         rank_num = as.numeric(str_replace(ranking, ".[a-z]","")))
names(dseeds_tournament) <- tolower(names(dseeds_tournament))

# team 1
outcome_tournament <- outcome_tournament %>% 
  left_join(
    select(dseeds_tournament, t1_rank = ranking, t1_rank_n = rank_num, teamid, season), 
    by = c("team1id"="teamid","season"="season")) 

# team 2
outcome_tournament <- outcome_tournament %>% 
  left_join(
    select(dseeds_tournament, t2_rank = ranking, t2_rank_n = rank_num, teamid, season), 
    by = c("team2id"="teamid","season"="season")) 


# replace NA seeds
outcome_tournament <- outcome_tournament %>% mutate(t1_rank = ifelse(is.na(t1_rank), 8.5, t1_rank),
                                                    t2_rank = ifelse(is.na(t2_rank), 8.5, t2_rank),
                                                    t1_rank_n = ifelse(is.na(t1_rank_n), 8.5, t1_rank_n),
                                                    t2_rank_n = ifelse(is.na(t2_rank_n), 8.5, t2_rank_n),
                                                    diff_rank = t1_rank_n - t2_rank_n)



season_elos <- read.csv(paste(dir,'season_elos.csv',sep='')) %>% rename(teamid = team_id)

# Join team 1 data
outcome_tournament <- outcome_tournament %>% 
  left_join(
    select(season_elos, 
           season, 
           teamid, 
           t1_season_elo = season_elo),
    by = c("team1id" = "teamid","season" = "season"))


# Join team 2 data
outcome_tournament <- outcome_tournament %>% 
  left_join(
    select(season_elos, 
           season, 
           teamid, 
           t2_season_elo = season_elo),
    by = c("team2id" = "teamid","season" = "season"))


# Compute ELO probabilities for the game, and the difference in ELO scores

outcome_tournament <- outcome_tournament %>% 
  mutate(elo_diff = t1_season_elo - t2_season_elo,
         elo_prob_1 = 1/(10^(-elo_diff/400)+1)
  )

################################
#PIE feature
outcome_tournament <- outcome_tournament[outcome_tournament$season>=2003,]
# Join team 1 data
outcome_tournament <- outcome_tournament %>% 
  left_join(
    select(stats_season, 
           Season, 
           TeamID, 
           t1_mpie = MPIE),
    by = c("team1id" = "TeamID","season" = "Season"))

# Join team 2 data
outcome_tournament <- outcome_tournament %>% 
  left_join(
    select(stats_season, 
           Season, 
           TeamID, 
           t2_mpie = MPIE),
    by = c("team2id" = "TeamID","season" = "Season"))
################################
#Netrtg

# Join team 1 data
outcome_tournament <- outcome_tournament %>% 
  left_join(
    select(stats_season, 
           Season, 
           TeamID, 
           t1_netrtg = MNetRTG),
    by = c("team1id" = "TeamID","season" = "Season"))

# Join team 2 data
outcome_tournament <- outcome_tournament %>% 
  left_join(
    select(stats_season, 
           Season, 
           TeamID, 
           t2_netrtg = MNetRTG),
    by = c("team2id" = "TeamID","season" = "Season"))



### Load data

sample_submission <- read.csv(paste(dir,'SampleSubmissionStage1.csv',sep=''))


### Join team data and ranking data


d_ss <- sample_submission


# create individual team ids and season 
d_ss <- d_ss %>% mutate(season = as.numeric(gsub("(.*)_(.*)_(.*)",ID, replacement = "\\1")), 
                        team1id =  as.numeric(gsub("(.*)_(.*)_(.*)",ID, replacement = "\\2")),
                        team2id =  as.numeric(gsub("(.*)_(.*)_(.*)",ID, replacement = "\\3")))

# Add rank data

# team 1
d_ss <- d_ss %>% 
  left_join(
    dplyr::select(dseeds_tournament, t1_rank = ranking, t1_rank_n = rank_num, teamid, season), 
    by = c("team1id"="teamid","season"="season")) 

# team 2
d_ss <- d_ss %>% 
  left_join(
    dplyr::select(dseeds_tournament, t2_rank = ranking, t2_rank_n = rank_num, teamid, season), 
    by = c("team2id"="teamid","season"="season")) 



### Join ELO rating data
#season_elos <- read.csv("../input/fivethirtyeight-elo-ratings/season_elos.csv") %>% rename(teamid = team_id)
season_elos <- read.csv(paste(dir,'season_elos.csv',sep='')) %>% rename(teamid = team_id)

# Join team 1 data
d_ss <- d_ss %>% 
  left_join(
    select(season_elos, 
           season, 
           teamid, 
           t1_season_elo = season_elo),
    by = c("team1id" = "teamid","season" = "season"))


# Join team 2 data
d_ss <- d_ss %>% 
  left_join(
    select(season_elos, 
           season, 
           teamid, 
           t2_season_elo = season_elo),
    by = c("team2id" = "teamid","season" = "season"))


# Key differences between winner and loser

d_ss <- d_ss %>% 
  mutate(elo_diff = t1_season_elo - t2_season_elo,
         elo_prob_1 = 1/(10^(-elo_diff/400)+1),
         diff_rank = t1_rank_n - t2_rank_n
  )

#PIE feature
d_ss <- d_ss[d_ss$season>=2003,]
# Join team 1 data
d_ss <- d_ss %>% 
  left_join(
    select(stats_season, 
           Season, 
           TeamID, 
           t1_mpie = MPIE),
    by = c("team1id" = "TeamID","season" = "Season"))

# Join team 2 data
d_ss <- d_ss %>% 
  left_join(
    select(stats_season, 
           Season, 
           TeamID, 
           t2_mpie = MPIE),
    by = c("team2id" = "TeamID","season" = "Season"))

################################
#Netrtg

# Join team 1 data
d_ss <- d_ss %>% 
  left_join(
    select(stats_season, 
           Season, 
           TeamID, 
           t1_netrtg = MNetRTG),
    by = c("team1id" = "TeamID","season" = "Season"))

# Join team 2 data
d_ss <- d_ss %>% 
  left_join(
    select(stats_season, 
           Season, 
           TeamID, 
           t2_netrtg = MNetRTG),
    by = c("team2id" = "TeamID","season" = "Season"))


train <- outcome_tournament

### Make predictions based on model 
train <- train %>% filter(season <= 2013)

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


# predict on test set
predict <- data.frame(Pred = predict(model, newdata = d_ss, type = 'response'))
d_ss <- d_ss %>% mutate(Pred = predict$Pred)# %>% dplyr::select(ID, Pred)

# use original sample 
#d_ss_fin <- sample_submission %>% mutate(Pred = d_ss$Pred)

#sample_submission <- read.csv(paste(dir,'SampleSubmissionStage1.csv',sep=''))

# output 
#write.csv(d_ss_fin, "submission_03032018_2.csv", row.names = FALSE)

outcome_tournament <- outcome_tournament %>% filter(season > 2013)

validation <- merge(x = outcome_tournament, y = d_ss, by=c("team1id","team2id","season"), all = FALSE)

library(MLmetrics)
LogLoss(y_pred = validation$Pred, y_true = validation$team1win)

final_df <- data.frame(x = validation$Pred, y =validation$team1win)
colnames(final_df) <- c("Prediction","Result")


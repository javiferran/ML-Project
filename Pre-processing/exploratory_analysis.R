dir <- '/Users/Marcelpv96/Dropbox/MASTER/1rANY/Q2/ML/Project/ML-Project/input/'

library(data.table)
library(dplyr)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(ggExtra)
library(corrplot)


# LOAD USED CSV FILES #
teams <- fread(paste(dir,'Teams.csv',sep=''))
seasons <- fread(paste(dir,'Seasons.csv',sep=''))
seeds <- fread(paste(dir,'NCAATourneySeeds.csv',sep=''))
seas_results <- fread(paste(dir,'RegularSeasonCompactResults.csv',sep=''))
tour_results <- fread(paste(dir,'NCAATourneyCompactResults.csv',sep=''))
seas_detail <- fread(paste(dir,'RegularSeasonDetailedResults.csv',sep=''))
tour_detail <- fread(paste(dir,'NCAATourneyDetailedResults.csv',sep=''))
conferences <- fread(paste(dir,'Conferences.csv',sep=''))
team_conferences <- fread(paste(dir,'TeamConferences.csv',sep=''))
coaches <- fread(paste(dir,'TeamCoaches.csv',sep=''))
tour_enrich <- fread(paste(dir,'NCAATourneyDetailedResultsEnriched.csv',sep=''))
seas_enrich <- fread(paste(dir,'NCAASeasonDetailedResultsEnrichedv2.csv',sep=''))


# 1st HISTORICAL PERFORMANCE #
setkey(teams, TeamID)
setkey(seeds, TeamID)

g1 <-
  teams[seeds][, one_seed := as.numeric(substr(Seed, 2, 3)) == 1][, sum(one_seed), by = TeamName][order(V1, decreasing = T)][1:15,] %>%
  ggplot(aes(x = reorder(TeamName, V1), y = V1)) +
  geom_bar(stat = 'identity', fill = 'darkblue') +
  labs(x = '', y = 'No 1 seeds', title = 'No. 1 Seeds since 1985') +
  coord_flip()

setkey(seas_results, WTeamID)

g2 <-
  seas_results[teams][, .(wins = .N), by = TeamName][order(-wins)][1:15,] %>%
  ggplot(aes(x = reorder(TeamName, wins), y = wins)) +
  geom_bar(stat = 'identity', fill = 'darkblue') +
  labs(x = '', y = 'Wins', title = 'Regular Season Wins since 1985') +
  coord_flip()

setkey(tour_results, WTeamID)

g3 <-
  tour_results[teams][, .(wins = .N), by = TeamName][order(-wins)][1:15,] %>%
  ggplot(aes(x = reorder(TeamName, wins), y = wins)) +
  geom_bar(stat = 'identity', fill = 'darkblue') +
  labs(x = '', y = 'Wins', title = 'Tournament Wins since 1985') +
  coord_flip()

g4 <-
  tour_results[teams][DayNum == 154, .(wins = .N), by = TeamName][order(-wins)][1:15,] %>%
  ggplot(aes(x = reorder(TeamName, wins), y = wins)) +
  geom_bar(stat = 'identity', fill = 'darkblue') +
  labs(x = '', y = 'Championships', title = 'Tournament Championships since 1985') +
  coord_flip()

grid.arrange(g1, g2, g3, g4, nrow = 2)



# 2nd SEASON DETAILS - Relation between all statics with win or lose. #
win_stats <- seas_detail[, .(
  Season,
  TeamID = WTeamID,
  Outcome = rep('W', .N),
  FGM = WFGM,
  FGA = WFGA,
  FGP = WFGM / WFGA,
  FGP2 = (WFGM - WFGM3) / (WFGA - WFGA3),
  FGM3 = WFGM3,
  FGA3 = WFGA3,
  FGP3 = WFGM3 / WFGA3,
  FTM = WFTM,
  FTA = WFTA,
  FTP = WFTM / WFTA,
  OR = WOR,
  DR = WDR,
  AST = WAst,
  TO = WTO,
  STL = WStl,
  BLK = WBlk,
  PF = WPF,
  ORP = WOR / (WOR + LDR),
  DRP = WDR / (WDR + LOR),
  POS = 0.96 * (WFGA + WTO + 0.44 * WFTA - WOR)
)]

los_stats <- seas_detail[, .(
  Season,
  TeamID = LTeamID,
  Outcome = rep('L', .N),
  FGM = LFGM,
  FGA = LFGA,
  FGP = LFGM / LFGA,
  FGP2 = (LFGM - LFGM3) / (LFGA - LFGA3),
  FGM3 = LFGM3,
  FGA3 = LFGA3,
  FGP3 = LFGM3 / LFGA3,
  FTM = LFTM,
  FTA = LFTA,
  FTP = LFTM / LFTA,
  OR = LOR,
  DR = LDR,
  AST = LAst,
  TO = LTO,
  STL = LStl,
  BLK = LBlk,
  PF = LPF,
  ORP = (LOR / (LOR + WDR)),
  DRP = LDR / (LDR + WOR),
  POS = 0.96 * (LFGA + LTO + 0.44 * LFTA - LOR)
)]

stats_all <- rbindlist(list(win_stats, los_stats))

# Statics related with scoring : %Field goals, %2-pt, %3-pt field goals, Free throw goals. All with M/A ratio. 
# Which is, made / attempted ratio.


g1 <- stats_all %>%
  ggplot(aes(x = FGP, fill = Outcome)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c('darkblue', 'grey')) + 
  labs(x = 'Field goals %', y = '', title = 'Field Goal Shotting')

g2 <- stats_all %>%
  ggplot(aes(x = FGP2, fill = Outcome)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c('darkblue', 'grey')) + 
  labs(x = '2 pt Field goal %', y = '', title = '2 Pt Field Goal Shooting')

g3 <- stats_all %>%
  ggplot(aes(x = FGP3, fill = Outcome)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c('darkblue', 'grey')) + 
  labs(x = '3 pt Field goal %', y = '', title = '3 Pt Field Goal Shooting')

g4 <- stats_all %>%
  ggplot(aes(x = FTP, fill = Outcome)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c('darkblue', 'grey')) + 
  labs(x = 'Free throw %', y = '', title = 'Free Throw Shooting')


# Other statics that not are directly related with score

grid.arrange(g1, g2, g3, g4, ncol=2)


g5 <- stats_all %>%
  ggplot(aes(x = ORP, fill = Outcome)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c('darkblue', 'grey')) + 
  labs(x = 'Offensive rebound %', y = '', title = 'Offensive Rebounding Efficiency')

g6 <- stats_all %>%
  ggplot(aes(x = DRP, fill = Outcome)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c('darkblue', 'grey')) + 
  labs(x = 'Defensive rebouding %', y = '', title = 'Defensive Rebounding Efficiency')

g7 <- stats_all %>%
  ggplot(aes(x = AST, fill = Outcome)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c('darkblue', 'grey')) + 
  labs(x = 'Assists', y = '', title = 'Assists per Game')

g8 <- stats_all %>%
  ggplot(aes(x = TO, fill = Outcome)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c('darkblue', 'grey')) + 
  labs(x = 'Turnovers', y = '', title = 'Turnovers per Game')

g9 <- stats_all %>%
  ggplot(aes(x = STL, fill = Outcome)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c('darkblue', 'grey')) + 
  labs(x = 'Steals', y = '', title = 'Steals per Game')

g10 <- stats_all %>%
  ggplot(aes(x = BLK, fill = Outcome)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c('darkblue', 'grey')) + 
  labs(x = 'Blocks', y = '', title = 'Blocks per Game')

grid.arrange(g5, g6, g7, g8, g9, g10, ncol = 2)



# When advanced statistics are applied:
seas_enriched <- fread("NCAASeasonDetailedResultsEnriched.csv")

win_advanced_stats <- seas_enriched[, .(
  Season,
  TeamID = WTeamID,
  Outcome = rep('W', .N),
  FGM = WFGM,
  Pos = WPos,
  OffRtg = WOffRtg,
  DefRtg = WDefRtg,
  NetRtg = WNetRtg,
  PIE = WPIE
)]

los_advanced_stats <- seas_enriched[, .(
  Season,
  TeamID = WTeamID,
  Outcome = rep('W', .N),
  FGM = LFGM,
  Pos = LPos,
  OffRtg = LOffRtg,
  DefRtg = LDefRtg,
  NetRtg = LNetRtg,
  PIE = LPIE
)]


gPie <- win_advanced_stats %>%
  ggplot(aes(x = BLK, fill = Outcome)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c('darkblue', 'grey')) + 
  labs(x = 'Blocks', y = '', title = 'Blocks per Game')


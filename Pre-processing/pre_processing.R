get.outcome_tournament <- function(dg_tournment, dseeds_tournament){
  # keep only season, daynum, win and loss team ids for the dg_tournament data
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
                                                      t2_rank_n = ifelse(is.na(t2_rank_n), 8.5, t2_rank_n))
  # Only data from season 2003>=
  
  return(outcome_tournament[outcome_tournament$season>=2003,])
}


outcome_tournament <- get.outcome_tournament(dg_tournment, dseeds_tournament)
season_elos <- read.csv(paste(dir,'season_elos.csv',sep='')) %>% rename(teamid = team_id)


add.season_elos <- function(outcome_tournament, season_elos){
  #Add season_elos (for t1 and t2) to outcome tournament
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
           elo_prob_1 = 1/(10^(-elo_diff/400)+1),
           diff_rank = t1_rank_n - t2_rank_n)
    
  
  return(outcome_tournament)
}


get.win_stats <- function(seas_enrich){
  win_stats <- seas_enrich[, .(
    Season,
    TeamID = WTeamID,
    Result = rep('W', .N),
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
    PIE = WPIE,
    ORP = WOR / (WOR + LDR),
    DRP = WDR / (WDR + LOR),
    eFG = WeFGP,
    NetRTG = WNetRtg,
    POS = 0.96 * (WFGA + WTO + 0.44 * WFTA - WOR)
  )]
  return(win_stats)
}

get.los_stats <- function(seas_enrich){
  los_stats <- seas_enrich[, .(
    Season,
    TeamID = LTeamID,
    Result = rep('L', .N),
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
    PIE = LPIE,
    ORP = (LOR / (LOR + WDR)),
    DRP = LDR / (LDR + WOR),
    eFG = LeFGP,
    NetRTG = LNetRtg,
    POS = 0.96 * (LFGA + LTO + 0.44 * LFTA - LOR)
  )]
  return(los_stats)
}


get.stats_season <- function(seas_enrich){
  win_stats <- get.win_stats(seas_enrich)
  los_stats <- get.los_stats(seas_enrich)
  stats_all <- rbindlist(list(win_stats, los_stats))
  stats_season <- stats_all[, .(
    FGP = sum(FGM) / sum(FGA),
    FGP3 = sum(FGM3) / sum(FGA3),
    FTP = sum(FTM) / sum(FTA),
    ORPG = mean(OR),
    DRPG = mean(DR),
    ASPG = mean(AST),
    TOPG = mean(TO),
    STPG = mean(STL),
    #BLPG = mean(BLK),
    #PFPG = mean(PF),
    MeFG = mean(eFG),
    MNetRTG = mean(NetRTG),
    #MORP = mean(ORP),
    MPIE = mean(PIE),
    MPOS = mean(POS),
    EFG = (mean(FGM)+0.5*mean(FGM3))/mean(FGA))
    
    , by = c('TeamID', 'Season')]
  return(stats_season)
}


add.advanced_Feature  <- function(stats_season, outcome_tournament, feature){
  # Join team 1 data
  outcome_tournament <- outcome_tournament %>% 
    left_join(
      select(stats_season, 
             Season, 
             TeamID, 
             feature_name = feature),
      by = c("team1id" = "TeamID","season" = "Season"))
  
  # Join team 2 data
  outcome_tournament <- outcome_tournament %>% 
    left_join(
      select(stats_season, 
             Season, 
             TeamID, 
             feature_name = feature),
      by = c("team2id" = "TeamID","season" = "Season"))
  columns_names <- colnames(outcome_tournament)
  columns_names <- columns_names[1:(length(columns_names)-2)]
  colnames(outcome_tournament) <- c(columns_names, c(paste('t1', feature, sep="_"), paste('t2', feature, sep="_")))
  return(outcome_tournament)
}


get.sample_submission <- function(sample_submission, dseeds_tournament){
  d_ss <- sample_submission
  
  # Add season, team1id and team2id columns from sample submission ID
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

  return(d_ss[d_ss$season>=2003,])
}



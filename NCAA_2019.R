#setwd("/Users/JaviFerrando/Dropbox Javi/Dropbox/Msc Data Science UPC/MVA/6th homework")

setwd("/Users/JaviFerrando/Desktop/MLProject")

dir <- '/Users/JaviFerrando/Desktop/MLProject/input/'

# Data Section 1
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
seas_enrich <- fread(paste(dir,'NCAASeasonDetailedResultsEnriched.csv',sep=''))

require(tidyverse)

source('./code/cleaning/CleaningUtilities.R')

# take in a year, output team's advanced stats per game
year = 2022

pbp = read_csv(paste0('./data/raw/PBP/PBP_', year, '.csv'))
box = read_csv(paste0('./data/raw/teamBox/teamBox_', year, '.csv'))
playerBox = read_csv(paste0('./data/raw/playerBox/playerBox_', year, '.csv'))
pbp_noGarbageTime = FilterGarbageTime(pbp, playerBox)
adv = AggregateAdvanced(pbp_noGarbageTime, box)

#rm(list=c("pbp", "box", "pbp_noGarbageTime", "playerBox"))

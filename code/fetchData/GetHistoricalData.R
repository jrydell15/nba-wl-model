require(hoopR)
require(tidyverse)

seasons = seq(2002, 2022)

for (season in seasons) {
  pbp = load_nba_pbp(season)
  
  write_csv(pbp, file=paste0('./data/raw/PBP/PBP_', season, '.csv'))
  rm(pbp)
}

for (season in seasons) {
  teamBox = load_nba_team_box(season)
  playerBox = load_nba_player_box(season)
  
  write_csv(teamBox, file=paste0('./data/raw/teamBox/teamBox_', season, '.csv'))
  write_csv(playerBox, file=paste0('./data/raw/playerBox/playerBox_', season, '.csv'))
  rm(playerBox)
  rm(teamBox)
}

require(hoopR)
require(tidyverse)

seasons = seq(2002, most_recent_nba_season()-1)

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

for (season in seasons) {
  schedule = load_nba_schedule(season) %>%
              select(game_id, date, season, season_type, venue.id, home.id, away.id)
  write_csv(schedule, file=paste0('./data/raw/schedule/schedule', season, '.csv'))
  rm(schedule)
}

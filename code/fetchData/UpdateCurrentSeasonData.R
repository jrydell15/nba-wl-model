require(hoopR)
require(tidyverse)

season = most_recent_nba_season()

pbp = load_nba_pbp(season)

write_csv(pbp, file=paste0('./data/raw/PBP/PBP_', season, '.csv'))
rm(pbp)

teamBox = load_nba_team_box(season)
playerBox = load_nba_player_box(season)

write_csv(teamBox, file=paste0('./data/raw/teamBox/teamBox_', season, '.csv'))
write_csv(playerBox, file=paste0('./data/raw/playerBox/playerBox_', season, '.csv'))
rm(playerBox)
rm(teamBox)

schedule = load_nba_schedule(season) %>%
  select(game_id, date, season, season_type, venue.id, home.id, away.id)
write_csv(schedule, file=paste0('./data/raw/schedule/schedule', season, '.csv'))
rm(schedule)
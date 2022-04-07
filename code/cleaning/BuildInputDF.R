require(here)
require(tidyverse)
require(lubridate)
require(hoopR)

source(here('code/cleaning/CleaningUtilities.R'))
venueDists = read_csv(here('data/raw/misc/venueDistances.csv'))

startYear = 2002
endYear   = most_recent_nba_season()

for (year in startYear:endYear) {
  schedule = read_csv(here(paste0('data/raw/schedule/schedule', year, '.csv')))
  teamBox = read_csv(here(paste0('data/raw/teamBox/teamBox_', year, '.csv')))
  aggDF = read_csv(here(paste0('data/aggregated/agg_', year, '.csv')))
  
  if (year == year(today())) {
    schedule = schedule %>%
      filter(date < today())
    
    teamBox = teamBox %>%
      filter(game_date < today())
  }
  
  winnerDF = GetGameWinners(teamBox)
  
  leftdf = schedule %>%
    arrange(date) %>%
    pivot_longer(cols=c('home.id', 'away.id'), values_to='teamID', names_to='loc') %>% 
    mutate(loc = as.factor(ifelse(str_detect(loc, 'home'), 'H', 'A'))) %>%
    left_join(winnerDF, by='game_id') %>%
    group_by(teamID) %>%
    mutate(teamGameNum = row_number(),
           lastVenue = lag(venue.id),
           lastGameDate = lag(date)) %>%
    ungroup() %>%
    mutate(restDays = as.integer(date - lastGameDate),
           win = as.factor(ifelse(loc == 'H', home_win, ifelse(home_win == 1, 0, 1)))) %>%
    left_join(venueDists,
              by = c('venue.id' = 'toVenueID', 'lastVenue' = 'fromVenueID')) %>%
    mutate(dist_miles = replace_na(dist_miles, 0),
           btb = ifelse(restDays == 1 | restDays == 0, 1, 0),
           btb = replace_na(btb, 0),
           across(btb, as.factor),
           logTravelDist = log(dist_miles + 1),
           restDays = ifelse(restDays == 0, restDays, restDays - 1),
           restDays = replace_na(restDays, -99)) %>% 
    select(game_id, date, teamID, teamGameNum, loc, btb, logTravelDist, restDays, win) %>% 
    left_join(aggDF %>%
                select(game_id, team_id, contains("Rat")),
              by=c('game_id', 'teamID' = 'team_id')) %>%
    arrange(date, game_id)
  
  set.seed(42)
  
  outdf = leftdf %>%
    left_join(leftdf %>% select(-date), by=c('game_id'), suffix=c('', '_opp')) %>%
    filter(teamID != teamID_opp) %>%
    group_by(game_id) %>%
    slice_sample(n=1) %>%
    ungroup() %>%
    arrange(date) %>%
    select(-date, -loc_opp, -win_opp)
  
  write_csv(outdf, file=here(paste0('data/input/input_', year, '.csv')))
}

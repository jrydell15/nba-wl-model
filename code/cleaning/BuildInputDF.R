require(here)
require(tidyverse)
require(lubridate)
require(hoopR)
require(glue)

source(here('code/cleaning/CleaningUtilities.R'))
venueDists = read_csv(here('data/raw/misc/venueDistances.csv'), show_col_types = FALSE)

BuildInputDFs = function(years) {

  for (year in years) {
    print(year)
    schedule = read_csv(here(glue('data/raw/schedule/schedule{year}.csv')), show_col_types = FALSE)
    teamBox = read_csv(here(glue('data/raw/teamBox/teamBox_{year}.csv')), show_col_types = FALSE)
    centeredScaledDF = read_csv(here(glue('data/centeredScaled/CS_{year}.csv')), show_col_types = FALSE)
    rawLaggedDF = read_csv(here(glue('data/lagged/lag_{year}.csv')), show_col_types = FALSE)
    
    
    if (year == year(today())) {
      schedule = schedule %>%
        filter(date < today())
      
      teamBox = teamBox %>%
        filter(game_date < today())
    }
    
    winnerDF = GetGameWinners(teamBox)
    
    leftdf = schedule %>%
      pivot_longer(cols=c('home.id', 'away.id'), values_to='teamID', names_to='loc') %>% 
      mutate(loc = as.factor(ifelse(str_detect(loc, 'home'), 'H', 'A'))) %>%
      left_join(winnerDF, by='game_id') %>%
      mutate(win = as.factor(ifelse(loc == 'H', home_win, ifelse(home_win == 1, 0, 1)))) %>%
      na.omit() %>%
      filter(teamID <= 30) %>%
      arrange(date) %>%
      group_by(teamID) %>%
      mutate(teamGameNum = row_number(),
             lastVenue = lag(venue.id),
             lastGameDate = lag(date),
             teamWins = cumsum(as.integer(win) - 1),
             cum_winPct = teamWins / teamGameNum) %>%
      ungroup() %>%
      mutate(restDays = day(as.period(date - lastGameDate))) %>%
      left_join(venueDists,
                by = c('venue.id' = 'toVenueID', 'lastVenue' = 'fromVenueID')) %>%
      mutate(dist_miles = replace_na(dist_miles, 0),
             btb = ifelse(restDays == 1 | restDays == 0, 1, 0),
             btb = replace_na(btb, 0),
             across(btb, as.factor),
             logTravelDist = log(dist_miles + 1),
             restDays = ifelse(restDays == 0, restDays, restDays - 1),
             restDays = replace_na(restDays, -99)) %>% 
      select(game_id, date, teamID, cum_winPct, teamGameNum, loc, btb, logTravelDist, restDays, win)
  
      inputCenteredScaled = leftdf %>%
        mutate(date = date(date)) %>%
        left_join(centeredScaledDF,
                  by=c('date', 'teamID' = 'team_id')) %>% 
        na.omit()

      inputLagged = leftdf %>%
        mutate(date = date(date)) %>%
        left_join(rawLaggedDF,
                  by=c('date', 'game_id', 'teamID'='team_id')) %>%
        na.omit()
    
    set.seed(42)
    
    outCenteredScaled = inputCenteredScaled %>%
      left_join(inputCenteredScaled %>% select(-date), by=c('game_id'), suffix=c('', '_opp')) %>%
      filter(teamID != teamID_opp) %>%
      group_by(game_id) %>%
      slice_sample(n=1) %>%
      ungroup() %>%
      arrange(date) %>%
      select(-date, -loc_opp, -win_opp)
    
    outLagged = inputLagged %>%
      left_join(inputLagged %>% select(-date), by=c('game_id'), suffix=c('', '_opp')) %>%
      filter(teamID != teamID_opp) %>%
      group_by(game_id) %>%
      slice_sample(n=1) %>%
      ungroup() %>%
      arrange(date) %>%
      select(-date, -loc_opp, -win_opp)
    
    write_csv(outCenteredScaled, file=here(glue('data/input/CS/input_CS_{year}.csv')))
    write_csv(outLagged, file=here(glue('data/input/lagged/input_lag_{year}.csv')))
  }
}

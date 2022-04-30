require(here)
require(tidyverse)
require(lubridate)
require(hoopR)
require(glue)

# requires all raw and lagged csvs to be up to date
# get sources and load in relevant data
source(here('code/cleaning/CleaningUtilities.R'))

year = most_recent_nba_season()

teams = espn_nba_teams() %>%
          select(team_id, display_name) %>%
          mutate(across(team_id, as.integer))

teamBox = read_csv(here(glue('data/raw/teamBox/teamBox_{year}.csv')),
                   show_col_types = FALSE) %>%
          filter(game_date <= today())

schedule = read_csv(here(glue('data/raw/schedule/schedule{year}.csv')),
                    show_col_types = FALSE) %>%
          filter(date <= today())

lastDay = read_csv(here(glue('data/lagged/lag_{year}.csv')),
                   show_col_types = FALSE) %>%
          arrange(date) %>%
          group_by(team_id) %>%
          slice_tail(n=1) %>%
          ungroup() %>%
          left_join(teams, by='team_id') %>%
          select(c(date, team_id, display_name, contains('cum_')))

# get win pct
winnerDF = GetGameWinners(teamBox)

winPcts = schedule %>%
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
  slice_tail(n=1) %>%
  ungroup() %>%
  left_join(teams, by=c('teamID' = 'team_id')) %>%
  select(display_name, cum_winPct)

# combine
out = lastDay %>%
  left_join(winPcts, by='display_name') %>%
  select(-team_id)

write_csv(out, file=here('nbaShiny/data/input.csv'))

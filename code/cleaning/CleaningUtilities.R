if(!require(hoopR)) {install.packages("hoopR"); require(hoopR)}
if(!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}

FixShotLocations = function(df) {
  # input:   raw pbp dataframe
  # output:  fixed pbp dataframe
  # changes: - fixes the coordinate_x to have the basket be (0,0)
  #          - adds a dist_from_basket column
  #          - adds shot_amount column
  #               - 1 for FTA
  #               - 2 for FGA that isn't 3 pointer
  #               - 3 for FG3A
  #               - NA for all else
  #
  # usage:    newdf = FixShotLocations(olddf)
  
  df = df %>%
    mutate(coordinate_x = coordinate_x - 25,
           dist_from_basket = sqrt(coordinate_x^2 + coordinate_y^2),
           shot_amount = ifelse(shooting_play == TRUE, 0, NA))
  
  dfthrees = df %>%
    filter(abs(coordinate_x) < 50, shooting_play == TRUE) %>%
    filter(dist_from_basket >= 23.75 | (coordinate_y < 14 & abs(coordinate_x) >= 22)) %>%
    mutate(shot_amount = 3)
  
  dffts = df %>%
    filter(abs(coordinate_x) > 100, shooting_play == TRUE) %>%
    mutate(shot_amount = 1)
  
  dftwos = df %>%
    filter(abs(coordinate_x) < 50, shooting_play == TRUE) %>%
    filter((coordinate_y < 14 & abs(coordinate_x) < 22) | (dist_from_basket < 23.75 & coordinate_y >= 14),
           !str_detect(text, "three point")) %>%
    mutate(shot_amount = 2)
  
  df = df %>%
    filter(shooting_play == FALSE) %>%
    rbind(dfthrees) %>%
    rbind(dffts) %>%
    rbind(dftwos) %>%
    mutate(across(sequence_number, as.integer)) %>%
    arrange(sequence_number)
  
  return(df)
}

GetPossessions = function(df, agg=FALSE) {
  # input:  -df: either raw or fix pbp df
  #         -agg: TRUE for 2x2 table, FALSE for full pbp df
  #
  # output: 2x2 table with columns being team_id and team possession count
  #         or pbp df with new_poss column added
  #
  # usage:  possTable = GetPossessions(df)
  #
  # while not perfect, offers a pretty good estimation of
  # the number of possessions in a game
  
  simpleplaytypes = read.csv('./code/cleaning/shottypessimple.csv')
  
  df = df %>%
    mutate(across(type_id, as.integer),
           across(sequence_number, as.integer)) %>%
    arrange(game_id, sequence_number) %>%
    left_join(simpleplaytypes, by="type_id") %>%
    rename(event = simple_text) %>%
    filter(event != "Substitution",
           event != "Timeout",
           event != "Challenge",
           event != "Delay of Game") %>%
    mutate(prevteam = lag(team_id),
           prevevent = lag(event),
           game_event = row_number(),
           prevscore = lag(scoring_play),
           new_poss = 0,
           new_poss = ifelse(event == "Foul", 0, new_poss),
           new_poss = ifelse(prevevent == "Kicked Ball" & team_id != prevteam, 1, new_poss),
           new_poss = ifelse(event == "Rebound" & team_id != prevteam, 1, new_poss),
           new_poss = ifelse(event == "Free Throw", 0, new_poss),
           new_poss = ifelse(prevscore == TRUE & team_id != prevteam & event != "Foul", 1, new_poss),
           new_poss = ifelse(prevscore == TRUE & team_id != prevteam & type_id == 42, 1, new_poss),
           new_poss = ifelse(prevevent == "Turnover", 1, new_poss),
           new_poss = ifelse(team_id == prevteam & event == "Turnover" & prevevent == "Foul", 0, new_poss),
           new_poss = ifelse(team_id != prevteam & prevevent == "Foul" & lag(scoring_play, 2) == TRUE & lag(team_id, 2) != team_id, 1, new_poss),
           new_poss = ifelse(team_id == prevteam & prevevent == "Turnover" & event == "Foul", 0, new_poss),
           new_poss = ifelse(lag(type_id, 2) == 84 & event == "Free Throw" & team_id != prevteam, 1, new_poss),
           new_poss = ifelse(is.na(prevteam), 1, new_poss),
           new_poss = ifelse(is.na(team_id), 0, new_poss),
           new_poss = ifelse(shooting_play != TRUE & str_detect(lead(event, 1), "End"), 0, new_poss)) %>%
    filter(!is.na(team_id))
  
  if (agg) {
    return(df %>% group_by(team_id) %>%
             summarize(poss = sum(new_poss)))
  } else {
    return(df)
  }
}

GetStarters = function(g_id, playerbox=load_nba_player_box()) {
  # inputs: -playerbox: df of player box scores (output of load_nba_player_box())
  #         -g_id: game_id
  # output: 2x2 tibble of team_id and lineup (comma separated list of athlete_id's)
  # usage: teamStarters = GetStarters(playerbox, g_id)  
  
  return(playerbox %>% 
           filter(starter == TRUE,
                  game_id == g_id) %>%
           select(players = athlete_id, team_id, game_id) %>%
           mutate(across(players, as.integer)) %>%
           arrange(game_id, team_id, players) %>%
           group_by(team_id) %>%
           summarize(lineup = paste0(players, collapse=', '),
                     game_id = game_id,
                     .groups = 'keep') %>%
           distinct() %>%
           ungroup())
}

SortLineup = function(lineupString) {
  # inputs: -lineupString: comma separated string of the lineup ids
  # output: sorted lineup string (small -> large)
  #
  # purpose: so that it will be easy to aggregate lineup data later
  
  return(lineupString %>%
           strsplit(', ') %>%
           sapply(., as.integer) %>%
           sort() %>%
           paste0(., collapse=', '))
}

GetGameWinners = function(df) {
  # input: -df: dataframe of box scores (output of load_nba_team_box())
  # output: n x 2 tibble of game_id and home_win (1 if home team won game)
  # usage: winnerdf = GetGameWinners(boxscoredf)
  
  return(df %>%  
           select(game_id, team_id, team=team_short_display_name, home_away, contains("attempted")) %>%
           separate(field_goals_made_field_goals_attempted, into=c("FGM", "FGA")) %>%
           separate(three_point_field_goals_made_three_point_field_goals_attempted, into=c("FG3M", "FG3A")) %>%
           separate(free_throws_made_free_throws_attempted, into=c("FTM", "FTA")) %>%
           select(-c("FGA", "FG3A", "FTA")) %>%
           mutate(across(FGM:FTM, as.integer)) %>%
           mutate(score = FTM + 2*(FGM - FG3M) + 3*FG3M) %>%
           select(game_id, home_away, score) %>%
           spread(home_away, score) %>%
           mutate(home_win = ifelse(AWAY > HOME, 0, 1)) %>%
           select(game_id, home_win))
}

idToTeamName = function(df, teamsdf=espn_nba_teams()) {
  # inputs: -df: df that has a team_id column that you want to switch
  #                 to a column that is team names
  #         -teamsdf: df output of espn_nba_teams()
  # output: original df replacing the team_id column with the team column 
  # usage: newdf = idToTeamName(olddf, teamsdf)
  
  return(df %>%
           left_join(teamsdf %>%
                       select(team_id, team=short_name) %>%
                       distinct(team_id, team),
                     by="team_id") %>%
           select(team, names(df), -team_id))
}

GetStandings = function(box=NA, year=most_recent_nba_season()) {
  # inputs: -box: df output of load_nba_team_box()
  # output: 30x3 tibble (team, wins, losses)
  # usage: df = GetStandings()
  
  if (is.null(dim(box))) {
    box = load_nba_team_box(year)
  }
  
  return(box %>% 
           left_join(box %>%
                       GetGameWinners(.),
                     by="game_id") %>%
           mutate(winner = ifelse(home_away == "HOME", home_win, !home_win),
                  across(team_id, as.integer)) %>%
           filter(season_type == 2,
                  team_id < 1000) %>%
           group_by(team_id) %>%
           mutate(teamgamenum = row_number()) %>%
           ungroup() %>%
           filter(teamgamenum <= 82) %>%
           group_by(team_id) %>%
           summarize(wins = sum(winner),
                     totalGames = n(),
                     losses = totalGames - wins) %>%
           ungroup() %>%
           mutate(across(team_id, as.character)) %>%
           idToTeamName(.) %>%
           select(team, wins, losses) %>%
           arrange(-wins))
}

AddLineupsToPBP = function(g_id, pbp=load_nba_pbp(), playerbox=load_nba_player_box()) {
  # inputs: -g_id: game_id
  #         -pbp: pbp df (output of load_nba_pbp())
  #         -playerbox: player box score df (output of load_nba_player_box())
  # output: df with game_id, sequence_number, home_lineup, away_lineup
  # usage: newdf = AddLineupsToPBP(game_id, pbp, playerbox)
  #
  # recommendation: pre-load the pbp and playerbox and feed them into the function
  #                 if pulling multiple games. Only use defaults if you're pulling
  #                 a one-off game.
  
  game = pbp %>%
    filter(game_id == g_id)
  
  gamestarters = GetStarters(g_id, playerbox)
  
  lineupdf = game %>% filter(type_text == "Substitution") %>%
    select(game_id, team_id, sequence_number, text,
           player_in = participants_0_athlete_id,
           player_out = participants_1_athlete_id) %>%
    mutate(event_number = row_number()) %>%
    group_by(game_id, team_id) %>%
    transmute(sequence_number, text, player_in,
              player_out, event_number,
              sub_event = row_number(),
              lineup = '')
  
  for (i in 1:max(lineupdf$event_number)) {
    if ((lineupdf[[i, 'sub_event']] == 1)) {
      lineupdf[[i, 'lineup']] = lineupdf %>%
        filter(event_number == i) %>%
        left_join(gamestarters, by=c("game_id", "team_id")) %>%
        mutate(newlineup = str_replace(lineup.y, player_out, player_in)) %>%
        pull(newlineup) %>%
        SortLineup(.)
    } else {
      lineupdf[[i, 'lineup']] = lineupdf %>%
        filter(event_number == i |
                 sub_event == (lineupdf %>% filter(event_number == i) %>% pull(sub_event)) - 1,
               team_id == (lineupdf %>% filter(event_number == i) %>% pull(team_id))) %>%
        mutate(newlineup = str_replace(lag(lineup), player_out, player_in)) %>%
        filter(event_number == i) %>%
        pull(newlineup) %>%
        SortLineup(.)
    }
  }
  
  return(game %>% 
           select(game_id, team_id, sequence_number, away_team_id, home_team_id, type_text) %>%
           mutate(home_lineup = ifelse(row_number() == 1, (gamestarters %>% filter(team_id == (game %>% filter(row_number() == 1) %>% pull(home_team_id))) %>% pull(lineup)), NA),
                  away_lineup = ifelse(row_number() == 1, (gamestarters %>% filter(team_id == (game %>% filter(row_number() == 1) %>% pull(away_team_id))) %>% pull(lineup)), NA)) %>%
           left_join(lineupdf, by=c("game_id", "team_id", "sequence_number")) %>%
           select(-c(player_in:event_number, text)) %>%
           mutate(home_lineup = ifelse(team_id == home_team_id & !is.na(lineup), lineup, home_lineup),
                  away_lineup = ifelse(team_id == away_team_id & !is.na(lineup), lineup, away_lineup),
                  across(sequence_number, as.integer)) %>%
           fill(home_lineup, away_lineup) %>%
           select(game_id, sequence_number, home_lineup, away_lineup))
}

PlayerIDToPlayerName = function(g_id, df, playerbox) {
  # inputs: -g_id: game_id
  #         -df: df after being run through AddLineupsToPBP()
  #         -playerbox: player box score df (output of load_nba_player_box())
  # output: df with game_id, sequence_number, home_lineup, away_lineup (player names instead of player ids)
  # usage: newdf = PlayerIDToPlayerName(game_id, df, playerbox)
  #
  # recommendation: pre-load the pbp and playerbox and feed them into the function
  #                 if pulling multiple games. Only use defaults if you're pulling
  #                 a one-off game.
  
  df = df %>% filter(game_id == g_id)
  
  return(df %>%
           separate(home_lineup, paste0("home_", 1:5), ', ') %>%
           separate(away_lineup, paste0("away_", 1:5), ', ') %>%
           gather(key='team', value='athlete_id', home_1:away_5) %>%
           mutate(team = ifelse(grepl("home", team), "home_lineup", "away_lineup")) %>%
           left_join(playerbox %>%
                       distinct(athlete_id, athlete_display_name, game_id),
                     by=c("game_id", "athlete_id")) %>%
           group_by(game_id, sequence_number, team) %>%
           summarize(lineup = paste0(athlete_display_name, collapse=', '),
                     .groups='keep') %>%
           ungroup() %>%
           spread(team, lineup))
}
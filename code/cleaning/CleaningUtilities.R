if(!require(hoopR)) {install.packages("hoopR"); require(hoopR)}
if(!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}
require(here)

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
           raw_y = coordinate_y,
           coordinate_y = coordinate_y + 5.25,
           dist_from_basket = sqrt(coordinate_x^2 + raw_y^2),
           shot_amount = ifelse(shooting_play == TRUE, 0, NA),
           pitp = ifelse(shooting_play == TRUE, 0, NA))
  
  dfthrees = df %>%
    filter(!str_detect(type_text, 'Free Throw'), shooting_play == TRUE) %>%
    filter(dist_from_basket >= 23.75 | (raw_y < 14 & abs(coordinate_x) >= 22)) %>%
    mutate(shot_amount = 3)
  
  dffts = df %>%
    filter(str_detect(type_text, 'Free Throw'), shooting_play == TRUE) %>%
    mutate(shot_amount = 1)
  
  dftwos = df %>%
    filter(!str_detect(type_text, 'Free Throw'), shooting_play == TRUE) %>%
    filter((raw_y < 14 & abs(coordinate_x) < 22) | (dist_from_basket < 23.75 & raw_y >= 14),
           !str_detect(text, "three point")) %>%
    mutate(shot_amount = 2,
           pitp = ifelse(coordinate_y <= 18.8 & abs(coordinate_x) <= 8, 1, pitp))
  
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
  
  simpleplaytypes = read.csv(here('code/cleaning/shottypessimple.csv'))

  
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

GetStarters = function(playerbox=load_nba_player_box()) {
  # inputs: -playerbox: df of player box scores (output of load_nba_player_box())
  # output: 2x2 tibble of team_id and lineup (comma separated list of athlete_id's)
  # usage: teamStarters = GetStarters(playerbox, g_id)  
  
  return(playerbox %>% 
           filter(starter == TRUE) %>%
           select(players = athlete_id, team_id, game_id) %>%
           mutate(across(players, as.integer),
                  across(team_id, as.character)) %>%
           arrange(game_id, team_id, players) %>%
           group_by(game_id, team_id) %>%
           summarize(lineup = paste0(players, collapse = ', '),
                     game_id = game_id,
                     .groups = 'keep') %>%
           distinct() %>%
           ungroup())
}

GetStartersLeftInGame = function(pbp, playerbox) {
  starters = GetStarters(playerbox) %>%
    group_by(game_id) %>%
    summarize(allStarters = paste0(lineup, collapse=', ')) %>%
    distinct() %>%
    ungroup()
  
  return(AddLineupsToPBP(pbp, playerbox) %>%
           left_join(starters, by="game_id") %>%
           rowwise() %>%
           mutate(allLineup = paste0(c(lineup_away, lineup_home), collapse=', '),
                  nStarters = sum(as.integer(unlist(strsplit(allLineup, ', '))) %in% as.integer(unlist(strsplit(allStarters, ', '))))) %>%
           ungroup() %>%
           select(game_id, sequence_number, nStarters) %>%
           mutate(across(sequence_number, as.character)))
}

GetStartOfGarbageTime = function(pbp, playerbox) {
  startersLeftdf = GetStartersLeftInGame(pbp, playerbox)
  
  return(pbp %>% 
           select(game_id, sequence_number, home_score, away_score, qtr, clock_minutes) %>%
           mutate(across(sequence_number, as.character)) %>%
           left_join(startersLeftdf, by=c("game_id", "sequence_number")) %>% 
           filter(qtr == 4) %>%
           group_by(game_id) %>%
           mutate(scoreDiff = abs(away_score - home_score),
                  scoreFilter = ifelse(qtr == 4, 
                                       ifelse(clock_minutes > 8 & scoreDiff >= 25,
                                              1,
                                              ifelse(clock_minutes > 5 & scoreDiff >= 20,
                                                     1,
                                                     ifelse(scoreDiff >= 10,
                                                            1, 0))),
                                       0),
                  garbageTime = ifelse(scoreFilter == 1 & nStarters <= 2, 1, 0),
                  across(sequence_number, as.integer)) %>%
           arrange(-sequence_number) %>%
           mutate(lastGarbageTime = lead(garbageTime)) %>%
           filter(garbageTime == 1, lastGarbageTime == 0) %>%
           filter(sequence_number == max(sequence_number)) %>%
           select(game_id, startOfGarbage = sequence_number) %>%
           ungroup() %>%
           mutate(across(startOfGarbage, as.character)))
}

FilterGarbageTime = function(pbp, playerbox) {
  return(pbp %>%
           left_join(GetStartOfGarbageTime(pbp, playerbox), by='game_id') %>%
           mutate(across(sequence_number, as.integer),
                  across(startOfGarbage, as.integer)) %>%
           mutate(garb = ifelse(sequence_number >= startOfGarbage, 1, 0)) %>%
           filter(garb == 0 | is.na(garb)) %>%
           select(-c("startOfGarbage", "garb")) %>%
           mutate(across(sequence_number, as.character)))
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

GetGameScores = function(pbp, box) {
  # input: -pbp: dataframe of pbp (output of load_nba_pbp())
  #        -box: dataframe of team box (output of load_nba_team_box())
  # output: n x 4 tibble of game_id, team_id, score_o, score_d
  # usage: scoredf = GetGameWinners(pbp)
  
  combo = pbp %>%
    group_by(game_id) %>%
    filter(row_number() == max(row_number())) %>%
    select(game_id, team_id=home_team_id, score=home_score) %>%
    distinct() %>%
    rbind(pbp %>%
            group_by(game_id) %>%
            filter(row_number() == max(row_number())) %>%
            select(game_id, team_id=away_team_id, score=away_score) %>%
            distinct()) %>%
    arrange(game_id) %>%
    mutate(across(team_id, as.character)) 
  
  return(combo %>%
           left_join(GetOpponent(box), by=c("game_id", "team_id")) %>%
           left_join(combo, by=c("game_id", "opponent_id" = "team_id"),
                     suffix=c("_o", "_d")) %>%
           select(-opponent_id) %>%
           ungroup())
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
           arrange(-wins) %>%
           mutate(winPct = wins/(wins+losses)))
}

AddLineupsToPBP = function(pbp=load_nba_pbp(), playerbox=load_nba_player_box()) {
  # inputs: -pbp: pbp df (output of load_nba_pbp())
  #         -playerbox: player box score df (output of load_nba_player_box())
  # output: df with game_id, sequence_number, home_lineup, away_lineup
  # usage: newdf = AddLineupsToPBP(game_id, pbp, playerbox)
  #
  # recommendation: pre-load the pbp and playerbox and feed them into the function
  #                 if pulling multiple games. Only use defaults if you're pulling
  #                 a one-off game.
  
  starterdf = GetStarters(playerbox)
  
  lineupdf = pbp %>%
    mutate(across(sequence_number, as.integer)) %>%
    arrange(sequence_number) %>%
    filter(type_text == "Substitution") %>%
    select(game_id, team_id, sequence_number, text,
           player_in = participants_0_athlete_id,
           player_out = participants_1_athlete_id) %>%
    group_by(game_id) %>%
    mutate(event_number = row_number()) %>%
    ungroup() %>%
    group_by(game_id, team_id) %>%
    transmute(sequence_number, text, player_in,
              player_out, event_number,
              sub_event = row_number(),
              lineup = "") %>%
    mutate(across(player_in:player_out, as.character))
  
  lineupdf = lineupdf %>%
    left_join(starterdf %>% mutate(across(team_id, as.integer)), by=c("game_id", "team_id"), suffix=c("", "_old")) %>%
    mutate(across(sub_event, as.integer)) %>%
    arrange(game_id, team_id, sub_event) %>%
    rowwise() %>%
    mutate(lineup_old = ifelse(sub_event == 1, lineup_old, lag(lineup)),
           lineup = str_replace(lineup_old, player_out, player_in)) %>%
    ungroup()
  
  for (i in 2:max(lineupdf$sub_event)) {
    lineupdf[lineupdf[,"sub_event"] == i, 'lineup'] = lineupdf %>%
      filter(sub_event %in% c(i-1, i)) %>%
      mutate(lineup_old = lag(lineup),
             lineup = str_replace(lineup_old, player_out, player_in)) %>%
      ungroup() %>%
      filter(sub_event == i) %>% pull(lineup)
  }
  
  switches = lineupdf %>%
    rowwise() %>%
    mutate(lineup = paste0(sort(sapply(strsplit(lineup, ', '), as.integer)), collapse=', ')) %>%
    ungroup() %>%
    select(game_id, team_id, sequence_number, lineup) %>%
    mutate(across(team_id, as.character))
  
  return(pbp %>%
           select(game_id, team_id, sequence_number,
                  team_id_away = away_team_id,
                  team_id_home = home_team_id,
                  type_text) %>%
           mutate(across(team_id_away, as.character),
                  across(team_id_home, as.character),
                  across(sequence_number, as.integer)) %>%
           left_join(switches, by=c("game_id", "sequence_number", "team_id_away" = "team_id")) %>%
           left_join(switches, by=c("game_id", "sequence_number", "team_id_home" = "team_id"), suffix=c("", "_home")) %>%
           rename(lineup_away = lineup) %>%
           left_join(starterdf, by=c("game_id", "team_id_away" = "team_id")) %>%
           left_join(starterdf, by=c("game_id", "team_id_home" = "team_id"), suffix=c("", "_home_start")) %>%
           rename(lineup_away_start = lineup) %>%
           group_by(game_id) %>%
           mutate(lineup_away = ifelse(row_number() == 1, lineup_away_start, lineup_away),
                  lineup_home = ifelse(row_number() == 1, lineup_home_start, lineup_home)) %>%
           fill(lineup_away, lineup_home) %>%
           ungroup() %>%
           select(game_id, sequence_number, lineup_away, lineup_home) %>%
           mutate(across(sequence_number, as.character)))
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

GetOpponent = function(box) {
  return(box %>%
           select(game_id, team_id, opponent_id) %>%
           mutate(across(opponent_id, as.character)))
}

OffensiveTeamByPossession = function(pbp) {
  return(pbp %>%
           FixShotLocations(.) %>%
           GetPossessions(.) %>%
           select(game_id, shooting_play, sequence_number, team_id, new_poss) %>%
           group_by(game_id) %>%
           mutate(aggposs = cumsum(new_poss)) %>%
           ungroup() %>%
           group_by(game_id, aggposs, team_id) %>%
           summarize(nShots = sum(shooting_play), .groups='keep') %>%
           top_n(1) %>%
           ungroup() %>%
           select(-nShots))
}

GetTeamPossessions = function(pbp) {
  df = pbp %>% OffensiveTeamByPossession(.)
  
  switchTeam = df %>%
    group_by(game_id, team_id) %>%
    summarize(oPoss = n(),
              .groups='keep') %>%
    ungroup() %>%
    left_join(df %>%
                left_join((pbp %>%
                             select(game_id, home=home_team_id, away=away_team_id) %>%
                             distinct(game_id, home, away)),
                          by="game_id") %>%
                mutate(opp_id = ifelse(team_id == home, away, home)) %>%
                select(-c("home", "away")), by=c("game_id", "team_id")) %>%
    distinct(game_id, team_id, oPoss, opp_id) %>%
    mutate(across(opp_id, as.character),
           across(team_id, as.character))
  
  return(switchTeam %>%
           left_join(switchTeam, by=c("game_id", "team_id" = "opp_id")) %>%
           select(game_id, team_id, poss_team = oPoss.x, poss_opp = oPoss.y) %>%
           mutate(across(team_id, as.character)))
}

SetPBPAdvanced = function(pbp) {
  return(pbp %>%
           FixShotLocations(.) %>%
           GetPossessions(.) %>%
           select(shooting_play, game_id, sequence_number, text, type_text = type_text.x,
                  team_id, event, shot_amount, new_poss, pitp, p1 = participants_0_athlete_id,
                  home_score, away_score, home_team_id, away_team_id) %>%
           group_by(game_id) %>%
           mutate(aggposs = cumsum(new_poss)) %>%
           ungroup() %>%
           mutate(morey = ifelse((event == "Jump" & shot_amount == 3) |
                                   event == "Free Throw" |
                                   event == "Layup" |
                                   event == "Dunk", 
                                 1, 0),
                  ft = ifelse(event == "Free Throw", 1, 0),
                  turnover = ifelse(event == "Turnover", 1, 0),
                  threePoint = ifelse(shot_amount == 3, 1, 0),
                  shot = ifelse(shooting_play == TRUE & event != "Free Throw", 1, 0),
                  oReb = ifelse(type_text == "Offensive Rebound" & !is.na(p1), 1, 0),
                  dReb = ifelse(type_text == "Defensive Rebound" & !is.na(p1), 1, 0),
                  assist = ifelse(str_detect(text, 'assist'), 1, 0),
                  steal = ifelse(str_detect(text, 'steal'), 1, 0),
                  block = ifelse(str_detect(text, 'block'), 1, 0),
                  teamScore = ifelse(team_id == home_team_id, home_score, away_score),
                  oppScore = ifelse(team_id == home_team_id, away_score, home_score)) %>%
           group_by(game_id, aggposs, team_id) %>%
           summarize(morey = sum(morey),
                     ft = sum(ft),
                     turnover = sum(turnover),
                     threePoint = sum(threePoint, na.rm=TRUE),
                     shots = sum(shot, na.rm=TRUE),
                     oReb = sum(oReb),
                     dReb = sum(dReb),
                     steals = sum(steal),
                     assists = sum(assist),
                     blocks = sum(block),
                     paintShots = sum(pitp, na.rm=TRUE),
                     oPts = max(teamScore),
                     dPts = max(oppScore),
                     .groups='keep') %>%
           ungroup() %>%
           mutate(across(team_id, as.character),
                  morey = ifelse(morey > 0, 1, morey)))
}

AggregateAdvanced = function(pbp, box=load_nba_team_box()) {
  leftdf = pbp %>%
    SetPBPAdvanced(.) %>%
    group_by(game_id, team_id) %>%
    summarize(morey_team = sum(morey),
              ft_team = sum(ft),
              to_lost_team = sum(turnover),
              threePoint_team = sum(threePoint),
              totalShots_team = sum(shots),
              oReb_team = sum(oReb),
              dReb_team = sum(dReb),
              steal_lost_team = sum(steals),
              block_against_team = sum(blocks),
              paintShots_team = sum(paintShots),
              oPts_team = max(oPts),
              dPts_team = max(dPts),
              .groups='keep') %>%
    ungroup() %>%
    mutate(across(team_id, as.character)) %>%
    left_join(GetOpponent(box), by=c("game_id", "team_id" = "opponent_id")) %>%
    rename(., opponent_id = team_id.y)
  
  return(leftdf %>%         
           left_join(leftdf %>% mutate(across(opponent_id, as.character)),
                     by=c("game_id", "team_id"="opponent_id"),
                     suffix = c("", ".y")) %>%
           select(-team_id.y) %>%
           mutate(across(team_id, as.character)) %>%
           rename_with(~gsub("_team.y", "_opp", .x)) %>%
           rename_with(~gsub('_lost_opp', '_opp', .x)) %>%
           rename_with(~gsub('_against_opp', '_opp', .x)) %>%
           left_join(pbp %>% GetTeamPossessions(.), by=c("game_id", "team_id")) %>%
           select(-opponent_id))
}

SeasonAdvancedStats = function(pbp, box) {
  return(pbp %>%
           AggregateAdvanced(., box) %>%
           left_join(GetGameScores(pbp, box), by=c("game_id", "team_id")) %>%
           group_by(team_id) %>%
           summarize(across(morey_o:score_d, sum)) %>%
           ungroup() %>%
           mutate(across(contains("_team") & !contains("poss"), ~ .x/poss_team),
                  across(contains("_opp") & !contains("poss"), ~ .x/poss_opp)) %>%
           select(-contains("poss")) %>%
           rename_with(~gsub("_team", "Rate_team", .x)) %>%
           rename_with(~gsub("_opp", "Rate_opp", .x)) %>%
           idToTeamName(.) %>%
           arrange(team))
}
# Download NBA play-by-plays (2002-2021)
tictoc::tic()
progressr::with_progress({
  nba_pbp <- hoopR::load_nba_pbp(2002:2021)
})
tictoc::toc()

glue::glue("{nrow(nba_pbp)} rows of nba play-by-play data from {length(unique(nba_pbp$game_id))} games.")

dplyr::glimpse(nba_pbp)

# Download NBA Team Box Scores (2002-2021)
tictoc::tic()
progressr::with_progress({
  nba_team_box <- hoopR::load_nba_team_box(2002:2021)
})
tictoc::toc()

glue::glue("{nrow(nba_team_box)} rows of NBA team boxscore data from {length(unique(nba_team_box$game_id))} games.")

dplyr::glimpse(nba_team_box)

# Download NBA Player Box Scores (2002-2021)
tictoc::tic()
progressr::with_progress({
  nba_player_box <- hoopR::load_nba_player_box(2016:2021)
})
tictoc::toc()

glue::glue("{nrow(nba_player_box)} rows of NBA player boxscore data from {length(unique(nba_player_box$game_id))} games.")

dplyr::glimpse(nba_player_box)

# Loading Packages
packs = c('dplyr','ggplot2', 'caret','corrplot', 'e1071','readr','reshape2')
lapply(packs,require,character.only=TRUE)

# Look at the data structure and estimate which are quantitative features.
str(nba_pbp)
sapply(nba_pbp,function(x){ length(unique(x)) })

str(nba_team_box)
sapply(nba_team_box,function(x){ length(unique(x)) })

#Plot the missing data
ggplot_missing <- function(x){
  if(!require(reshape2)){warning('you need to install reshape2')}
  require(reshape2)
  require(ggplot2)
  #### This function produces a plot of the missing data pattern
  #### in x.  It is a modified version of a function in the 'neato' package
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
ggplot_missing(nba_team_box)

#Show the structure of data and how many unique levels in each variables.
rbind(sapply(nba_team_box,function(x){ length(unique(x))}),
        sapply(nba_team_box,class))
str(nba_team_box)

#Look at the data of only Celtics vs Rocket
team_box_BostonVsHouston = nba_team_box %>% 
  filter(., team_abbreviation == "BOS" & opponent_abbrev == "HOU")
ggplot_missing(team_box_BostonVsHouston)

rbind(sapply(team_box_BostonVsHouston,function(x){ length(unique(x))}),
      sapply(team_box_BostonVsHouston,class))

#Separate the goals make and goals attempted

tempFieldGoals = 
        sapply(team_box_BostonVsHouston$field_goals_made_field_goals_attempted, 
        strsplit, split = "-") %>% 
        unlist() %>% 
        matrix(., ncol = 2, byrow = TRUE)%>%
        data.frame()

names(tempFieldGoals) = c("field_goals", "field_goals_attempted")

temp3Points = 
        sapply(team_box_BostonVsHouston$three_point_field_goals_made_three_point_field_goals_attempted, 
        strsplit, split = "-") %>% 
        unlist() %>% 
        matrix(., ncol = 2, byrow = TRUE)%>%
        data.frame()

names(temp3Points) = c("three_points", "three_points_attempted")

tempFreeThrows = 
        sapply(team_box_BostonVsHouston$free_throws_made_free_throws_attempted, 
        strsplit, split = "-") %>% 
        unlist() %>% 
        matrix(., ncol = 2, byrow = TRUE)%>%
        data.frame()   

names(tempFreeThrows) = c("free_throws", "free_throws_attempted")

## Select the remaining quantitative features
BostonVsHouston_quanData = team_box_BostonVsHouston %>%
  select(field_goal_pct,
         three_point_field_goal_pct,
         free_throw_pct,
         total_rebounds,
         offensive_rebounds,
         defensive_rebounds,
         assists,
         steals,
         blocks,
         turnovers,
         team_turnovers,
         total_turnovers,
         technical_fouls,
         total_technical_fouls,
         flagrant_fouls,
         turnover_points,
         fast_break_points,
         points_in_paint,
         fouls)

BostonVsHouston_quanData = bind_cols(BostonVsHouston_quanData,
                                      tempFieldGoals,
                                      temp3Points,
                                      tempFreeThrows)

## Covert all characters to numeric
BostonVsHouston_quanData = as.data.frame(sapply(BostonVsHouston_quanData,
                                                 as.numeric))

## Plot the correlation plot of all numeric features
## There is some issue with the "point_in_paint" feature (lots of "-1")
## And there is lots of missing values in "largest_lead" featur
## Need to evaluate
corrplot(cor(BostonVsHouston_quanData), tl.cex = 0.8)

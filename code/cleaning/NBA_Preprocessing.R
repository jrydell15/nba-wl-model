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

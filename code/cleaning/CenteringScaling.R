require(tidyverse)
require(glue)
require(here)

GetCenteredScaledByDay = function(years) {
  for (year in years) {
    dfYear = read_csv(here(glue('data/aggregated/agg_{year}.csv')), show_col_types = FALSE)
    dfYear = dfYear %>%
      mutate(season = year,
             daysSinceOpener = day(as.period(date - min(date, na.rm=TRUE)))) %>%
      select(game_id, team_id, gameNum, date, season, daysSinceOpener,
             contains("Rat"))
  
    cols = names(dfYear)[str_detect(names(dfYear), 'Rat')]
    centscale = dfYear
    
    for (col in cols) {
      centscale = centscale %>%
        group_by(team_id) %>%
        mutate("{col}_lag" := lag(!! sym(col))) %>%
        ungroup()
    }
    
    centscale = centscale %>% na.omit()
    
    dates = unique(date(centscale$date))
    datetfull = tibble()
    for (date_fil in dates) {
      datet = centscale %>%
        filter(date <= as_date(date_fil)) %>%
        mutate(date_comp = as_date(date_fil)) %>%
        group_by(team_id) %>%
        slice_tail(n=1) %>%
        ungroup()
      
      datetfull = rbind(datetfull, datet)
    }
    
    centscale = centscale %>%
                 mutate(date = date(date)) %>%
                 select(game_id:daysSinceOpener, contains('_lag')) %>%
                 rename_with(~gsub('_lag', '', .x))
    
    datetfull = datetfull %>%
      mutate(date_comp = date(date_comp)) %>%
      pivot_wider(id_cols = c(season, date_comp),
                  names_from=team_id, names_prefix='team_id.',
                  values_from=cum_moreyRate_team_lag:cum_dRating_team_lag) %>%
      na.omit() %>%
      pivot_longer(cols=contains('team_id')) %>%
      separate(name, into=c('stat', 'team_id'), sep='_lag_team_id.') %>%
      group_by(season, date_comp, stat) %>%
      mutate(cs = as.numeric(scale(value, scale=T))) %>%
      ungroup() %>%
      pivot_wider(id_cols=c(season, date_comp, team_id),
                  names_from=stat,
                  values_from=cs) %>%
      rename(date = date_comp)
    
    write_csv(centscale, here(glue('data/lagged/lag_{year}.csv')))
    write_csv(datetfull, here(glue('data/centeredScaled/CS_{year}.csv')))
  }
}

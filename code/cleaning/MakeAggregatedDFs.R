require(here)

source(here('code/cleaning/CleanRaw.R'))

startSeason = 2002
endSeason = hoopR::most_recent_nba_season()

for (year in startSeason:endSeason) {
  if (file.exists(here(paste0('data/aggregated/agg_', year, '.csv')))) {
    next
  }
  else {
    print(year)
    df = GetCummulativeDF(year)
    
    write_csv(df, file=here(paste0('data/aggregated/agg_', year, '.csv')))
    rm(df)
  }
}

require(tidyverse)
require(here)

source(here('code/cleaning/CleaningUtilities.R'))

GetCummulativeDF = function(year=hoopR::most_recent_nba_season()) {
  pbp = read_csv(here(paste0('data/raw/PBP/PBP_', year, '.csv')))
  box = read_csv(here(paste0('data/raw/teamBox/teamBox_', year, '.csv')))
  playerBox = read_csv(here(paste0('data/raw/playerBox/playerBox_', year, '.csv')))
  pbp_noGarbageTime = FilterGarbageTime(pbp, playerBox)
  adv = AggregateAdvanced(pbp_noGarbageTime, box)
  
  rm(list=c("pbp", "box", "pbp_noGarbageTime", "playerBox"))
  
  teams = adv %>% select(team_id) %>% distinct()
  team = teams %>% mutate(across(team_id, as.numeric)) %>% filter(team_id <= 30)
  
  cumadv = adv %>% 
    filter(as.numeric(team_id) %in% team$team_id) %>%
    group_by(team_id) %>%
    # o is "team 1", d is "team 2"
    mutate(cum_morey_o = cumsum(morey_o),
           cum_poss_o = cumsum(poss_o),
           cum_moreyRate_o = cum_morey_o / cum_poss_o,           #moreyRate: the prop of possessions which produced a FT, 3PT, or layup/dunk
           cum_steal_o = cumsum(steal_lost_o),
           cum_stealRate_o = cum_steal_o / cum_poss_o,           #stealRate_o: prop of possessions which ended by turning over the ball due to it being stolen (high is bad)
           cum_ft_o = cumsum(ft_o),
           cum_totalShots_o = cumsum(totalShots_o),
           cum_ftRate_o = cum_ft_o / cum_totalShots_o,           #FTA / FGA
           cum_oReb_o = cumsum(oReb_o),
           cum_dReb_d = cumsum(dReb_d),
           cum_oRebRate_o = cum_oReb_o / (cum_oReb_o + cum_dReb_d),   #oRebRate_o: oRebRate gotten by the offense
           cum_threePoint_o = cumsum(threePoint_o),
           cum_threePointRate_o = cum_threePoint_o / cum_totalShots_o, #3FGA / FGA
           cum_dReb_o = cumsum(dReb_o),
           cum_oReb_d = cumsum(oReb_d),
           cum_dRebRate_o = cum_dReb_o / (cum_dReb_o + cum_oReb_d),    #dRebRate_o: dRebRate for the "offensive team"
           cum_block_against_o = cumsum(block_against_o),
           cum_blockRate_d = cum_block_against_o / cum_totalShots_o,   #blockRate_d: prop of total shots were blocked by the defense (high is good)
           cum_morey_d = cumsum(morey_d),
           cum_poss_d = cumsum(poss_d),
           cum_moreyRate_d = cum_morey_d / cum_poss_d,
           cum_steal_d = cumsum(steal_d),
           cum_stealRate_d = cum_steal_d / cum_poss_d,
           cum_ft_d = cumsum(ft_d),
           cum_totalShots_d = cumsum(totalShots_d),
           cum_ftRate_d = cum_ft_d / cum_totalShots_d,
           cum_oReb_d = cumsum(oReb_d),
           cum_dReb_o = cumsum(dReb_o),
           cum_oRebRate_d = cum_oReb_d / (cum_oReb_d + cum_dReb_o),
           cum_threePoint_d = cumsum(threePoint_d),
           cum_threePointRate_d = cum_threePoint_d / cum_totalShots_d,
           cum_dReb_d = cumsum(dReb_d),
           cum_oReb_o = cumsum(oReb_o),
           cum_dRebRate_d = cum_dReb_d / (cum_dReb_d + cum_oReb_o),
           cum_block_d = cumsum(block_d),
           cum_blockRate_o = cum_block_d / cum_totalShots_d,
           cum_paintRate_o = cumsum(paintShots_o) / cum_totalShots_o,
           cum_paintRate_d = cumsum(paintShots_d) / cum_totalShots_d,
           gameNum = row_number()) %>%
    ungroup()
  
  rm(list=c("adv"))
  
  return(cumadv)
}
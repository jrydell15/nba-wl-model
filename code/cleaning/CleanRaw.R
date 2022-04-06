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
    mutate(cum_morey_team = cumsum(morey_team),
           cum_poss_team = cumsum(poss_o),
           cum_moreyRate_team = cum_morey_team / cum_poss_team,           #moreyRate: the prop of possessions which produced a FT, 3PT, or layup/dunk
           cum_steal_team = cumsum(steal_lost_team),
           cum_stealRate_team = cum_steal_team / cum_poss_team,           #stealRate_team: prop of possessions which ended by turning over the ball due to it being stolen (high is bad)
           cum_ft_team = cumsum(ft_team),
           cum_totalShots_team = cumsum(totalShots_team),
           cum_ftRate_team = cum_ft_team / cum_totalShots_team,           #FTA / FGA
           cum_oReb_team = cumsum(oReb_team),
           cum_dReb_opp = cumsum(dReb_opp),
           cum_oRebRate_team = cum_oReb_team / (cum_oReb_team + cum_dReb_opp),   #oRebRate_team: oRebRate gotten by the offense
           cum_threePoint_team = cumsum(threePoint_team),
           cum_threePointRate_team = cum_threePoint_team / cum_totalShots_team, #3FGA / FGA
           cum_dReb_team = cumsum(dReb_team),
           cum_oReb_opp = cumsum(oReb_opp),
           cum_dRebRate_team = cum_dReb_team / (cum_dReb_team + cum_oReb_opp),    #dRebRate_team: dRebRate for the "offensive team"
           cum_block_against_team = cumsum(block_against_team),
           cum_blockRate_opp = cum_block_against_team / cum_totalShots_team,   #blockRate_opp: prop of total shots were blocked by the defense (high is good)
           cum_morey_opp = cumsum(morey_opp),
           cum_poss_opp = cumsum(poss_d),
           cum_moreyRate_opp = cum_morey_opp / cum_poss_opp,
           cum_steal_opp = cumsum(steal_opp),
           cum_stealRate_opp = cum_steal_opp / cum_poss_opp,
           cum_ft_opp = cumsum(ft_opp),
           cum_totalShots_opp = cumsum(totalShots_opp),
           cum_ftRate_opp = cum_ft_opp / cum_totalShots_opp,
           cum_oReb_opp = cumsum(oReb_opp),
           cum_dReb_team = cumsum(dReb_team),
           cum_oRebRate_opp = cum_oReb_opp / (cum_oReb_opp + cum_dReb_team),
           cum_threePoint_opp = cumsum(threePoint_opp),
           cum_threePointRate_opp = cum_threePoint_opp / cum_totalShots_opp,
           cum_dReb_opp = cumsum(dReb_opp),
           cum_oReb_team = cumsum(oReb_team),
           cum_dRebRate_opp = cum_dReb_opp / (cum_dReb_opp + cum_oReb_team),
           cum_block_opp = cumsum(block_opp),
           cum_blockRate_team = cum_block_opp / cum_totalShots_opp,
           cum_paintRate_team = cumsum(paintShots_team) / cum_totalShots_team,
           cum_paintRate_opp = cumsum(paintShots_opp) / cum_totalShots_opp,
           gameNum = row_number()) %>%
    ungroup()
  
  rm(list=c("adv"))
  
  return(cumadv)
}
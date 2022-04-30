library(shiny)
library(hoopR)
library(tidyverse)
library(DT)
lastDay = read_csv('./data/input.csv')
model = readRDS('./finalModel.rds')

lastDate = max(lastDay$date)
lastDay = lastDay %>% select(-date)

year = hoopR::most_recent_nba_season()

getGTTable = function(team) {
  return(lastDay %>%
    mutate(across(contains('Rating'), ~round(.*100, 2)),
           across(c(cum_oRating_team, contains("_team")), ~rank(-.), .names="{col}_rank"),
           across(c(cum_dRating_team, contains("_opp")), ~rank(.), .names="{col}_rank")) %>%
    filter(display_name == team) %>%
    select(!c(contains("_rank"), display_name)) %>%
    pivot_longer(cols=everything(), names_to = 'Stat', values_to = 'Raw') %>%
    filter(!str_detect(Stat, 'RebRate_opp')) %>%
    mutate(type = ifelse(str_detect(Stat, '_team'), 'off', 'def'),
           type = ifelse(Stat == 'cum_dRating_team', 'def', type),
           type = ifelse(Stat == 'cum_dRebRate_team', 'def', type),
           fixedName = case_when(str_detect(Stat, "Rating_") ~ 'Points per 100 Possessions',
                                 str_detect(Stat, 'morey') ~ 'Morey Rate',
                                 str_detect(Stat, 'steal') ~ 'Steal Rate',
                                 str_detect(Stat, 'ftRate') ~ 'Free Throw Rate',
                                 str_detect(Stat, 'threePoint') ~ 'Three Point Rate',
                                 str_detect(Stat, 'RebRate') ~ 'Rebound Rate',
                                 str_detect(Stat, 'paintRate') ~ 'Paint Rate', 
                                 str_detect(Stat, 'block') ~ 'Block Rate',
                                 TRUE ~ 'other')) %>%
    filter(fixedName != 'other') %>%
    pivot_wider(id_cols = fixedName, names_from = type, values_from = Raw) %>%
    left_join(lastDay %>%
                mutate(across(c(cum_oRating_team, contains("_team")), ~rank(-.), .names="{col}_rank"),
                       across(c(cum_dRating_team, contains("_opp")), ~rank(.), .names="{col}_rank")) %>%
                filter(display_name == team) %>%
                select(contains("_rank")) %>%
                pivot_longer(cols=contains("_rank"), names_to='Stat', values_to='Rank') %>%
                filter(!str_detect(Stat, 'RebRate_opp')) %>%
                mutate(type = ifelse(str_detect(Stat, '_team'), 'offRank', 'defRank'),
                       type = ifelse(Stat == 'cum_dRating_team_rank', 'defRank', type),
                       type = ifelse(Stat == 'cum_dRebRate_team_rank', 'defRank', type),
                       fixedName = case_when(str_detect(Stat, "Rating_") ~ 'Points per 100 Possessions',
                                             str_detect(Stat, 'morey') ~ 'Morey Rate',
                                             str_detect(Stat, 'steal') ~ 'Steal Rate',
                                             str_detect(Stat, 'ftRate') ~ 'Free Throw Rate',
                                             str_detect(Stat, 'threePoint') ~ 'Three Point Rate',
                                             str_detect(Stat, 'RebRate') ~ 'Rebound Rate',
                                             str_detect(Stat, 'paintRate') ~ 'Paint Rate', 
                                             str_detect(Stat, 'block') ~ 'Block Rate',
                                             TRUE ~ 'other')) %>%
                filter(fixedName != 'other') %>%
                pivot_wider(id_cols = fixedName, names_from = type, values_from = Rank),
              by='fixedName')  %>%
      gt(rowname_col = 'fixedName') %>%
      tab_stubhead(label = 'Stat') %>%
      fmt_percent(columns = c(off, def),
                  rows = ends_with("Rate"),
                  decimals = 1) %>%
      tab_spanner(label = 'Offense',
                  columns = c(off, offRank)) %>%
      tab_spanner(label = 'Defense',
                  columns = c(def, defRank)) %>%
      data_color(columns = c(offRank, defRank),
                 colors = scales::col_numeric(domain = 1:30,
                                              palette = 'Blues',
                                              reverse = TRUE)) %>%
      cols_align(align = 'center',
                 columns = c(off:defRank)) %>%
      cols_label(
        off = "",
        def = "",
        offRank = 'Rank',
        defRank = 'Rank'
      ) %>%
      tab_style(
        style = list(
          cell_text(align='left')
        ),
        locations = cells_stub()
      ) %>%
      tab_header(
        title = paste0(team, ' Stats for ', year),
        subtitle = paste0('As of ', lastDate, ' (Garbage Time Removed)')
      ) %>%
      opt_align_table_header(align = 'left') %>%
      tab_source_note(source_note = 'Data: hoopR'))
}

buildInputFrame = function(team1, team2, homeRest, awayRest, homeBTB, awayBTB) {
  return(cbind(lastDay %>%
          filter(display_name == team1) %>%
          select(-display_name),
        lastDay %>%
          filter(display_name == team2) %>%
          select(-display_name) %>%
          rename_all(~paste0(., "_opp"))) %>% 
    mutate(restDays = ifelse(homeBTB == 1, 0, homeRest),
           restDays_opp = ifelse(awayBTB == 1, 0, awayRest),
           loc.H = 1,
           btb.1 = homeBTB,
           btb_opp.1 = awayBTB) %>%
    select(model$finalModel$xNames))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NBA Modeling"),
    
    hr(),
    
    # dropdown for choosing team
    fluidRow(
      column(6,
           column(6, h3("Home Team")),
           wellPanel(
            fluidRow(
              column(12,
                     selectInput(inputId = 'homeTeam',
                                 label = '',
                                 choices = lastDay %>% pull(display_name) %>% unique(),
                                 selected = "Houston Rockets"))
                    ),
            fluidRow(
              column(6,
                     checkboxInput(inputId = 'homeBTB',
                                   label = 'Back to Back',
                                   value = FALSE)),
              column(6,
                     sliderInput(inputId = 'homeDayOff',
                                 label = 'Days Off',
                                 min = 0,
                                 max = 7,
                                 value = 2,
                                 ticks = FALSE))
                    )
            )
           ),
      column(6,
             column(6, h3("Away Team")),
             wellPanel(
               fluidRow(
                 column(12,
                        selectInput(inputId = 'awayTeam',
                                    label = '',
                                    choices = lastDay %>% pull(display_name) %>% unique(),
                                    selected = 'Dallas Mavericks'))
               ),
               fluidRow(
                 column(6,
                        checkboxInput(inputId = 'awayBTB',
                                      label = 'Back to Back',
                                      value = FALSE)),
                 column(6,
                        sliderInput(inputId = 'awayDayOff',
                                    label = 'Days Off',
                                    min = 0,
                                    max = 7,
                                    value = 2,
                                    ticks = FALSE))
               )
             )
           )
    ),
    
    fluidRow(column(6, gt_output("homeRanks")),
             column(6, gt_output("awayRanks"))),
    
    hr(),
    h3("Model Prediction"),
    
    fluidRow(plotOutput('outPlot'))
      
    
)


server <- function(input, output) {
  
    output$homeRanks = render_gt(getGTTable(input$homeTeam))
    output$awayRanks = render_gt(getGTTable(input$awayTeam))
    
    output$outPlot = renderPlot(
            tibble('win' = predict(model,
                                   buildInputFrame(input$homeTeam,
                                                   input$awayTeam,
                                                   input$homeDayOff,
                                                   input$awayDayOff,
                                                   input$homeBTB,
                                                   input$awayBTB),
                                   type='prob')$'win') %>%
            ggplot(aes(ymin=1, ymax=2)) +
              geom_rect(aes(xmin=0, xmax=win), fill='#669bf8') +
              geom_rect(aes(xmin=win, xmax=1), fill='#94969b') +
              theme_void() +
              lims(y=c(0, 2)) +
              geom_text(aes(x=win/2, y=1.5,
                            label=scales::percent(win, accuracy=0.01)),
                        size=10) +
              theme(panel.background=element_blank())
            )
}

# Run the application 
shinyApp(ui = ui, server = server)

library(tidyverse)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(na.tools)
library(ggimage)
library(nflfastR)
library(gt)
library(mgcv)
library(scales)
library(ggforce)
library(remotes)
library(ggtext)
library(bayesboot)
library(rvest)
library(shiny)
library(shinythemes)
library(Cairo)
library(ggbeeswarm)
library(cfbfastR)
library(ggthemes)

gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "middle",
      ...
    )}

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 13, hjust = 0.5),
      axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16),
      axis.text = element_text(size = 12)
    )
}

changed_pos_data <- read_csv("https://raw.githubusercontent.com/tejseth/recruiting/master/change_pos_stats.csv")
rec_with_waa <- read_csv("https://raw.githubusercontent.com/tejseth/recruiting/master/recruiting_with_waa.csv")
season_waa_stats <- read_csv("https://raw.githubusercontent.com/tejseth/recruiting/master/season_waa_stats.csv")
position_waa_stats <- read_csv("https://raw.githubusercontent.com/tejseth/recruiting/master/position_waa_stats.csv")

position_waa_stats <- position_waa_stats %>%
  select(-X1)

season_waa_stats <- season_waa_stats %>%
  select(-X1)

rec_with_waa <- rec_with_waa %>%
  select(-X, -X1) %>%
  filter(!data.position %in% c("K", "P", "FB", "LS")) %>%
  filter(!position %in% c("K", "P", "FB", "LS"))

changed_pos_data <- changed_pos_data %>%
  select(-X1)

pos_options <- unique(rec_with_waa$data.position)
con_options <- unique(season_waa_stats$conference)
team_options <- unique(season_waa_stats$school)
p_5 <- c("SEC", "Pac-12", "Big 12", "ACC", "Big Ten")

options(shiny.usecairo=T)

ui <- fluidPage(
  
  theme = shinytheme("cosmo"),
  
  titlePanel("PFF Recruiting Guide"),
  
  mainPanel(
    navbarPage("PFF",
               tabPanel("Position Breakdown",
                        fluidRow(
                          column(4, align = "center",
                                 
                                 tags$h3("Parameters"),
                                 selectInput(
                                   inputId = "pos",
                                   label = "Position:",
                                   choices = pos_options,
                                   selected = "QB"
                                 ),
                                 selectInput(
                                   inputId = "change",
                                   label = "Changed Position",
                                   choices = c("Yes", "No", "Both"),
                                   selected = "No"
                                 ),
                                 sliderInput("min_seasons", "Seasons Played Range", 
                                             value = c(2, 5), min = 1, max = 5, sep = ""),
                                 
                          )
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "pos_graph",
                                     width = "100%",
                                     height = "50%"),
                          br(),
                          plotOutput(outputId = "change_graph",
                                     width = "100%",
                                     height = "50%"),
                          br()
                        )        
                        
               ),
               tabPanel("Team Breakdown",
                        fluidRow(
                          column(4, align = "center",
                                 selectInput("con_1",
                                             "Conference 1", 
                                             c(sort(unique(as.character(con_options)))), selected = "SEC"),
                                 selectInput("con_2",
                                             "Conference 2", 
                                             c(sort(unique(as.character(con_options)))), selected = "Pac-12"),
                                 selectInput("con_3",
                                             "Conference 3", 
                                             c(sort(unique(as.character(con_options)))), selected = "Big 12")),
                          column(8, align = "center",
                                 selectInput("con_4",
                                             "Conference 4", 
                                             c(sort(unique(as.character(con_options)))), selected = "ACC"),
                                 selectInput("con_5",
                                             "Conference 5", 
                                             c(sort(unique(as.character(con_options)))), selected = "Big Ten"),
                                 sliderInput("seasons_range", "Recruiting Class Range", 
                                             value = c(2014, 2020), min = 2014, max = 2020, sep = ""),
                          )
                      ),
                        mainPanel(
                          plotOutput(outputId = "team_graph",
                                     width = "100%",
                                     height = "50%"),
                          br(),
                        ),
                      fluidRow(
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        column(4, align = "center",
                               selectInput("team",
                                           "Select Team", 
                                           c(sort(unique(as.character(team_options)))), selected = "Alabama"),  
                        )
                      ),
                      mainPanel(
                        tableOutput("team_table"),
                        br(),
                      ),
               ),
               tabPanel("Each Team's Position Breakdown",
                        fluidRow(
                          column(4, align = "center",
                                 
                                 tags$h3("Parameters"),
                                 selectInput(
                                   inputId = "pos_2",
                                   label = "Position:",
                                   choices = pos_options,
                                   selected = "WR"
                                 ),
                                 sliderInput("seasons_range_2", "Recruiting Class Range", 
                                             value = c(2014, 2020), min = 2014, max = 2020, sep = ""),
                                 
                          )
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "p_5",
                                     width = "100%",
                                     height = "50%"),
                          br(),
                          plotOutput(outputId = "g_5",
                                     width = "100%",
                                     height = "50%"),
                          br()
                        )        
                        
               )
    )
  )
)

server <- function(input, output) {
  
  output$pos_graph <-  renderPlot({
    
    specific_position <- rec_with_waa %>%
      filter(data.position == input$pos)
    
    specific_position <- specific_position %>%
      filter(same_pos == case_when(
        input$change == "No" ~ 1,
        input$change == "Yes" ~ 0,
        input$change == "Both" ~ c(0, 1)
      ))
    
    specific_position <- specific_position %>%
      filter(seasons >= input$min_seasons[1] & seasons <= input$min_seasons[2])
    
    specific_position %>%
      ggplot(aes(x = data.rating, y = total_waa)) +
      geom_jitter(aes(color = as.factor(star)), alpha = 0.6, size = 5) +
      geom_smooth(color = "black", size = 2) +
      theme_reach() +
      labs(x = "247 Rating",
           y = "Total WAA",
           title = paste0("How 247 Rating Influences Total Wins Above Average for ", input$pos),
           subtitle = paste0("Seasons ", input$min_seasons[1], "-", input$min_seasons[2], ", color is for star rating"))
  },  height = 600, width = 850)
  
  output$change_graph <-  renderPlot({
    
    pos <- changed_pos_data %>%
      filter(data.position == input$pos)
    
    pos %>%
      ggplot(aes(x = position, y = count)) +
      geom_bar(aes(fill = position), stat = "identity", color = "black") +
      theme_reach() +
      scale_fill_brewer(palette = "Set3") +
      labs(x = "Position Played in College",
           y = "# of Players",
           title = paste0("Where High School ", input$pos, "'s Usually Went During a Position Change"),
           subtitle = "College positions listed in each bar") 
  },  height = 600, width = 850)
  
  output$team_graph <-  renderPlot({
    
    spec_conferences <- season_waa_stats %>%
      filter(conference %in% c(input$con_1, input$con_2, input$con_3, input$con_4, input$con_5))
    
    spec_conferences <- spec_conferences %>%
      filter(season >= input$seasons_range[1] & season <= input$seasons_range[2])
    
    group <- spec_conferences %>%
      group_by(school, logo) %>%
      summarize(exp_waa =sum(exp_waa),
                actual_waa = sum(actual_waa))
      
    group %>%
      ggplot(aes(x = exp_waa, y = actual_waa)) +
      geom_image(aes(image = logo), asp = 16/9, size = 0.04) +
      theme_reach() +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      geom_hline(yintercept = mean(group$actual_waa), linetype = "dashed") +
      geom_vline(xintercept = mean(group$exp_waa), linetype = "dashed") +
      labs(x = "Expected WAA From <span style= 'color:red'>Recruiting</span>",
           y = "Actual WAA From <span style= 'color:blue'>Playing</span>",
           title = paste0("How Schools <span style= 'color:red'>Recruit</span> and 
                          <span style= 'color:blue'>Develop</span> Talent, ", input$seasons_range[1], "-", input$seasons_range[2]),
           subtitle = paste0("Conferences: ", input$con_1, ", ", input$con_2, ", ", input$con_3, ", ", input$con_4, ", ", input$con_5)) +
      theme(plot.title = element_markdown(size = 20, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            axis.title.x = element_markdown(size=16),
            axis.title.y = element_markdown(size=16))
  },  height = 600, width = 850)
  
  output$team_table <- render_gt({
    
    team_season <- season_waa_stats %>%
      filter(school == input$team)
    
    team_by_season <- team_season %>%
      select(logo, season, croots, exp_waa, actual_waa, waa_oe)
    
    team_by_season <- team_by_season %>%
      mutate_if(is.numeric, ~round(., 2))
    
    team_by_season %>% gt() %>%
      text_transform(
        locations = cells_body(c(logo)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>% 
      cols_label(
        logo = "Team",
        season = "Recruiting Class",
        croots = "# of Recruits",
        exp_waa = "Expected WAA",
        actual_waa = "Actual WAA",
        waa_oe = "WAA Over Expected") %>%
      data_color(
        columns = c(waa_oe),
        colors = scales::col_numeric(
          palette = c("white", "#3fc1c9"),
          domain = NULL
        )
      ) %>% 
      opt_align_table_header(align = "center") %>%
      tab_header(
        title = md(paste0("How ", input$team, " Recruits and Develops Talent")),
        subtitle = md("WAA = Wins Above Average, Expected WAA is based off recruiting, Actual WAA is based off playing")) %>% 
      opt_row_striping() %>%
      gt_theme_538()
  }, width = 700)
  
  output$p_5 <-  renderPlot({
    
    p_5 <- position_waa_stats %>%
      filter(conference %in% p_5) %>%
      filter(data.position == input$pos_2) %>%
      filter(season >= input$seasons_range_2[1] & season <= input$seasons_range_2[2])
    
    p_5_2 <- p_5 %>%
      group_by(school, logo) %>%
      summarize(exp_waa =sum(exp_waa),
                actual_waa = sum(actual_waa))
    
    p_5_2 %>%
      ggplot(aes(x = exp_waa, y = actual_waa)) +
      geom_image(aes(image = logo), asp = 16/9, size = 0.04) +
      theme_reach() +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      geom_hline(yintercept = mean(p_5_2$actual_waa), linetype = "dashed") +
      geom_vline(xintercept = mean(p_5_2$exp_waa), linetype = "dashed") +
      labs(x = "Expected WAA From Recruiting",
           y = "Actual WAA From Playing",
           title = paste0("How Power 5 Recruit and Develop Talent at ", input$pos_2),
           subtitle = paste0("Years: ", input$seasons_range_2[1], "-", input$seasons_range_2[2])) 
  },  height = 600, width = 850)
  
  output$g_5 <-  renderPlot({
    
    g_5 <- position_waa_stats %>%
      filter(!conference %in% p_5) %>%
      filter(data.position == input$pos_2)
    
    g_5 <- g_5 %>%
      filter(season >= input$seasons_range_2[1] & season <= input$seasons_range_2[2])
    
    g_5_2 <- g_5 %>%
      group_by(school, logo) %>%
      summarize(exp_waa =sum(exp_waa),
                actual_waa = sum(actual_waa))
    
    g_5_2 %>%
      ggplot(aes(x = exp_waa, y = actual_waa)) +
      geom_image(aes(image = logo), asp = 16/9, size = 0.04) +
      theme_reach() +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      geom_hline(yintercept = mean(g_5_2$actual_waa), linetype = "dashed") +
      geom_vline(xintercept = mean(g_5_2$exp_waa), linetype = "dashed") +
      labs(x = "Expected WAA From Recruiting",
           y = "Actual WAA From Playing",
           title = paste0("How Non-Power 5 School Recruit and Develop Talent at ", input$pos_2),
           subtitle = paste0("Years: ", input$seasons_range_2[1], "-", input$seasons_range_2[2])) 
  },  height = 600, width = 850)

}

# Run the application 
shinyApp(ui = ui, server = server)






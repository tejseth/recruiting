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

season_waa_stats <- season_waa_stats %>%
  select(-X1)

rec_with_waa <- rec_with_waa %>%
  select(-X, -X1) %>%
  filter(!data.position %in% c("K", "P", "FB", "LS")) %>%
  filter(!position %in% c("K", "P", "FB", "LS"))

changed_pos_data <- changed_pos_data %>%
  select(-X1)

pos_options <- unique(rec_with_waa$data.position)

options(shiny.usecairo=T)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  
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
                        sliderInput("min_seasons", "Seasons Played Range", value = c(2, 5), min = 1, max = 5, sep = ""),
                        )
                      ),
               mainPanel(
                 plotOutput(
                   outputId = "pos_graph",
                   width = "100%",
                   height = "50%"
                 )
               ),
              mainPanel(
                br(),
                plotOutput(
                  outputId = "change_graph",
                  width = "100%",
                  height = "50%"
          ),
        br()
        )
      ),
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

}

# Run the application 
shinyApp(ui = ui, server = server)

ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  titlePanel("Rushing Yards Over Expected"),
  
  mainPanel(
    navbarPage("@mfbanalytics",
               tabPanel("By Season",
                        fluidRow(
                          column(4, align = "center",
                                 
                                 tags$h3("Parameters"),
                                 
                                 selectInput(
                                   inputId =  "season",
                                   label = "Season:",
                                   choices = 1999:2020,
                                   selected = 2020
                                 ),
                                 
                                 sliderInput(
                                   inputId =  "min_rushes",
                                   label = "Minimum Rushes:",
                                   min = 1, max = 300,
                                   value = 80
                                 ),
                                 
                          )
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "rusher_graph",
                                     width = "100%",
                                     height = "50%"),
                          tableOutput("rusher_table")
                        )        
                        
               ),
               tabPanel("By Team",
                        fluidRow(
                          column(4, align = "center",
                                 
                                 tags$h3("Parameters"),
                                 selectInput("team",
                                             "Offense:",
                                             c(sort(unique(as.character(ids)))), selected = "DET"),
                                 sliderInput(
                                   inputId =  "team_min_rushes",
                                   label = "Minimum Rushes:",
                                   min = 1, max = 300,
                                   value = 50
                                 ),
                                 selectInput(
                                   inputId =  "team_season",
                                   label = "Season:",
                                   choices = 1999:2020,
                                   selected = 2020
                                 ),
                          )
                        ),
                        mainPanel(
                          plotOutput(outputId = "team_graph",
                                     width = "100%",
                                     height = "50%"),
                          tableOutput(outputId = "team_table_1"),
                          tableOutput(outputId = "team_table_2")
                        ),
               ),
               tabPanel('Rusher Comparison',
                        fluidRow(
                          column(4, align = "center",
                                 tags$h3('Parameters'),
                                 selectInput("player_1",
                                             "Player 1", 
                                             c(sort(unique(as.character(rushers)))), selected = "A.Kamara"),
                                 selectInput("player_2",
                                             "Player 2", 
                                             c(sort(unique(as.character(rushers)))), selected = "N.Chubb"),
                                 selectInput("player_3",
                                             "Player 3", 
                                             c(sort(unique(as.character(rushers)))), selected = "D.Henry"),
                          ),
                          sliderInput("year_range", "Year Range", value = c(2018, 2020), min = 1999, max = 2020, sep = ""),
                          sliderInput("week_range", "Weeks Range", value = c(1, 17), min = 1, max = 17),
                        ),
                        mainPanel(
                          plotOutput(outputId = "csum_graph",
                                     width = "750px", height = "500px"),
                          tableOutput(outputId = "rusher_comp_tab"), 
                          plotOutput(outputId = "perc_stacked",
                                     width = "750px", height = "500px")
                        ),
                        column(6, plotOutput(outputId = "rusher_graph_1", width = "750px", height = "500px")),
                        column(9, plotOutput(outputId = "rusher_graph_2", width = "750px", height = "500px")),
                        column(12, plotOutput(outputId = "rusher_graph_3", width = "750px", height = "500px")),
                        #plotOutput(outputId = "rusher_graph_1"),
                        #plotOutput(outputId = "rusher_graph_2"),
                        #plotOutput(outputId = "rusher_graph_3"),
               )
    )
  )
)





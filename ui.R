# ui.R
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(stringr)
library(plyr)
library(scales)
library(emo)

source("data_process.R")
movie_df <- load_and_clean_data()

ui <- fluidPage(
    tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  titlePanel(paste(emo::ji("clapper_board"), "CineInsights Explorer: Dive into Film Finances!")),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("genreInput",
                  paste(emo::ji("film_frames"), "Select Genre:"),
                  choices = c("All", sort(unique(movie_df$genre)))
                 ),
      selectInput("decadeInput",
                  paste(emo::ji("calendar"), "Select Decade:"),
                  choices = c("All", "1980s", "1990s", "2000s", "2010s")
                 ),
      selectInput("budgetProfitInput",
                  paste(emo::ji("dollar_banknote"), "Select Metric:"),
                  choices = c("Budget", "Profit"),
                  selected = "Profit"
                 ),
      selectInput("seasonInput",
                  paste(emo::ji("deciduous_tree"), "Select Season:"),
                  choices = c("All", "Winter", "Spring", "Summer", "Fall")
                 ),
      sliderInput("budgetInput",
                  paste(emo::ji("money_bag"), "Filter by Budget:"),
                  min = 0,
                  max = round_any(max(movie_df$Budget, na.rm = TRUE), 100000000, ceiling),
                  value = c(0, round_any(max(movie_df$Budget, na.rm = TRUE), 100000000, ceiling)),
                  step = 1000000,
                  ticks = FALSE
                 ),
      sliderInput("profitInput", 
                  paste(emo::ji("chart_increasing"), "Filter by Profit:"),
                  min = 0,
                  max = round_any(max(movie_df$Profit, na.rm = TRUE), 100000000, ceiling),
                  value = c(0, round_any(max(movie_df$Profit, na.rm = TRUE), 100000000, ceiling)),
                  step = 1000000,
                  ticks = FALSE
                 ),
      selectInput("learnMoreToggle",
                  paste(emo::ji("bar_chart"), "Learn More About this Project:"),
                  choices = c("Overview", 
                            "Visualization Design Choices", 
                            "Results Across Genres", 
                            "Results Across Seasons", 
                            "Results Across Countries", 
                            "Results Between Metrics"
                            )
                  ),

      conditionalPanel(
        condition = "input.deepDiveToggle == true",
        selectInput("directorInput",
                    paste(emo::ji("movie_camera"), "Select Director:"),
                    choices = c("All", sort(unique(movie_df$director)))
                   ),
        selectInput("countryInput",
                    paste(emo::ji("globe"), "Select Country:"),
                    choices = c("All", sort(unique(movie_df$country)))
                   ),
        sliderInput("scoreInput", 
                    paste(emo::ji("hundred_points"), "Filter by User Score:"),
                    min = 0,
                    max = 10,
                    value = c(0, 10), 
                    step = 0.1
                  ),
        selectInput("ratingInput",
                    paste(emo::ji("warning"), "Select Rating:"),
                    choices = c("All", sort(unique(movie_df$rating)))
                   ),
        sliderInput("runtimeInput", 
                    paste(emo::ji("hourglass_done"), "Filter by Runtime (minutes):"),
                    min = 0,
                    max = 210,
                    value = c(0, 210)
                   ),
      ),

      checkboxInput("deepDiveToggle", "Enable Deep Dive"),
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(paste(emo::ji("package"), "Across Genres"), 
        plotOutput("BoxPlot", height = "600px")),
      tabPanel(paste(emo::ji("herb"), "Across Seasons"), 
        plotOutput("SeasonBoxPlot", height = "600px")),
      tabPanel(paste(emo::ji("globe"), "Across Countries"), 
        plotOutput("FinanceMap", height = "600px")),
      tabPanel(paste(emo::ji("dollar_banknote"), "Between Metrics"), 
        plotOutput("BudgetProfitPlot", height = "600px")),
      tabPanel(paste(emo::ji("ledger"), "Details"), 
        tableOutput("FilteredDataTable")),
      tabPanel(paste(emo::ji("bar_chart"), "Results Discussion"), 
        tableOutput("learnMore"))
      )
    )
  )
)
library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(haven)
library(lubridate)

# Load and clean data ----------------------------------------------
ieq <- read.csv('Daily_IEQ_Weather_Data.csv')
ieq$Date <- as.Date(ieq$Date, format = "%Y-%m-%d")
survey <- read.csv('Survey_Data.csv')
xwalk <- read.csv('Crosswalk.csv')

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Weatherization Results Dashboard",
                          titleWidth = 400)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(width = 300,
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Indoor Temperature and Humidity", icon = icon("temperature-half"), tabName = "ieq"),
    menuItem("Weather Data", icon = icon("sun"), tabName = "weather"),
    menuItem("Survey Data", icon = icon("clipboard"), tabName = "survey"),
    
    # Inputs: select variables to plot ----------------------------------------------
    selectInput("homeSelect",
                "Homes:",
                choices = sort(unique(ieq$SerialNoFactor)),
                multiple = TRUE,
                selectize = TRUE,
                selected = sort(unique(ieq$SerialNoFactor))),
    
    # Birth year Selection ----------------------------------------------
    selectInput("timeSelect",
                "Months:",
                choices = sort(unique(ieq$month)),
                multiple = TRUE,
                selectize = TRUE,
                selected = sort(unique(ieq$month)))
  )
)








# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # IEQ page ----------------------------------------------
  tabItem("ieq",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("mass"),
            valueBoxOutput("height")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            box(title = "Indoor Temperature",
                   width = 12,
                   plotlyOutput("plot_temp"))
          )
  ),
  
  # Weather Page ----------------------------------------------
  tabItem("weather",
          fluidPage(
            box(title = "Selected Character Stats", width = 12))
  ),
  
  # Survey page ----------------------------------------------
  tabItem("survey",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("mass"),
            valueBoxOutput("height")
          )
  )
)
)

ui <- dashboardPage(header, sidebar, body)







# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  homeData <- reactive({
      
      # Time Slider Filter ----------------------------------------------
    if (length(input$timeSelect) > 0 ) {
      ieq <- subset(ieq, month %in% input$timeSelect)
    }
    
    # Home Filter ----------------------------------------------
    if (length(input$homeSelect) > 0 ) {
      ieq <- subset(ieq, SerialNoFactor %in% input$homeSelect)
    }
    
    # Return dataframe ----------------------------------------------
    return(ieq)
  })
  
  # A plot showing temperature over time for each home -----------------------------------
  output$plot_temp <- renderPlotly({
    ggplot(data = homeData(), aes(x=Date)) + 
      geom_line(aes(y=ind.temp, color="darkred")) +
      geom_line(aes(y=ind.RH, color="steelblue", linetype = "twodash")) +
      facet_wrap(~ SerialNoFactor)
  })
  
}





# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
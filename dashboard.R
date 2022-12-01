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
    
    # Month Selection ----------------------------------------------
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
        fluidPage(
            box(title = "Indoor Temperature",
                   width = 12,
                   plotlyOutput("plot_temp"))
          )
  ),
  
  # Weather Page ----------------------------------------------
  tabItem("weather",
          fluidPage(
            box(title = "Indoor vs. Outdoor Temperature", 
                width = 12,
                plotlyOutput("plot_weather")))
  ),
  
  # Survey page ----------------------------------------------
  tabItem("survey",
          
          # Input and Value Boxes ----------------------------------------------
          fluidPage(
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
      geom_line(aes(y=ind.temp)) +
      geom_line(aes(y=ind.RH)) +
      facet_wrap(~ SerialNoFactor)
  })
  
  # A plot showing indoor vs. outdoor temperature for each home -----------------------------------
  output$plot_weather <- renderPlotly({
    ggplot(data=homeData(), aes(x=out.temp, y=ind.temp, color=Pre_Post)) + 
      geom_point(alpha=.3) + 
      geom_smooth(method="lm", se = FALSE) + 
      facet_wrap(~SerialNoFactor) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      xlab('Outdoor Temperature (Daily Average, F)') + 
      ylab('Indoor Temperature (Daily Average, F)')
  })
  
}





# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
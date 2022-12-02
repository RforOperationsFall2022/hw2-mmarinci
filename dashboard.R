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
survey$Date <- as.Date(survey$Date, format = "%Y-%m-%d")

xwalk <- read.csv('Crosswalk.csv')
names(xwalk)[names(xwalk) == "Serial"] <- "SerialNo"
survey <- merge(xwalk, survey)

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
    sliderInput("timeSelect",
                "Time Frame:",
                min = min(ieq$Date),
                max = max(ieq$Date),
                value = c(min(ieq$Date), max(ieq$Date)),
                timeFormat = "%Y-%m-%d")
  )
)








# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # IEQ page ----------------------------------------------
  tabItem("ieq",
        fluidPage(
            tabBox(title = "IEQ",
                   width = 12,
                   tabPanel("Indoor Temperature", plotlyOutput("plot_temp")),
                   tabPanel("Indoor RH", plotlyOutput("plot_RH"))
          )
  )
  ),
  
  # Weather Page ----------------------------------------------
  tabItem("weather",
          fluidPage(
            tabBox(title = "Weather Impacts", 
                width = 12,
                tabPanel("Indoor vs. Outdoor Temperature", plotlyOutput("plot_weather_temp")),
                tabPanel("Indoor vs. Outdoor Relative Humidity", plotlyOutput("plot_weather_RH")))
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
)


ui <- dashboardPage(header, sidebar, body)







# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  homeData <- reactive({
      
      # Time Slider Filter ----------------------------------------------
    ieq <- filter(ieq, ieq$Date >= input$timeSelect[1] & ieq$Date <= input$timeSelect[2])
    
    # Home Filter ----------------------------------------------
    if (length(input$homeSelect) > 0 ) {
      ieq <- subset(ieq, SerialNoFactor %in% input$homeSelect)
    }
    
    # Return dataframe ----------------------------------------------
    return(ieq)
  })
  
  # A plot showing temperature over time for each home -----------------------------------
  output$plot_temp <- renderPlotly({
    ggplot(data = homeData(), aes(x=Date, y=ind.temp, color=Pre_Post)) + 
      geom_line() +
      facet_wrap(~ SerialNoFactor) + 
      ylab('Indoor Temperature (Daily Average, F)') +
      theme_classic()
  })
  
  # A plot showing RH over time for each home -----------------------------------
  output$plot_RH <- renderPlotly({
    ggplot(data = homeData(), aes(x=Date, y=ind.RH, color=Pre_Post)) + 
      geom_line() +
      facet_wrap(~ SerialNoFactor) + 
      ylab('Indoor Relative Humidity (Daily Average, %)') +
      theme_classic()
  })
  
  # A plot showing indoor vs. outdoor temperature for each home -----------------------------------
  output$plot_weather_temp <- renderPlotly({
    ggplot(data=homeData(), aes(x=out.temp, y=ind.temp, color=Pre_Post)) + 
      geom_point(alpha=.3) + 
      geom_smooth(method="lm", se = FALSE) + 
      facet_wrap(~SerialNoFactor) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      xlab('Outdoor Temperature (Daily Average, F)') + 
      ylab('Indoor Temperature (Daily Average, F)') +
      theme_classic()
  })
  
  # A plot showing indoor vs. outdoor RH for each home -----------------------------------
  output$plot_weather_RH <- renderPlotly({
    ggplot(data=homeData(), aes(x=out.RH, y=ind.RH, color=Pre_Post)) + 
      geom_point(alpha=.3) + 
      geom_smooth(method="lm", se = FALSE) + 
      facet_wrap(~SerialNoFactor) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      xlab('Outdoor Relative Humidity (Daily Average, %)') + 
      ylab('Indoor Relative Humidity (Daily Average, %)') +
      theme_classic()
  })
  
}





# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
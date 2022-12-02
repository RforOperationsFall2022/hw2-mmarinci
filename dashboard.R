library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(haven)
library(lubridate)
library(scales)

# Load and clean data ----------------------------------------------
ieq <- read.csv('Daily_IEQ_Weather_Data.csv') %>%
  transform(Pre_Post = factor(Pre_Post, levels = c("Pre-Weatherization", "Post-Weatherization")))
ieq$Date <- as.Date(ieq$Date, format = "%Y-%m-%d")

survey <- read.csv('Survey_Data.csv')

xwalk <- read.csv('Crosswalk.csv')
names(xwalk)[names(xwalk) == "Serial"] <- "SerialNo"
survey <- merge(xwalk, survey) %>%
  transform(Pre_Post = factor(Pre_Post, levels = c("Pre-Weatherization", "Post-Weatherization"))) %>%
  transform(C1_How_Hard_Pay_Energy_Bill = factor(C1_How_Hard_Pay_Energy_Bill, 
                                                 levels = c("Very Easy", "Easy", "Neither Hard nor Easy", "Hard", "Very Hard")))


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
    
    # Time Selection ----------------------------------------------
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
  )
  ),
  
  # Survey page ----------------------------------------------
  tabItem("survey",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            valueBoxOutput("unsafe_pre"),
            valueBoxOutput("unsafe_post"),
            valueBoxOutput("COPD")
          ),
          
          fluidRow(
            box(title = "Comfort Level",
                width = 12,
                plotlyOutput("comfort"))
          ),
          
          fluidRow(
            box(title = "Difficulty Paying Energy Bill",
                width = 12,
                plotlyOutput("bills"))
          )
  )
)
)


ui <- dashboardPage(header, sidebar, body)







# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive IEQ & weather data function -------------------------------------------
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
  
  # Reactive survey data function -------------------------------------------
  surveyData <- reactive({
    
    # Home Filter ----------------------------------------------
    if (length(input$homeSelect) > 0 ) {
      survey <- subset(survey, SerialNo %in% input$homeSelect)
    }
    
    # Return dataframe ----------------------------------------------
    return(survey)
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
  
  # Percent of people with unsafe temperatures before and after
  output$unsafe_pre <- renderValueBox({
    dat <- surveyData() %>%
      filter(Pre_Post == "Pre-Weatherization")
    num <- scales::percent(mean(dat$A2_Unsafe_Indoor_Temp, na.rm = T))
    valueBox(subtitle = "Reported Unsafe Temperatures, Pre", value = num, color = "orange")
  })
  
  output$unsafe_post <- renderValueBox({
    dat <- surveyData() %>%
      filter(Pre_Post == "Post-Weatherization")
    num <- scales::percent(mean(dat$A2_Unsafe_Indoor_Temp, na.rm = T))
    valueBox(subtitle = "Reported Unsafe Temperatures, Post", value = num, color = "green")
  })
  
  output$COPD <- renderValueBox({
    dat <- surveyData() %>%
      filter(Pre_Post == "Pre-Weatherization")
    num <- scales::percent(mean(dat$B2_Have_Asthma, na.rm = T))
    valueBox(subtitle = "Have Asthma", value = num, color = "light-blue")
  })
  
  # Histogram of comfort before and after
  output$comfort <- renderPlotly({
    ggplot(surveyData(), aes(x = A1_Last_Summer_Indoor_Temp)) +
      geom_histogram(stat = "count") +
      facet_wrap(~ Pre_Post) +
      ylab("Count") +
      xlab("How would you describe the temperature in your home during summer?") +
      theme_classic() +
      theme(axis.title.x = element_text(size = 13))
  })
  
  # Histogram of difficulty paying bills before and after
  output$bills <- renderPlotly({
    ggplot(surveyData(), aes(x = C1_How_Hard_Pay_Energy_Bill)) +
      geom_histogram(stat = "count") +
      facet_wrap(~ Pre_Post) +
      ylab("Count") +
      xlab("How difficult is it for you to pay your energy bill?") +
      theme_classic() +
      theme(axis.title.x = element_text(size = 13))
  })
  
}





# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
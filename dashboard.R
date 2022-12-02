library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(haven)
library(lubridate)
library(scales)

# Load and clean IEQ data ----------------------------------------------
ieq <- read.csv('Daily_IEQ_Weather_Data.csv') %>%
  transform(Pre_Post = factor(Pre_Post, levels = c("Pre-Weatherization", "Post-Weatherization"))) # Set order so pre displays first
ieq$Date <- as.Date(ieq$Date, format = "%Y-%m-%d")

# Load and clean survey data
survey <- read.csv('Survey_Data.csv')
xwalk <- read.csv('Crosswalk.csv') # Crosswalk between data monitor serial number and home ID
names(xwalk)[names(xwalk) == "Serial"] <- "SerialNo"

# Add serial numbers to survey data and set factor variables
survey <- merge(xwalk, survey) %>%
  subset(select = -c(Site_ID)) %>%
  transform(Pre_Post = factor(Pre_Post, levels = c("Pre-Weatherization", "Post-Weatherization"))) %>%
  transform(C1_How_Hard_Pay_Energy_Bill = factor(C1_How_Hard_Pay_Energy_Bill, 
                                                 levels = c("Very Easy", "Easy", "Neither Hard nor Easy", "Hard", "Very Hard")))

# Avoid plotly issues ----------------------------------------------
pdf(NULL)



# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Weatherization Results",
                          titleWidth = 300)

# Dashboard Sidebar -----------------------------------------------------------
sidebar <- dashboardSidebar(width = 300,
  sidebarMenu(
    id = "tabs",
    
    # Menu Items --------------------------------------------------------------
    menuItem("Indoor Temperature and Humidity", icon = icon("temperature-half"), tabName = "ieq"),
    menuItem("Weather Data", icon = icon("sun"), tabName = "weather"),
    menuItem("Survey Data", icon = icon("clipboard"), tabName = "survey"),
    
    # Inputs: select variables to plot ----------------------------------------
    selectInput("homeSelect",
                "Select Homes to View:",
                choices = sort(unique(ieq$SerialNoFactor)),
                multiple = TRUE,
                selectize = TRUE,
                selected = sort(unique(ieq$SerialNoFactor))),
    
    # Pre/Post Weatherization Selection ---------------------------------------
    checkboxGroupInput("statusSelect",
                       "Weatherization Status:",
                       choices = unique(ieq$Pre_Post),
                       selected = unique(ieq$Pre_Post)),
    
    # Time Selection ----------------------------------------------------------
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
        fluidPage(theme = shinytheme("cosmo"),
          div("Indoor Environmental Quality (IEQ)", style = "font-size:20pt; padding:15px; color:black"),
          div("The goal of this research was to measure the real impacts of home weatherization on indoor temperature and humidity. The graphs below show trends in these two measures over time in homes before and after receiving weatherization during the summer of 2021.", style = "font-size:13pt; padding:15px"),
          tabBox(title = "",
                   width = 12,
                   tabPanel("Indoor Temperature", plotlyOutput("plot_temp")),
                   tabPanel("Indoor RH", plotlyOutput("plot_RH"))
          )
  )
  ),
  
  # Weather Page ----------------------------------------------
  tabItem("weather",
          fluidPage(
            div("Weather and Home Comfort", style = "font-size:20pt; padding:15px; color:black"),
            div("In a home that has been properly weatherized, we expect outdoor weather conditions to have only a small impact on indoor comfort levels. A well-insulated home should stay warm regardless of how cold it gets outside, and vice versa. Therefore, in the graphs below, a relatively horizontal line indicates the house is well sealed, while a strongly upward sloping lines indicate the house is getting warmer as the heat builds outside.", style = "font-size:13pt; padding:15px"),
            tabBox(title = "", 
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
            valueBoxOutput("asthma")
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
          ),
          
          fluidRow(
            box(title = "Survey Data",
                width = 12,
                DT::dataTableOutput("dt"))
          )
  )
)
)


ui <- dashboardPage(header, sidebar, body, skin = "green")






# Define server function required to create plots and value boxes -----
server <- function(input, output, session) {
  
  observeEvent(input$statusSelect, {
      ieq <- subset(ieq, Pre_Post %in% input$statusSelect)
    
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session,
                      "timeSelect",
                      min = min(ieq$Date),
                      max = max(ieq$Date),
                      value = c(min(ieq$Date), max(ieq$Date)),
                      timeFormat = "%Y-%m-%d")
  })
  
  # Reactive IEQ & weather data function -------------------------------------------
  homeData <- reactive({
      
      # Time Slider Filter ----------------------------------------------
    ieq <- filter(ieq, ieq$Date >= input$timeSelect[1] & ieq$Date <= input$timeSelect[2])
    
    # Home Filter ----------------------------------------------
    if (length(input$homeSelect) > 0 ) {
      ieq <- subset(ieq, SerialNoFactor %in% input$homeSelect)
    }
      
      # Wx Status Filter ----------------------------------------------
      if (length(input$statusSelect) > 0 ) {
        ieq <- subset(ieq, Pre_Post %in% input$statusSelect)
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
    
    # Wx Status Filter ----------------------------------------------
    if (length(input$statusSelect) > 0 ) {
      survey <- subset(survey, Pre_Post %in% input$statusSelect)
    }
    
    # Return dataframe ----------------------------------------------
    return(survey)
  })
  
  # Reactive box data function -------------------------------------------
  valueData <- reactive({
    
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
     # ggtitle('Temperature Inside Homes Receiving Weatherization Services') +
      ylab('Indoor Temperature (Daily Average, F)') +
      theme_classic() +
      theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=1)) + 
      scale_color_manual(values=c("orange","skyblue"), name = "")
  })
  
  # A plot showing RH over time for each home -----------------------------------
  output$plot_RH <- renderPlotly({
    ggplot(data = homeData(), aes(x=Date, y=ind.RH, color=Pre_Post)) + 
      geom_line() +
      facet_wrap(~ SerialNoFactor) + 
    #  ggtitle('Relative Humidity (RH) Inside Homes Receiving Weatherization Services') +
      ylab('Indoor Relative Humidity (Daily Average, %)') +
      theme_classic() +
      theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=1)) + 
      scale_color_manual(values=c("orange","skyblue"), name = "")
  })
  
  # A plot showing indoor vs. outdoor temperature for each home -----------------------------------
  output$plot_weather_temp <- renderPlotly({
    ggplot(data=homeData(), aes(x=out.temp, y=ind.temp, color=Pre_Post)) + 
      geom_smooth(method="lm", se = FALSE) + 
      facet_wrap(~SerialNoFactor) + 
      xlab('Outdoor Temperature (Daily Average, F)') + 
      ylab('Indoor Temperature (Daily Average, F)') +
      theme_classic() +
      theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=1)) + 
      scale_color_manual(values=c("orange","skyblue"), name = "")
  })
  
  # A plot showing indoor vs. outdoor RH for each home -----------------------------------
  output$plot_weather_RH <- renderPlotly({
    ggplot(data=homeData(), aes(x=out.RH, y=ind.RH, color=Pre_Post)) + 
      geom_smooth(method="lm", se = FALSE) + 
      facet_wrap(~SerialNoFactor) + 
      xlab('Outdoor Relative Humidity (Daily Average, %)') + 
      ylab('Indoor Relative Humidity (Daily Average, %)') +
      theme_classic() +
      theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=1)) + 
      scale_color_manual(values=c("orange","skyblue"), name = "")
  })
  
  # Percent of people with unsafe temperatures before and after
  output$unsafe_pre <- renderValueBox({
    dat <- valueData() %>%
      filter(Pre_Post == "Pre-Weatherization")
    num <- scales::percent(mean(dat$A2_Unsafe_Indoor_Temp, na.rm = T))
    valueBox(subtitle = "Reported Unsafe Temperatures, Pre", value = num, icon = icon("triangle-exclamation"), color = "orange")
  })
  
  output$unsafe_post <- renderValueBox({
    dat <- valueData() %>%
      filter(Pre_Post == "Post-Weatherization")
    num <- scales::percent(mean(dat$A2_Unsafe_Indoor_Temp, na.rm = T))
    valueBox(subtitle = "Reported Unsafe Temperatures, Post", value = num, icon = icon("temperature-arrow-down"), color = "green")
  })
  
  output$asthma <- renderValueBox({
    dat <- valueData() %>%
      filter(Pre_Post == "Pre-Weatherization")
    num <- scales::percent(mean(dat$B2_Have_Asthma, na.rm = T))
    valueBox(subtitle = "Have Asthma", value = num, icon = icon("lungs"), color = "light-blue")
  })
  
  # Histogram of comfort before and after -------------------------------------
  output$comfort <- renderPlotly({
    ggplot(surveyData(), aes(x = A1_Last_Summer_Indoor_Temp)) +
      geom_histogram(stat = "count") +
      facet_wrap(~ Pre_Post) +
      ylab("Count") +
      xlab("How would you describe the temperature in your home during summer?") +
      theme_classic() +
      theme(axis.title.x = element_text(size = 13))
  })
  
  # Histogram of difficulty paying bills before and after ---------------------
  output$bills <- renderPlotly({
    ggplot(surveyData(), aes(x = C1_How_Hard_Pay_Energy_Bill)) +
      geom_histogram(stat = "count") +
      facet_wrap(~ Pre_Post) +
      ylab("Count") +
      xlab("How difficult is it for you to pay your energy bill?") +
      theme_classic() +
      theme(axis.title.x = element_text(size = 13))
  })
  
  # Data Table ----------------------------------------------------------------
  output$dt <- DT::renderDataTable(
    subset(surveyData(), select = c(SerialNo, Pre_Post, A1_Last_Summer_Indoor_Temp, A2_Unsafe_Indoor_Temp, B2_Have_Asthma, C1_How_Hard_Pay_Energy_Bill)) %>%
      setNames(c("Serial_Number", "Pre_Post", "Summer_Indoor_Temp", "Unsafe_Indoor_Temp", "Have_Asthma", "How_Hard_Pay_Energy_Bill")),
    rownames = FALSE
    )
  
}





# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
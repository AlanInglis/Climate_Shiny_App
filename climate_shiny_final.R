library(shiny)
library(ggplot2)
library(mgcv)  # For GAM
library(readr)  # For read_csv()
library(forecast)

# need this line to deploy app
source("pred_plot_V2.R")  

# Preload datasets
dataset1 <- read_csv("global_temps.csv", col_types = cols(Year = col_double(), Temperature = col_double()))
dataset2 <- read_csv("global_temps_fake.csv", col_types = cols(Year = col_double(), Temperature = col_double()))

# datasets for dropdown
datasets <- list("Dataset 1" = dataset1, "Dataset 2" = dataset2)

# shiny function
shinyGlobalTemp <- function() {
  ui <- fluidPage(
    titlePanel("Temperature Prediction"),
    
    sidebarLayout(
      sidebarPanel(
        # Dropdown for preloaded datasets
        selectInput("selectedDataset", "Choose a dataset:", choices = names(datasets)),
        # select prediction method
        selectInput("predictionMethod", "Select Prediction Method:",
                    choices = c("GAM" = "GAM", "ARIMA" = "ARIMA", "LM" = "LM", "GLM" = "GLM")),
        # select moving average
        selectInput("showMA", "Select Moving Average:",
                    choices = c("None" = "none", "CMA" = "CMA", "OSMA" = "OSMA")),
        # moving average window size
        numericInput("windowSize", "Window Size for Moving Average:", value = 5, min = 1, step = 1),
        # show original data
        radioButtons("showData", "Display Original Data Line:",
                     choices = list("Show" = "TRUE", "Hide" = "FALSE"),
                     selected = "TRUE"),
        # show text
        wellPanel(
          div(style = "text-align: center;", htmlOutput("targetYearMessage")),
          div(style = "text-align: center;", htmlOutput("baseTempText"))
        )
      ),
      
      mainPanel(
        #main plot
        plotOutput("temperaturePlot", width = "100%", height = "600px"),
        # slider bar
        sliderInput("yearRange", "Select Year Range:",
                    width = "100%",
                    min = 1880, max = 2023, 
                    value = c(1900, 1950), step = 1, sep = "")
      )
    )
  )
  
  server <- function(input, output, session) {
    # reactive box for the selected data
    selectedData <- reactive({
      datasets[[input$selectedDataset]]
    })
    
    output$baseTempText <- renderText({
      df <- selectedData()
      baseTempRange <- input$yearRange
      adjustedData <- df[df$Year >= baseTempRange[1] & df$Year <= baseTempRange[2], ]
      bt <- mean(adjustedData$Temperature, na.rm = TRUE)
      HTML(sprintf("‣ Base Temperature: <b>%.2f°C</b>", bt))
    })
    
    # actual function in pred_plot.R
    output$temperaturePlot <- renderPlot({
      df <- selectedData()
      baseTempRange <- input$yearRange
      result <- generate_pred_plot(df, method = input$predictionMethod, windowSize = input$windowSize, showMA = input$showMA, baseTemp = baseTempRange, showData = as.logical(input$showData))
      output$targetYearMessage <- renderText({ HTML(sprintf("‣ 1.5°C increase by: <b>%s</b>", result$target_year)) })
      return(result$plot)
    })
  }
  
  shinyApp(ui = ui, server = server)
}

shinyGlobalTemp()

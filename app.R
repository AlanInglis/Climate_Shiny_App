

library(shiny)
library(ggplot2)
library(mgcv)  # For GAM
library(readr)  # For read_csv()

source("climate_pred_plot.R")


shinyGlobalTemp <- function() {
  ui <- fluidPage(
    titlePanel("Temperature Prediction"),
    
    # Main sidebar and plot layout
    sidebarLayout(
      sidebarPanel(
        fileInput("fileUpload", "Upload CSV Files", accept = ".csv", multiple = TRUE),
        uiOutput("datasetSelectorUI"),  # For dataset selection
        selectInput("predictionMethod", "Select Prediction Method:",
                    choices = c("GAM" = "GAM", "LM" = "LM", "GLM" = "GLM")),
        
        # Positioning the wellPanel directly in sidebar
        wellPanel(
          div(style = "text-align: center;", htmlOutput("targetYearMessage")),
          div(style = "text-align: center;", htmlOutput("baseTempText")),
        )
      ),
      mainPanel(
        plotOutput("temperaturePlot", width = "100%", height = "600px"),
        # slider below the plot
        sliderInput("yearRange", "Select Year Range:",
                    width = "100%",
                    min = 1800, max = 2023, 
                    value = c(1800, 1850), step = 1, sep = "")
      )
    )
  )
  server <- function(input, output, session) {
    # Store uploaded data
    datasets <- reactiveValues(dataList = list())
    
    observeEvent(input$fileUpload, {
      req(input$fileUpload)
      # Reset dataList when new files are uploaded
      datasets$dataList <- list()
      # Read each uploaded file
      for (i in seq_along(input$fileUpload$name)) {
        file <- input$fileUpload$datapath[i]
        datasets$dataList[[input$fileUpload$name[i]]] <- readr::read_csv(file, col_types = cols(
          Year = col_double(),
          Temperature = col_double()
        ))
      }
      # Update dataset selection dropdown
      updateSelectInput(session, "selectedDataset", choices = names(datasets$dataList))
    })
    
    output$datasetSelectorUI <- renderUI({
      if (length(datasets$dataList) > 0) {
        selectInput("selectedDataset", "Choose a dataset:", choices = names(datasets$dataList))
      }
    })
    
    # Reactive expression for the selected dataset
    selectedData <- reactive({
      req(input$selectedDataset)
      datasets$dataList[[input$selectedDataset]]
    })
    
    observe({
      df <- selectedData()
      if (!is.null(df)) {
        yrRange <- range(df$Year, na.rm = TRUE)
        updateSliderInput(session, "yearRange", min = min(yrRange), max = max(yrRange), value = yrRange)
      }
    })
    
    output$baseTempText <- renderText({
      df <- selectedData()
      if (is.null(df)) return("‣ Base Temperature: NA")
      baseTempRange <- input$yearRange
      adjustedData <- df[df$Year >= baseTempRange[1] & df$Year <= baseTempRange[2], ]
      bt <- mean(adjustedData$Temperature, na.rm = TRUE)
      HTML(sprintf("‣ Base Temperature: <b>%.2f°C</b>", bt))
    })
    
    output$temperaturePlot <- renderPlot({
      df <- selectedData()
      if (is.null(df)) return()
      baseTempRange <- input$yearRange 
      result <- generate_prediction_plot(df, input$predictionMethod, baseTempRange)
      output$targetYearMessage <- renderText({  HTML(sprintf("‣ 1.5°C increase by: <b>%s</b>", result$target_year)) })
      return(result$plot)
    })
    
  }
  
  shinyApp(ui = ui, server = server)
}



shinyGlobalTemp()


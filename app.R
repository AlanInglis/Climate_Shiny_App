library(shiny)
library(tidyverse)
library(shinyWidgets)
library(mgcv)
library(forecast)

# UI
ui <- fluidPage(
  titlePanel("Have we reached 1.5°C yet?"),
  sidebarLayout(
    sidebarPanel(
      # select data
      selectInput("dataName", "Choose a dataset", choices = c("NASA_GISS", "HADCRUT5", "CRUTEM5", "HADSST", "NOAA_NCEI", "ERA_5", "HAD_CRUT4_Krig", "Berkeley")),
      # select years
      uiOutput("yearInput"),
      # select smoothing method
      pickerInput("method", "Choose a method", choices = c("LOESS", "Spline", "OSMA10", "OSMA20", "COR", "ARIMA", "30yrlt", "20yrlt"), options = list(`style` = "btn-info")),
      # add text
      wellPanel(
        div(style = "text-align: center;", htmlOutput("tempAnomalyMessage"))
      )
    ),
    mainPanel(
      plotOutput("anomalyPlot", height = "50vh")
    )
  )
)

#  server 
server <- function(input, output) {
  output$yearInput <- renderUI({
    startYears <- c(NASA_GISS = 1880, HADCRUT5 = 1850, CRUTEM5 = 1857, HADSST = 1850, NOAA_NCEI = 1880, ERA_5 = 1950, HAD_CRUT4_Krig = 1850, Berkeley = 1850)
    
    startYear <- startYears[input$dataName]
    endYear <- startYear + 20  # First 20 years
    
    tagList(
      numericInput("minYear", "Start of pre-industrial period", value = startYear, min = startYear, max = 2024),
      numericInput("maxYear", "End of pre-industrial period", value = endYear, min = startYear, max = 2024)
    )
  })
  
  calcValues <- reactive({
    req(input$dataName, input$method)
    data_name <- input$dataName
    method <- input$method
    
    # Load in the data set using data_name
    data <- read_csv(paste0("data/", data_name, ".csv"), show_col_types = FALSE) %>% na.omit()
    
    # Renormalise the data based on the selected pre-industrial period
    data$Anomaly <- data$Anomaly - mean(data$Anomaly[data$Year >= input$minYear & data$Year <= input$maxYear])
    
    # Fit the method to the data set
    if(method == "Spline") {
      model_fit <- gam(Anomaly ~ s(Year_num), data = data)
      data$Smooth <- model_fit$fitted.values
    } else if(method == "AR1") {
      ts_data <- ts(data$Anomaly, start = min(data$Year_num), frequency = 1)
      model_fit <- Arima(ts_data, order = c(1,0,0))
      data$Smooth <- model_fit$fitted
    } else if(method == "ARIMA") {
        ts_data <- ts(data$Anomaly, start = min(data$Year_num), frequency = 1)
        model_fit <- forecast::auto.arima(ts_data)
        data$Smooth <- model_fit$fitted
    } else if(method == "COR") {
      model_fit <- lm(Anomaly ~ poly(Year_num, 3), data = data)
      data$Smooth <- model_fit$fitted.values
    } else if(method == "OSMA20") {
      model_fit <- stats::filter(data$Anomaly, rep(1 / 240, 240), sides = 1)
      data$Smooth <- model_fit
    } else if(method == "OSMA10") {
      model_fit <- stats::filter(data$Anomaly, rep(1 / 120, 120), sides = 1)
      data$Smooth <- model_fit
    } else if(method == "LOESS") {
      model_fit <- stats::loess(Anomaly ~ Year_num, data = data)
      data$Smooth <- model_fit$fitted
    } else if(method == "30yrlt") {
      # Estimate a 30 year linear trend
      data$Smooth <- rep(NA, nrow(data))
      for(i in 30:nrow(data)) {
        data$Smooth[i] <- lm(Anomaly[(i-29):i] ~ Year_num[(i-29):i], data = data)$fitted.values[30]
      }
    } else if(method == "20yrlt") {
      # Estimate a 20 year linear trend
      data$Smooth <- rep(NA, nrow(data))
      for(i in 20:nrow(data)) {
        data$Smooth[i] <- lm(Anomaly[(i-19):i] ~ Year_num[(i-19):i], data = data)$fitted.values[20]
      }
    }
    
    # Add method descriptions
    method_desc <- data.frame(
      names = c("LOESS", "Spline", "AR1", "OSMA10", "OSMA20", "COR", 
                "ARIMA", "30yrlt", "20yrlt"),
      detail = c("Local polynomial regression",
                 "Penalised cubic regression spline", 
                 "Auto-regressive model with order 1", 
                 "One sided moving average over 10 years", 
                 "One sided moving average over 20 years", 
                 "Cubic orthogonal regression",
                 "Best fit ARIMA model",
                 "Last point of 30-year linear trend",
                 "Last point of 20-year linear trend")
    )
    
    curr_pred_temp <- data$Smooth[nrow(data)]
    curr_year <- data$Year[nrow(data)]
    curr_month <- data$Month[nrow(data)]
    
    message <- paste0('Smoothed line = ',
                      method_desc$detail[method_desc$names == method])
    
    message_2 <- paste0("The current temperature anomaly is <b>", round(curr_pred_temp, 2), "°C in ", 
                        month.abb[curr_month], '-', curr_year, "</b> above pre-industrial levels.")
    
    
    list(data = data, message = message, message_2 = message_2)
    
  })
  
  output$anomalyPlot <- renderPlot({
    values <- calcValues()
    data <- values$data
    message <- values$message
    data_name <- input$dataName
    method <- input$method
    
    # Create the ggplot
    data %>% pivot_longer(cols = c(Anomaly, Smooth), 
                          names_to = "Type", values_to = "Value") %>% 
      mutate(Type = factor(Type, levels = c("Anomaly", "Smooth"), 
                           ordered = TRUE)) %>%
      na.omit() %>% 
      ggplot(aes(x = Year_num, y = Value, colour = Type)) +
      annotate("text", x = mean(range(data$Year)), 
               y = mean(range(data$Anomaly)),
               label = paste0(round(data$Smooth[nrow(data)], 2), "°C"),
               size = 160/.pt, alpha = 0.05) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_line(alpha = 0.7) + 
      scale_colour_manual(values = c("black", "red")) +
      theme_bw() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) + 
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
      labs(title = paste0(data_name," with ", method, " smoothing"), 
           subtitle = message,
           y = "Temperature anomaly (°C)", x = "Year") + 
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,0,-10,-10))
  })
  
  # used to make the text in text box
  output$tempAnomalyMessage <- renderUI({
    values <- calcValues()
    HTML(values$message_2)
  })
}

# run
shinyApp(ui = ui, server = server)

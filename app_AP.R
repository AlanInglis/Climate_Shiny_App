library(shiny)
library(tidyverse)
library(shinyWidgets)
library(mgcv)
library(forecast)

# Define the UI
ui <- fluidPage(
  titlePanel("Have we reached 1.5C yet?"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataName", "Choose a dataset", choices = c("HADCRUT5", 
                                                              "CRUTEM5", 
                                                              "HADSST",
                                                              "GISTEMP",
                                                              "NOAA_NCEI",
                                                              "ERA_5",
                                                              "HAD_CRUT4_Krig",
                                                              "Berkeley")),
      numericInput("minYear", "Start of pre-industrial period", value = 1850, min = 1850, max = 2024),
      numericInput("maxYear", "End of pre-industrial period", value = 1870, min = 1850, max = 2024),
      pickerInput("method", "Choose a method", choices = c("Spline", "AR1", "OSMA10", "OSMA20", "COR"), options = list(`style` = "btn-info")),
      width = 3
    ),
    mainPanel(
      plotOutput("anomalyPlot"),
      width = 9
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$anomalyPlot <- renderPlot({
    data_name <- input$dataName
    method <- input$method
    
    # Load in the data set using data_name
    data <- read_csv(paste0("data/",data_name,".csv"), 
                     show_col_types = FALSE) %>% 
      na.omit()
    
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
    } else if(method == "COR") {
      model_fit <- lm(Anomaly ~ poly(Year_num, 3), data = data)
      data$Smooth <- model_fit$fitted.values
    } else if(method == "OSMA20") {
      model_fit <- stats::filter(data$Anomaly, rep(1 / 240, 240), sides = 1)
      data$Smooth <- model_fit
    } else if(method == "OSMA10") {
      model_fit <- stats::filter(data$Anomaly, rep(1 / 120, 120), sides = 1)
      data$Smooth <- model_fit
    }
    
    # Add method descriptions
    method_desc <- data.frame(
      names = c("Spline", "AR1", "OSMA10", "OSMA20", "COR"),
      detail = c("Penalised cubic regression spline", 
                 "Auto-regressive model with order 1", 
                 "One sided moving average of 10 years", 
                 "One sided moving average of 20 years", 
                 "Cubic orthogonal regression")
    )
    
    
    # Get the message
    curr_pred_temp <- data$Smooth[nrow(data)]
    curr_year <- data$Year[nrow(data)]
    curr_month <- data$Month[nrow(data)]
    message <- paste0("The current temperature anomaly is ", round(curr_pred_temp, 2), "°C in ", 
                      month.abb[curr_month],'-',curr_year, 
                      ' above pre-industrial levels.\nSmoothed line = ',
                      method_desc$detail[method_desc$names == method])
    
    # Create the ggplot
    data %>% pivot_longer(cols = c(Anomaly, Smooth), 
                          names_to = "Type", values_to = "Value") %>% 
      mutate(Type = factor(Type, levels = c("Anomaly", "Smooth"), 
                           ordered = TRUE)) %>%
      na.omit() %>% 
      ggplot(aes(x = Year_num, y = Value, colour = Type)) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_line(alpha = 0.7) + 
      scale_colour_manual(values = c("black", "red")) +
      theme_bw() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
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
}

# Run the application
shinyApp(ui = ui, server = server)

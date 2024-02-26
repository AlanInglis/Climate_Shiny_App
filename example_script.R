# Example script

# Load in some packages
library(tidyverse)

# Choose a data set
data_name <- "HADCRUT5"

# Choose a method - Spline, AR1, OSMA20, cubicreg, OSMA10
method <- "OSMA10"
    
method_desc <- data.frame(
  names = c("Spline", "AR1", "OSMA10", "OSMA20", "COR"),
  detail = c("Spline = Penalised cubic regression spline", 
             "AR1 = Auto-regressive model with order 1", 
             "OSMA10 = One sided moving average of 10 years", 
             "OSMA20 = One sided moving average of 20 years", 
             "COR = Cubic orthogonal regression")
)

# Load in the data set using data_name
data <- read_csv(paste0("data/",data_name,".csv"),
                 show_col_types = FALSE) %>% 
  na.omit()

# Select a default pre-industrial period
min_baseline_year <- min(data$Year)
max_baseline_year <- min_baseline_year + 20

# Renormalise the data based on this period
data$Temperature <- data$Temperature - mean(data$Temperature[data$Year >= min_baseline_year & data$Year <= max_baseline_year])

# Fit the method to the data set
if(method == "Spline") {
  model_fit <- mgcv::gam(Temperature ~ s(Year), data = data)
  data$Smooth <- model_fit$fitted.values
} else if(method == "AR1") {
  ts_data <- ts(data$Temperature, start = min(data$Year), frequency = 1) # time series data
  model_fit <- forecast::Arima(ts_data, order = c(1,0,0))
  data$Smooth <- model_fit$fitted
} else if(method == "COR") {
  model_fit <- lm(Temperature ~ poly(Year, 3), data = data)
  data$Smooth <- model_fit$fitted.values
} else if(method == "OSMA20") {
  # Create a one sided moving average of 240 months to smooth the data
  model_fit <- stats::filter(data$Temperature, rep(1 / 240, 240), sides = 1)
  data$Smooth <- model_fit
} else if(method == "OSMA10") {
  # Create a one sided moving average of 120 months to smooth the data
  model_fit <- stats::filter(data$Temperature, rep(1 / 120, 120), sides = 1)
  data$Smooth <- model_fit
} else {
  stop("Unsupported method")
}

# Create message
curr_pred_temp <- data$Smooth[nrow(data)]
curr_year <- data$Year[nrow(data)]
curr_month <- data$Month[nrow(data)]
message <- paste0("The current temperature is ", round(curr_pred_temp, 2), "°C in ", 
                 month.abb[curr_month],'-',curr_year, ' above\npre-industrial levels defined as ', min_baseline_year, 
                 ' to ', max_baseline_year)

data %>% pivot_longer(cols = c(Temperature, Smooth), 
                      names_to = "Type", values_to = "Value") %>% 
  mutate(Type = factor(Type, levels = c("Temperature", "Smooth"), 
                       ordered = TRUE)) %>%
  na.omit() %>% 
  ggplot(aes(x = Year, y = Value, colour = Type)) +
  geom_line(alpha = 0.7) + 
  scale_colour_manual(values = c("black", "red")) +
  theme_bw() +
  labs(y = "Temperature (°C)", x = "Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  labs(title = paste0(data_name," with ", method, " smoothing"), 
       subtitle = message,
       y = "Temperature (°C)", x = "Year") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.justification="right",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,0,-10,-10)) + 
  annotate("text", # Put at left hand edge of plot
           x = min(data$Year),
           y = max(data$Temperature), 
           label = method_desc$detail[method_desc$names == method], 
           hjust = 0, vjust = 0, size = 3)




# this function uses the CMA and OSMA as the data and make predictions from them


t4 <- function(data, method = "LM", baseTemp = NULL, windowSize = 5, showMA = "none", showData = TRUE) {
  # Initialize columns for lower and upper confidence intervals
  data$Lower <- NA
  data$Upper <- NA
  
  
  # create original temperature column (only used if using moving average)
  data$Temperature_original <- data$Temperature
  
  # Get the base temperature
  if(is.null(baseTemp)){
    base_temp <- data$Temperature[1] 
    min_year <- min(data$Year)
    max_year <- max(data$Year)
  } else {
    min_year <- baseTemp[1]
    if(is.na(baseTemp[2])){
      baseTemp[2] <- baseTemp[1]
    }
    max_year <- baseTemp[2]
    base_temp <- mean(data$Temperature[data$Year >= min_year & data$Year <= max_year])
  }

  data$Temperature <- data$Temperature - base_temp
  data$Temperature_original <- data$Temperature

  
  # Calculate the moving average if requested
  if(showMA == "CMA") {
    halfWindow <- (windowSize - 1) / 2
    data$MA <- stats::filter(data$Temperature, rep(1/windowSize, windowSize), sides = 2)
  } else if(showMA == "OSMA") {
    data$MA <- stats::filter(data$Temperature, rep(1/windowSize, windowSize), sides = 1)
  }
  
  
  # Adjust the data based on the showMA argument
  if(showMA %in% c("CMA", "OSMA")) {
    # Remove NA values that result from moving average calculation
    data <- data[!is.na(data$MA),]
    # Use the moving average as the Temperature data for predictions
    data$Temperature <- data$MA
  }
  
  # Set future years for prediction
  future_years <- data.frame(Year = (max(data$Year) + 1):4000)
  
  # Method selection for prediction
  if (method == "LM") {
    model_fit <- lm(Temperature ~ Year, data = data)
  } else if (method == "GLM") {
    model_fit <- glm(Temperature ~ Year, data = data)
  } else if (method == "GAM") {
    model_fit <- mgcv::gam(Temperature ~ s(Year), data = data)
  } else if (method == "ARIMA") {
    # Convert data to a time series object for ARIMA
    ts_data <- ts(data$Temperature, start = min(data$Year), frequency = 1)
    model_fit <- forecast::auto.arima(ts_data)
  } else {
    stop("Unsupported method")
  }
  
  # Prediction for future years
  if (method == "LM") {
    future_years$Temperature <- predict(model_fit, newdata = future_years, interval = "confidence")[, "fit"]
    future_years$Lower <- predict(model_fit, newdata = future_years, interval = "confidence")[, "lwr"]
    future_years$Upper <- predict(model_fit, newdata = future_years, interval = "confidence")[, "upr"]
    smooth_method <- "lm"
  } else if (method == "GLM"){
    pred <- predict(model_fit, newdata = future_years)
    future_years$Temperature <- pred
    future_years$Lower <- pred -  sd(pred)
    future_years$Upper <- pred +  sd(pred)
    smooth_method <- "glm"
  } else if (method == "GAM") {
    pred <- predict(model_fit, newdata = future_years, se = T)
    future_years$Temperature <- pred$fit
    future_years$Lower <- pred$fit - 1.96 * pred$se
    future_years$Upper <- pred$fit + 1.96 * pred$se
    smooth_method <- "gam"
  } else if (method == "ARIMA"){
    future_forecast <- forecast(model_fit, h = length(future_years$Year))
    future_years$Temperature <- future_forecast$mean
    future_years$Lower <- future_forecast$lower[,2]
    future_years$Upper <- future_forecast$upper[,2]
    smooth_method <- "arima"
  } else {
    stop("Unsupported method")
  }
  
  # Calculate the increase and target year
  #future_years$Increase <- future_years$Temperature - base_temp
  target_year <- min(future_years$Year[future_years$Temperature >= 1.5], na.rm = TRUE)
  future_years <- subset(future_years, Year <= target_year)
  combined_data <- rbind(data[, c("Year", "Temperature", "Lower", "Upper")], 
                         future_years[, c("Year", "Temperature", "Lower", "Upper")])
  
  # Plotting
  if(showData){
    plot <- ggplot() +
      geom_line(data = data, aes(x = Year, y = Temperature_original), color = "black") +
      geom_ribbon(data = combined_data, aes(x = Year, ymin = Lower, ymax = Upper), fill = "red", alpha = 0.2) +
      theme_bw() +
      labs(y = "Temperature (°C)", x = "Year") +
      geom_line(data = future_years, aes(x = Year, y = Temperature), color = "red", linetype = "dashed") +
      geom_vline(xintercept = target_year, linetype = "dashed", color = "darkred") +
      geom_text(aes(x = target_year, y = min(data$Temperature), label = paste(target_year)), vjust =1, hjust = 1.1, color = "darkred")  +
      labs(title = paste("Prediction with", method, "Method"), y = "Temperature (°C)", x = "Year")
  }else{
    plot <- ggplot() +
      #geom_line(data = data, aes(x = Year, y = Temperature_original), color = "black") +
      geom_ribbon(data = combined_data, aes(x = Year, ymin = Lower, ymax = Upper), fill = "red", alpha = 0.2) +
      theme_bw() +
      labs(y = "Temperature (°C)", x = "Year") +
      geom_line(data = future_years, aes(x = Year, y = Temperature), color = "red", linetype = "dashed") +
      geom_vline(xintercept = target_year, linetype = "dashed", color = "darkred") +
      geom_text(aes(x = target_year, y = min(data$Temperature), label = paste(target_year)), vjust =1, hjust = 1.1, color = "darkred")  +
      labs(title = paste("Prediction with", method, "Method"), y = "Temperature (°C)", x = "Year")
  }
 
  
  # ggplot smoothing
  if(method != 'ARIMA'){
    plot <- plot +
      geom_smooth(data = data, aes(x = Year, y = Temperature), method = smooth_method,
                  se = TRUE, color = "darkred", fill = "red", alpha = 0.2, linewidth = 0.7)
  }
  
  if(smooth_method == 'arima'){
    data$fitted <- model_fit$fitted
    
    # place holder interval
    std_error <- sd(resid(model_fit))
    data$fitted_Lower <- data$fitted - std_error
    data$fitted_Upper <- data$fitted + std_error
    
    plot <-  plot +
      geom_line(data = data, aes(x = Year, y = fitted), color = "red") +
      geom_ribbon(data = data, aes(x = Year, ymin = fitted_Lower, ymax = fitted_Upper), fill = "red", alpha = 0.2)
  }
  
  # Display Moving Average if requested
  if(showMA %in% c("CMA", "OSMA")) {
    plot <- plot + geom_line(data = data, aes(x = Year, y = MA), color = "blue", size = 1) +
      labs(title = paste("Prediction with", method, "Method using", showMA), y = "Temperature Difference (°C)", x = "Year")
  }
  
  
  # Highlight the baseline temperature range if specified
  if (!is.null(baseTemp)) {
    plot <- plot +
      geom_rect(aes(xmin = min_year, xmax = max_year, ymin = -Inf, ymax = Inf), fill = "blue", alpha = 0.1)
  }

  
  # Return the plot and the target year
  return(list(plot = plot, target_year = target_year))
}



t4(data = df, method = "ARIMA", windowSize = 5, showMA = "none", baseTemp = c(1900, 1950), showData = F)



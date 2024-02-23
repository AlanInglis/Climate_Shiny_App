
generate_prediction_plot <- function(data, method = "LM", baseTemp = NULL) {
  
  # get the base temperature
  if(is.null(baseTemp)){
    base_temp <- data$Temperature[1]
    min_year <- min(data$Year)
    max_year <- max(data$Year)
  } else {
    min_year <- baseTemp[1]
    max_year <- baseTemp[2]
    base_temp <- mean(data$Temperature[data$Year >= min_year & data$Year <= max_year])
    
    graph_temp_max <- data$Temperature[data$Year == max_year]
    graph_temp_min <- data$Temperature[data$Year == min_year]
  }
  
  # set future years
  future_years <- data.frame(Year = (max(data$Year) + 1):3000)
  
 # set up
  data$Lower <- NA
  data$Upper <- NA
  
  # run models
  if (method == "LM") {
    model_fit <- lm(Temperature ~ Year, data = data)
    future_years$Temperature <- predict(model_fit, newdata = future_years, interval = "confidence")[, "fit"]
    future_years$Lower <- predict(model_fit, newdata = future_years, interval = "confidence")[, "lwr"]
    future_years$Upper <- predict(model_fit, newdata = future_years, interval = "confidence")[, "upr"]
    smooth_method <- "lm"
  } else if (method == "GLM"){
    model_fit <- glm(Temperature ~ Year, data = data)
    pred <- predict(model_fit, newdata = future_years)
    future_years$Temperature <- pred
    future_years$Lower <- pred -  sd(pred)
    future_years$Upper <- pred +  sd(pred)
    smooth_method <- "glm"
  } else if (method == "GAM") {
    model_fit <- mgcv::gam(Temperature ~ s(Year), data = data)
    pred <- predict(model_fit, newdata = future_years, se = T)
    future_years$Temperature <- pred$fit
    future_years$Lower <- pred$fit - 1.96 * pred$se
    future_years$Upper <- pred$fit + 1.96 * pred$se
    smooth_method <- "gam"
  } else {
    stop("Unsupported method")
  }
  
  # set up data frame for plotting 
  future_years$Increase <- future_years$Temperature - base_temp
  target_year <- future_years$Year[which.max(future_years$Increase >= 1.5)]
  
  future_years <- future_years[, c("Year", "Temperature", "Lower", "Upper")]
  future_years <- subset(future_years, Year <= target_year)
  
  combined_data <- rbind(data[, c("Year", "Temperature", "Lower", "Upper")], future_years)
  
  # plot
 plot <-  ggplot() +
    geom_line(data = data, aes(x = Year, y = Temperature), color = "black") +
    geom_smooth(data = data, aes(x = Year, y = Temperature), method = smooth_method,
                se = TRUE, color = "red", fill = "red", alpha = 0.2, linewidth = 0.7) +
    geom_ribbon(data = combined_data, aes(x = Year, ymin = Lower, ymax = Upper), fill = "red", alpha = 0.2) +
    geom_line(data = future_years, aes(x = Year, y = Temperature), color = "red", linetype = "dashed") +
    geom_vline(xintercept = target_year, linetype = "dashed", color = "darkred") +
   geom_rect(aes(xmin = min_year, xmax = max_year, ymin = -Inf, ymax = Inf), fill = "blue", alpha = 0.1) +  # Highlighting range
   # geom_rect(aes(xmin = min_year, xmax = max_year, ymin = graph_temp_min-0.1, ymax = graph_temp_max+0.1), fill = "blue", alpha = 0.1) +  # Highlighting range
    geom_text(aes(x = target_year, y = min(data$Temperature), label = paste(target_year)), vjust =1, hjust = 1.1, color = "darkred") +
    theme_bw() +
    labs(title = paste("Prediction with", method, "Method"), y = "Temperature (°C)", x = "Year")
  
  
  # Return
  myList <- list(plot = plot, target_year = target_year)
  return(myList)
}


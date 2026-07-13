myCaliGraph2 <- function(df,sel_massTrace){
  # Das Data Frame muss die Spalten "x" und "y" enthalten
  
  require("ggplot2")
  n <- nrow(df) # Find length of y to use as sample size
  lm.model <- lm(df[[2]] ~ df[[1]]) # Fit linear model
  
  # Extract fitted coefficients from model object
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]
  
  # Find SSE and MSE
  sse <- sum((df[[2]] - lm.model$fitted.values)^2)
  mse <- sse / (n - 2)
  
  t.val <- qt(0.975, n - 2) # Calculate critical t-value
  
  # Fit linear model with extracted coefficients
  x_new <- 1:max(df[[1]])
  y.fit <- b1 * x_new + b0
  
  # Find the standard error of the regression line
  se <- sqrt(sum((df[[2]] - y.fit)^2) / (n - 2)) * sqrt(1 / n + (df[[1]] - mean(df[[1]]))^2 / sum((df[[1]] - mean(df[[1]]))^2))
  
  # Fit a new linear model that extends past the given data points (for plotting)
  x_new2 <- 1:max(df[[1]] + 100)
  y.fit2 <- b1 * x_new2 + b0
  
  # Warnings of mismatched lengths are suppressed
  slope.upper <- suppressWarnings(y.fit2 + t.val * se)
  slope.lower <- suppressWarnings(y.fit2 - t.val * se)
  
  # Collect the computed confidence bands into a data.frame and name the colums
  bands <- data.frame(cbind(x_new2,slope.lower, slope.upper))
  colnames(bands) <- c('x Values','Lower Confidence Band', 'Upper Confidence Band')
  
  
  
  rSquare <- summary(lm.model)$r.squared
  lm_eq <- paste("y =",as.character(round(b0,2)),
                 "+",
                 as.character(round(b1,2)),"x",
                 "\n R =",as.character(round(rSquare,5)))
  
  
  # Plot the fitted linear regression line and the computed confidence bands
  g <- ggplot(df,aes(df[,1],df[,2])) +  # mit "+" können mehrere asthetics hinzugefügt werden
    
    geom_point() +

    geom_abline(slope = b1,
                intercept = b0) +

    annotate("text",
             x = max(df[,names(df)[1]])/2,
             y = Inf,
             size = 5,
             label = lm_eq,
             vjust = "inward") +

    labs(y = paste0("Intensity (",names(df)[2],")"),
         x = paste0("Concetration (",names(df)[1],")"),
         title = "Calibration",
         subtitle = sel_massTrace) +

    geom_line(bands,aes(bands[,1],bands[,2]),
              color = "blue",
              linetype = "dashed") +

    geom_line(bands,aes(bands[,1],bands[,3]),
              color = "blue",
              linetype = "dashed")

  return(g)

}
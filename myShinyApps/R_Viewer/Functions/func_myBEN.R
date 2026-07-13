myBEN <- function(df,method = "Kaiser"){
source('func_myLinReg.R')
  x <- df$xVal
  y <- df$yVal
  lr <- lm(y~x)
  sumlr <- summary(lr)
  
  b0 <- lr$coefficients[1]
  b1 <- lr$coefficients[2]
  freedom <- "bla"
  
  if (method == "Kaiser") {
    
  }
  
  
}
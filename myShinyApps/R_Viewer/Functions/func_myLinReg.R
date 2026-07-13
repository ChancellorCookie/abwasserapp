myLinReg <- function(df,nAllStds){
 
  i <- nAllStds
  
  df <- df[order(df$xVal),] # Sortieren der Datenpunkte nach aufsteigender Konzentration der Standards
 
   # Wozu war nochmal dieser Schritt???
  # wenn ein Standard mehrfach gemessen wurde?
  #valSTD <- unique(df$xVal) # Ermittlung der einzelnen Konzentrationen
  valSTD <- df$xVal
  
  subdf <- df %>% filter(xVal <= valSTD[i]) # Extraktion der konzentrationspunkte
  
  
  ### Lineare Regression
  
  sum_LinReg <- summary(lm(subdf$yVal ~ subdf$xVal))
  b0 <- sum_LinReg$coefficients[[1]]
  b1 <- sum_LinReg$coefficients[[2]]
  rSquare <- sum_LinReg$r.squared
  out <- list("b0" = b0,"b1" = b1,"rSquare" = rSquare)
  
  return(out)
  
}

myLinReg.Simple <- function(df,xVal,yVal){
  
  if (is.null(df)) {
    if (length(xVal) != length(yVal)) {
      return(NULL)
    }
    df <- data.frame("Index" = c(1:length(xVal)),
                     "xVal" = xVal,
                     "yVal" = yVal,
                     stringsAsFactors = F)
  }
  df <- df[order(df$xVal),] # Sortieren der Datenpunkte nach aufsteigender Konzentration der Standards
  
  ### Lineare Regression
  
  sum_LinReg <- summary(lm(df$yVal ~ df$xVal))
  b0 <- sum_LinReg$coefficients[[1]]
  b1 <- sum_LinReg$coefficients[[2]]
  rSquare <- sum_LinReg$r.squared
  out <- list("b0" = b0,"b1" = b1,"rSquare" = rSquare)
  
  return(out)
  
}
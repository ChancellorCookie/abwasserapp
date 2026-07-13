myLinReg <- function(df,nAllStds){
 
  i <- nAllStds
  
  df <- df[order(df$xVal),] # Sortieren der Datenpunkte nach aufsteigender Konzentration der Standards
  valSTD <- unique(df$xVal) # Ermittlung der einzelnen Konzentrationen

  
  subdf <- df %>% filter(xVal <= valSTD[i]) # Extraktion der konzentrationspunkte
  
  
  ### Lineare Regression
  
  sum_LinReg <- summary(lm(subdf$yVal ~ subdf$xVal))
  b0 <- sum_LinReg$coefficients[[1]]
  b1 <- sum_LinReg$coefficients[[2]]
  rSquare <- sum_LinReg$r.squared
  out <- list("b0" = b0,"b1" = b1,"rSquare" = rSquare)
  
  return(out)
  
}
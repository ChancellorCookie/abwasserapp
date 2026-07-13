myOLS_BG<- function(OLS.List,Parameter = "xBG"){
  # OLS.List ist die resultierende Liste der myOLS() Funktion

  BG <- c()
  
  for (v in names(OLS.List)) {
    subOLS <- OLS.List[[v]]
    BG <- c(BG,subOLS[[Parameter]])
  }
  if (is.null(BG)) {
    BG <- rep(NA,length(OLS.List))
  }
  names(BG) <- names(OLS.List)
  
  BG
  
  
  
}
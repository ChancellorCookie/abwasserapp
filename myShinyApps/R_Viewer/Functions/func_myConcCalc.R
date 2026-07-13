myConcCalc <- function(Intensity,Coeffs){
  
  return(conc <- (Intensity - Coeffs[[1]])/Coeffs[[2]])
  
}
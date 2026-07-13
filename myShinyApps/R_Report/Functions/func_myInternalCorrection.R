myInternalCorrection <- function(myList){
  
  
  RawData <- myList$eQuant$Raw$Raw.Average$Data
  Assignment <- myList$eQuant$IntStd$Assignment
  Factors <- myList$eQuant$IntStd$Average.Factors
  
  ### Corrected Data
  Corr.Average <- myList$eQuant$Raw$Raw.Average$Data # Neue Variable mit gleichem Meta
  Corr.Average[Assignment$Analytes] <- RawData[Assignment$Analytes]/Factors[Assignment$Standards]
  
  
  
  
  RawData <- myList$eQuant$Raw$Raw.Intensity$Data
  Assignment <- myList$eQuant$IntStd$Assignment
  Factors <- myList$eQuant$IntStd$Intensity.Factors
  
  Corr.Intensity <- myList$eQuant$Raw$Raw.Intensity$Data # Neue Variable mit gleichem Meta
  Corr.Intensity[Assignment$Analytes] <- RawData[Assignment$Analytes]/Factors[Assignment$Standards]
  
  list("Corr.Average"= Corr.Average,
       "Corr.Intensity"= Corr.Intensity)

}
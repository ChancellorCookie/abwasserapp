myInternalCorrection <- function(myList){
  # Needed Variables:
  # RawData
  # Assignment
  # Factors
  
  
  # Calculation on averaged Intensities
  RawData     <- myList$Raw.Data$eQuant$Raw$Raw.Average$Data
  Assignment  <- myList$Raw.Data$eQuant$IntStd$Assignment
  Factors     <- myList$Raw.Data$eQuant$IntStd$Average.Factors
  
  ### Corrected Data
  Corr.Average <- myList$Raw.Data$eQuant$Raw$Raw.Average$Data # Kopie um eine Spalte zu ändern
  
  if (as.logical(myList$Input.Parameter$UseIntCorr)) {
    Corr.Average[Assignment$Analytes] <- RawData[Assignment$Analytes]/Factors[Assignment$Standards]
  }
  
  
  # Calculation on single Intensities
  RawData <- myList$Raw.Data$eQuant$Raw$Raw.Intensity$Data
  Assignment <- myList$Raw.Data$eQuant$IntStd$Assignment
  Factors <- myList$Raw.Data$eQuant$IntStd$Intensity.Factors
  
  Corr.Intensity <- myList$Raw.Data$eQuant$Raw$Raw.Intensity$Data # Neue Variable mit gleichem Meta
  
  if (as.logical(myList$Input.Parameter$UseIntCorr)) {
    Corr.Intensity[Assignment$Analytes] <- RawData[Assignment$Analytes]/Factors[Assignment$Standards]
  }
  
  
  # Output
  list("Corr.Average"= Corr.Average,
       "Corr.Intensity"= Corr.Intensity)

}
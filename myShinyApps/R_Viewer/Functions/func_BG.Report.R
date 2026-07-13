BG.Report <- function(BG,BG.default){
  
  # The compared Vectors must be of same length and must have same name
  
  
  #___________________________________________________________________________
  # Validation test
  getLogicals <- names(BG) %in% names(BG.default)
  # is one of the default names existing in BG?
  if (!any(getLogicals)) {
    return(NULL)
  }
  #___________________________________________________________________________
  
  # Loop for single measured Analyte in 
  for (Analyte in names(BG[getLogicals])) {
    # Get calculated BG Value
    BG.Meas <- BG[[Analyte]]
    # Get default BG Value
    BG.SAA <- BG.default[[Analyte]]
    # Proof if calculated values is less of required minimum
    if (BG.Meas < BG.SAA) {
      # if the calculated value is lower than the minimum requirement, the minimum required replace the calculated one
      BG[Analyte] <- BG.SAA
    }
  }
  return(BG)
}
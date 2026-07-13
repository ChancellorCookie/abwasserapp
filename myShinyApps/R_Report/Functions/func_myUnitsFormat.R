myUnitsFormat <- function(UnitsVector){
  UnitsVector <- gsub("Y ","",UnitsVector) # Entfernt das Sonderzeichen aus den Zellen (Y (Âµg/l))
  UnitsVector <- gsub("[()]","",UnitsVector) # Entfernt das Sonderzeichen aus den Zellen (Y (Âµg/l))
  UnitsVector <- gsub("Â","",UnitsVector)
  UnitsVector
}  
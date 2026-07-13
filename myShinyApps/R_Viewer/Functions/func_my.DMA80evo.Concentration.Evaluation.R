my.DMA80evo.Concentration.Evaluation <- function(myList){
  
  ### KALIBRATION
  CaliFile <- myList$Input.Parameter$Calibration$fullpath
  
  cal <- myDMA80.Cali(CaliFile) # Einlesen der Calibrationsdatei
  
  
  OLS <- list("Hg" = myBEN_DIN(x = cal$Hg..ng., # DIN Auswertung
                               y = cal$Height,
                               k = 3,
                               m = 1,
                               alpha = myList$Input.Parameter$Alpha,
                               Method = "Kal",
                               Titel = "Hg (abs)"))
  
  
  # Calibration Statistics
  myList[["Calibration"]] <- list("Daten" = cal,
                                  "Info" = list("Analytes" = "Hg"), # Notwendig für "Dynamic Outputs in loops" in "R_Viewer" app
                                  "OLS" = OLS)
  
  myList[["Concentration"]] <- list("Concentration" = myList$RawData$RawClean,
                                    "BG.Report" =  myOLS_BG(OLS,"xBG"))
  
  return(myList)
}
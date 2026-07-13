myDMA80 <- function(myParams){
  
  myDF_Raw <- suppressWarnings(readCSV_DMA80evo(myParams$inFile))
  
  myDF <- myDF_Raw
  myDF$CreationDate %<>% as.POSIXct(format = "%Y-%m-%d-%H-%M-%S")
  myDF$ProcessTime  %<>% as.POSIXct(format = "%Y-%m-%d-%H-%M-%S")
  
  myDF <- myDF[complete.cases(myDF$Concentration),]
  
  # Sonderfall:
 
  
  if(sum(myDF$Concentration == "Out of range") > 0){
    myDF[myDF$Concentration == "Out of range",c("Concentration","Hg..ng.")] <- NA
    myDF$Hg..ng. %<>% str_replace_all(.,",",".")
    myDF$Concentration %<>% str_replace_all(.,",",".")
    myDF$Hg..ng. %<>% as.numeric()
    myDF$Concentration %<>% as.numeric()
  }
  
  
  
  myParams[["Date"]] <- as.character.Date(myDF$ProcessTime[1],format ="%d %b %Y")
  myParams[["FileName"]] <- basename(myParams$inFile)
  myParams[["FilePath"]] <- gsub(myParams$FileName,"",myParams$inFile)
  
  ### KALIBRATION
  CaliFile <- myParams$PathCalibration
  
  cal <- myDMA80.Cali(CaliFile) # Einlesen der Calibrationsdatei
  OLS <- list()
  OLS[["Hg"]] <- myBEN_DIN(x = cal$Hg..ng., # DIN Auswertung
                           y = cal$Height,
                           k = 3,
                           m = 1,
                           alpha = myParams$Alpha,Method = "Kal",sl = NULL,Titel = "Hg Gesamt (ng)")
  

  BG.vec <- myOLS_BG(OLS,"xBG")
  # Extract BG Values
  if (myParams$BG.SAA) {  ## wenn die Bestimmungsgrenze im Report mit den Akzeptanzkriterien der SAA abgeglichen werden soll
    if (BG.vec < myParams$TargetValue$LimitBG) {
      BG.vec <- myParams$TargetValue$LimitBG
    }
  }
  
  Calibration <- list("Daten" = cal,
                      "OLS" = OLS)
  
  list("Input.Parameter" = myParams,
       "RawData" = myDF_Raw,
       "Quantified" = myDF,
       "Calibration" = Calibration,
       "BG.Report" = BG.vec)
}
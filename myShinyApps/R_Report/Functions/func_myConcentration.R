myConcentration <- function(Corr.Average,
                            StdDef,
                            CalIndex,
                            Analytes){
  
  StdIndex <- StdDef$Index
  
  # Generierung von leeren DF Kopien
  conc <- Corr.Average
  CalcConcNumber <- conc
  conc[,Analytes] <- 0 # wird später als Charakter formatiert
  CalcConcNumber[,Analytes] <- 0
  
  myPlots <- list()
  
  for (v in Analytes) { # Schleife für Anzahl definierter Elemente
    
  
    # DataFrame der Kalibrationsdaten

    df <- data.frame("Index" = StdIndex,
                     "xVal" = StdDef[[v]],
                     "yVal" = Corr.Average[StdIndex,v])
    # Doppelte Eckige Klammern, um "yVal" als Vektor zu extrahieren. Sonst wir der ColName nicht überschrieben
    
    
    # Wenn NA vorhanden sind, sollen diese entfernt werden
    df %<>% filter(complete.cases(df))
    
    df <- df[order(df$xVal),] # Sortieren der Datenpunkte nach aufsteigender Konzentration der Standards
    
    ##########################
    # Bug durch eine Datei, bei der die ersten Messungen der SampleList Fehlmessungen waren und nicht exportiert wurden
    # -> Der die Spalte "Index" der Raw_Intensity  beginnt nicht bei 1
    # -> nAllStds ist die Differenz der Indizes.
    # -> +1 weil der index nicht bei 0 startet.
    
    nAllStds <- max(df$Index)-df$Index[1] + 1 # Der erste Index muss nicht zwingend bei 1 beginnen!
    ##########################
    
    
    
    
    for (j in 1:nrow(CalIndex)) { # Schleife für Anzahl an Proben
      
      nStds <- CalIndex[j,v]
      # Wenn NA innerhalb der Kalibration vorhanden sind, dann kann es zu BUGs in der myLinReg() führen
      if (nStds > nAllStds | is.na(nStds)) {
        nStds <- nAllStds
      }
      
      lr <- myLinReg(df,nStds)
      c <- (Corr.Average[j,v] - lr$b0)/lr$b1
      
      while ((lr$rSquare < .99 | lr$b1 <= 0 ) & nStds < nAllStds ) {
        nStds <- nStds + 1
        lr <- myLinReg(df,nStds)
        c <- (Corr.Average[j,v] - lr$b0)/lr$b1
      }
      conc[j,v] <- c
      CalcConcNumber[j,v] <- nStds
    }
  }
 list("Concentration" = conc,
      "UsedStandards" = CalcConcNumber)
}
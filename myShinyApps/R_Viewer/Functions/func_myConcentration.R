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
                     "yVal" = Corr.Average[StdIndex,v],
                     stringsAsFactors = F)
    # Doppelte Eckige Klammern, um "yVal" als Vektor zu extrahieren. Sonst wir der ColName nicht überschrieben
    
    
    # Wenn NA vorhanden sind, sollen diese entfernt werden
    df %<>% filter(complete.cases(df))
    
    df <- df[order(df$xVal),] # Sortieren der Datenpunkte nach aufsteigender Konzentration der Standards
    
    ##########################
    # Bug durch eine Datei, bei der die ersten Messungen der SampleList Fehlmessungen waren und nicht exportiert wurden
    # -> Der die Spalte "Index" der Raw_Intensity  beginnt nicht bei 1
    # -> nAllStds ist die Differenz der Indizes.
    # -> +1 weil der index nicht bei 0 startet.
    
    #nAllStds <- max(df$Index)-df$Index[1] + 1 # Der erste Index muss nicht zwingend bei 1 beginnen!
    #nAllStds <- nrow(df)
    ##########################
    
    
    
    
    for (j in 1:nrow(CalIndex)) { # Schleife für Anzahl an Proben
      
      # Wenn der Wert NA beträgt
      # Sollte nicht mehr passieren, da NA's in Zeile 28 gelöscht werden
      nStds <- CalIndex[j,v]
      if (is.na(nStds)) {
        conc[j,v] <- NA
        CalcConcNumber[j,v] <- NA
        next
      }
  
      # Bug Prevention
      if (nStds > max(df$Index)) {
        nStds <- max(df$Index)
      }
      
      
      
      lr <- myLinReg(df,which(df$Index == nStds))
      c <- (Corr.Average[j,v] - lr$b0)/lr$b1
      
      while ((lr$rSquare < .99 | lr$b1 <= 0 ) & nStds < max(df$Index) ) {
        nStds <- df$Index[which(df$Index == nStds) + 1]
        lr <- myLinReg(df,which(df$Index == nStds))
        c <- (Corr.Average[j,v] - lr$b0)/lr$b1
      }
      conc[j,v] <- c
      CalcConcNumber[j,v] <- nStds
    }
  }
 list("Concentration" = conc,
      "UsedStandards" = CalcConcNumber)
}
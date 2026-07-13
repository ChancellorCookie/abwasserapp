myCaliGraph.list <- function(Analytes,Units,Corr.Average,StdDef,StdIndex){
  
  myPlots <- list()
  for (v in Analytes) { # Schleife für Anzahl definierter Elemente
    
    
    # DataFrame der Kalibrationsdaten
    
    # DataFrame der Kalibrationsdaten
    df <- data.frame("Index" = StdIndex,
                     "xVal" = StdDef[[v]],
                     "yVal" = Corr.Average %>% filter(Index %in% StdIndex) %>% select(v) %>% pull())
    # Doppelte Eckige Klammern, um "yVal" als Vektor zu extrahieren. Sonst wir der ColName nicht überschrieben
    # Doppelte Eckige Klammern, um "yVal" als Vektor zu extrahieren. Sonst wir der ColName nicht überschrieben
    
    
    # Wenn NA vorhanden sind, sollen diese entfernt werden
    df %<>% filter(complete.cases(df))
    
    df <- df[order(df$xVal),] # Sortieren der Datenpunkte nach aufsteigender Konzentration der Standards
    
    g <- myCaliGraph(df,v,Units) # Beinhaltet auch die Berechnung der Lineraren Regression
    
    myPlots[[v]] <- g
  }
  return(myPlots)
}
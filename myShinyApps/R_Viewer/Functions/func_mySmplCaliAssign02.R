mySmplCaliAssign02 <- function(SampleDF,StdDef,ExtCal.which.Analytes,OutOfRange.Factor = 1.2){
  
  Analytes <- ExtCal.which.Analytes
  StdIndex <- StdDef$Index
  
  
  CalIndex   <- SampleDF
  OutOfRange <- SampleDF
  CalIndex[1:nrow(CalIndex),Analytes] <- 0
  OutOfRange[1:nrow(OutOfRange),Analytes] <- 0
  
  # Abfrage für die Schleife
  for (j in Analytes) { # Schleife für Anzahl an Massen
    
    # Bereinigte Standards, falls ein Standard NA ist
    # Std.clean <- SampleDF[StdIndex,j][!is.na(SampleDF[StdIndex,j])]
    Std.clean <- SampleDF[StdIndex,] %>% select(Index,j) %>% filter(complete.cases(.))
    Std.clean <- Std.clean[order(Std.clean$Index),]
    
    # Wenn weniger als 3 Standards für die Auswertung vorhanden sind, werden alle Indizes auf NA gesetzt 
    # und die gesamte Auswertung dieses Analyten übersprungen
    
    if (nrow(Std.clean) < 3) {
      CalIndex[,j] <- NA
      OutOfRange[,j] <- 1
      next
    }
    
    for (i in 1:nrow(CalIndex)) { # Schleife für Anzahl an Proben
      
      # Pick Sample Value
      smpl <- SampleDF[i,j]
      if(is.na(smpl)){
        CalIndex[i,j] <- NA
        OutOfRange[i,j] <- NA
        next
      }
      
      for (k in nrow(Std.clean):3) { # Schleife für Anzahl an Standards bin Minimum 3!
        
        std2 <- Std.clean[k,j]
        std1 <- Std.clean[k-1,j]
        
        if (k == nrow(Std.clean) & smpl > std2*OutOfRange.Factor) { # wenn größer als 120% des höchsten Standards
          CalIndex[i,j] <- Std.clean[k,"Index"]
          OutOfRange[i,j] <- 1
          break
        } else if(std1 < smpl & smpl <= std2*OutOfRange.Factor) {
          CalIndex[i,j] <- Std.clean[k,"Index"]
          break
        } else if(k == 3){ # wenn kleiner als der dritte standard
          CalIndex[i,j] <- Std.clean[k,"Index"]
          break
        }
      }
    }
  }
  list("CalIndex" = CalIndex,
       "OutOfRange" = OutOfRange)
}
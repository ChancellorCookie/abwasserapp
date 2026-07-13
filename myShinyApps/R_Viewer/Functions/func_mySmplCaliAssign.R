mySmplCaliAssign <- function(SampleDF,StdDef,ExtCal.which.Analytes,OutOfRange.Factor = 1.2){
  
  Analytes <- ExtCal.which.Analytes
  StdIndex <- StdDef$Index
  
  CalIndex   <- SampleDF
  OutOfRange <- SampleDF
  CalIndex[1:nrow(CalIndex),Analytes] <- 0
  OutOfRange[1:nrow(OutOfRange),Analytes] <- 0
  
  # Abfrage für die Schleife
  for (j in Analytes) { # Schleife für Anzahl an Massen
    Std.Idx.Analyte <- SampleDF[StdIndex,c("Index",j)]
    # Bereinigte Standards, falls ein Standard NA ist
    StdIndex <- Std.Idx.Analyte[complete.cases(Std.Idx.Analyte),"Index"]
    Std.clean <- Std.Idx.Analyte[complete.cases(Std.Idx.Analyte),j]
    # Wenn weniger als 3 Standards für die Auswertung vorhanden sind, werden alle Indizes auf NA gesetzt 
    # und die gesamte Auswertung dieses Analyten übersprungen
    
    if (length(Std.clean) < 3) {
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
      
      for (k in length(Std.clean):3) { # Schleife für Anzahl an Standards bin Minimum 3!
        
        std2 <- Std.clean[k]
        std1 <- Std.clean[k-1]
        
        if (k == length(Std.clean) & smpl > std2*OutOfRange.Factor) { # wenn größer als 120% des höchsten Standards
          CalIndex[i,j] <- StdIndex[std2 == SampleDF[StdIndex,j]]
          OutOfRange[i,j] <- 1
          break
        } else if(std1 < smpl & smpl <= std2*OutOfRange.Factor) {
          CalIndex[i,j] <- StdIndex[std2 == SampleDF[StdIndex,j]]
          break
        } else if(k == 3){ # wenn kleiner als der dritte standard
          whichStd <- StdIndex[std2 == SampleDF[StdIndex,j]]
          if(length(whichStd) != 1){
            CalIndex[i,j] <- NA
          }else{
            CalIndex[i,j] <- StdIndex[std2 == SampleDF[StdIndex,j]]
          }
          
          break
        }
      }
    }
  }
  list("CalIndex" = CalIndex,
       "OutOfRange" = OutOfRange)
}
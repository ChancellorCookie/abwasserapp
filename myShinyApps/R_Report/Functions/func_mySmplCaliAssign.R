mySmplCaliAssign <- function(SampleDF,StdIndex,ExtCal.which.Analytes,OutOfRange.Factor = 1.2){
  
  CalIndex   <- SampleDF
  OutOfRange <- SampleDF
  
  Analytes <- ExtCal.which.Analytes
  
  
  CalIndex[1:nrow(CalIndex),Analytes] <- 0
  OutOfRange[1:nrow(OutOfRange),Analytes] <- 0
  # Abfrage für die Schleife
  for (i in 1:nrow(CalIndex)) { # Schleife für Anzahl an Proben
    for (j in Analytes) { # Schleife für Anzahl an Massen
      for (k in length(StdIndex):3) { # Schleife für Anzahl an Standards bin Minimum 3!
        
        smpl <- SampleDF[i,j]
        if(is.na(smpl)){
          CalIndex[i,j] <- NA
          OutOfRange[i,j] <- NA
          break}
        std2 <- SampleDF[StdIndex[k],j] 
        std1 <- SampleDF[StdIndex[k-1],j]
        
        test_low    <- smpl < std2   
        test_high   <- smpl > std1 
        test_higher <- smpl > std2 
        
        if (k == length(StdIndex) & smpl > std2*OutOfRange.Factor) { # wenn größer als 120% des höchsten Standards
          CalIndex[i,j] <- k
          OutOfRange[i,j] <- 1
          break
        } else if(std1 < smpl & smpl <= std2) {
          CalIndex[i,j] <- k
          break
        } else if(k == 3){ # wenn kleiner als der dritte standard
          CalIndex[i,j] <- k
          break
        }
      }
    }
  }
  list("CalIndex" = CalIndex,
       "OutOfRange" = OutOfRange)
}
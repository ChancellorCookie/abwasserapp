myIntStdAssignment <- function(ExtCal.which.Analytes,IntStd.which.Standards){
  
  
  # Die Namen der eineindeutigen Massespuren werden als Zeilenname eingefügt
  # Das erleichtert später den Aufruf der entsprächenden Zeile in der Schleife
  Analytes <- ExtCal.which.Analytes   %>% `row.names<-`(ExtCal.which.Analytes$Analytes) 
  # Entfernt Analyten, die die gleiche Element Bezeichnung, Masse und Methode aufweisen
  Analytes <- Analytes[!duplicated(Analytes %>% select(Elements,Masses,Methods)),]
  
  Standards <- IntStd.which.Standards %>% `row.names<-`(IntStd.which.Standards$Analytes)
  # Entfernt doppelte Standards, die die gleiche Element Bezeichnung, Masse und Methode aufweisen
  # Führt ansonsten zu Fehlern in der weiteren Verrechnung
  Standards <- Standards[!duplicated(Standards %>% select(Elements,Masses,Methods)),]
  
  
  # Konvertierung der Massen in Nummern
  Analytes$Masses %<>% as.numeric()
  Standards$Masses %<>% as.numeric()
  
  # Prüfung auf Konsistenz der Daten
  diff.meth.anal <- length(unique(Analytes$Methods))
  diff.meth.stds <- length(unique(Standards$Methods))
  if(abs(diff.meth.anal-diff.meth.stds) != 0){
    warning("Die Methoden Anzahl unterscheidet sich zwischen IntStd und Analytes!",
            immediate. = T,
            call. = T)
    return(NA)
  }
  
  # Ergebniss soll eine logische Matrix der Dimension (nStd x nAnalyten)
  
  # Initialisierung der ersten Spalte der Zuordnungsmatrix

  
  
  if(nrow(Standards) >= 1){
    
    for (i in 1:nrow(Analytes)) { # jede Analytmasse einzeln abfragen
      
      # Definition der Methode, der der Standard entsprechen muss
      current_Method <- Analytes[i,"Methods"] 
      
      # Nur Massen Spalte der Standards, deren Methode mit der des Analyten übereinstimmt
      mSTD <- Standards %>% filter(Methods %in% current_Method) %>% select(Masses) %>% pull()
      
      # Kriteruim für Zuordnung ist die kleinste Differenz zu den Standardmassen
      # Suche nach minimalstem Massenabstand
      j <- which.min(abs(mSTD - Analytes[i,"Masses"]))
      
      toAssign <- Standards %>% filter(Methods %in% current_Method) %>% filter(Masses %in% mSTD[j]) %>% select(Analytes) %>% pull()
      
      if(i == 1){
        assigned <- c(toAssign)
      }else{
        assigned <- c(assigned,toAssign)
        
         
      }
    }
    return(data.frame(Analytes$Analytes,assigned,stringsAsFactors = F) %>% `names<-`(c("Analytes","Standards")))
  }
  
 
}

myIntStdAssignment_OES <- function(ExtCal.which.Analytes,IntStd.which.Standards){
  
  
  # Die Namen der eineindeutigen Massespuren werden als Zeilenname eingefügt
  # Das erleichtert später den Aufruf der entsprächenden Zeile in der Schleife
  Analytes <- ExtCal.which.Analytes   %>% `row.names<-`(ExtCal.which.Analytes$Analytes) 
  # Entfernt Analyten, die die gleiche Element Bezeichnung, Masse und Methode aufweisen
  Analytes <- Analytes[!duplicated(Analytes %>% select(Elements,Wavelength,Order,Methods)),]
  
  Standards <- IntStd.which.Standards %>% `row.names<-`(IntStd.which.Standards$Analytes)
  # Entfernt doppelte Standards, die die gleiche Element Bezeichnung, Masse und Methode aufweisen
  # Führt ansonsten zu Fehlern in der weiteren Verrechnung
  Standards <- Standards[!duplicated(Standards %>% select(Elements,Wavelength,Order,Methods)),]
  
  
  ## Konvertierung der Massen in Nummern
  # Analytes$Masses %<>% as.numeric()
  # Standards$Masses %<>% as.numeric()
  
  # Prüfung auf Konsistenz der Daten
  diff.meth.anal <- length(unique(Analytes$Methods))
  diff.meth.stds <- length(unique(Standards$Methods))
  if(abs(diff.meth.anal-diff.meth.stds) != 0){
    warning("Die Methoden Anzahl unterscheidet sich zwischen IntStd und Analytes!",
            immediate. = T,
            call. = T)
    return(NA)
  }
  
  # Ergebniss soll eine logische Matrix der Dimension (nStd x nAnalyten)
  
  # Initialisierung der ersten Spalte der Zuordnungsmatrix
  
  
  
  if(nrow(Standards) >= 1){
    
    if (nrow(Standards) == 1) {
      
      assigned <- rep(Standards[["Analytes"]],nrow(Analytes))
      
      } else if (nrow(Standards) > 1){
      for (i in 1:nrow(Analytes)) { # jede Analytmasse einzeln abfragen
        
        # Definition der Methode, der der Standard entsprechen muss
        current_Method <- Analytes[i,"Methods"] 
        
        # Kriterium zur Auswahl des Internen Standards ist die gemessene Methode und die Ordnung
        # Methode Axial oder Radial
        # Ordnung < 300 oder > 300
        
        oSTD <- Standards %>% filter(Methods %in% current_Method) %>% select(Order) %>% pull()
        
        if (length(oSTD) > 0) {
          j <- which.min(abs(as.numeric(oSTD) - as.numeric(Analytes[i,"Order"])))
          toAssign <- Standards %>% filter(Methods %in% current_Method) %>% filter(Order %in% oSTD[j]) %>% select(Analytes) %>% pull()
        } else {
          toAssign <- Standards[["Analytes"]][1]
        }
        
        if(i == 1){
          assigned <- c(toAssign)
        }else{
          assigned <- c(assigned,toAssign)
          
          
        }
      }
    }
    
    return(data.frame(Analytes$Analytes,assigned,stringsAsFactors = F) %>% `names<-`(c("Analytes","Standards")))
  }
  
  
}

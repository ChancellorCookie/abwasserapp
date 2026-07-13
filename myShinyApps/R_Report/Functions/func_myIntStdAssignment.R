myIntStdAssignment <- function(ExtCal.which.Analytes,IntStd.which.Standards){
  
  
  # Die Namen der eineindeutigen Massespuren werden als Zeilenname eingefügt
  # Das erleichtert später den Aufruf der entsprächenden Zeile in der Schleife
  Analytes <- ExtCal.which.Analytes   %>% `row.names<-`(ExtCal.which.Analytes$Analytes) 
  Standards <- IntStd.which.Standards %>% `row.names<-`(IntStd.which.Standards$Analytes) 
  
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
    stop()
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
      
      if(i == 1){
        assigned <- c(Standards$Analyte[j])
      }else{
        assigned <- c(assigned,Standards$Analyte[j])
      }
    }
    data.frame(Analytes$Analytes,assigned,stringsAsFactors = F) %>% `names<-`(c("Analytes","Standards"))
  }
  
}

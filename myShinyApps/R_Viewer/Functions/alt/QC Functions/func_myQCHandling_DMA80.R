myQCHandling_DMA80 <- function(QC_DF,
                               Handling = "VBW",
                               PathToWrite,
                               KillOld = FALSE){
  
  
  
  if (Handling == "VBW") { 
    ##############################################################################################
    # Handling für Verfahrensblindwerte
    ##############################################################################################
    QC <- QC_DF %>% select(SampleNames.Simple, 
                           StartTime, 
                           CalFile,
                           MethodFile,
                           Height,
                           Hg..ng.,
                           CalFactor)
    
    # Auslesen ggf. vorhandener Daten
    ####################################################
    if(file.exists(PathToWrite) & KillOld == F){
      QCold <- read.csv2(PathToWrite,sep = ",",stringsAsFactors = F) %>% `names<-`(names(QC))
      QCold$StartTime %<>% strptime(format ="%Y-%m-%d %H:%M:%S")%<>% as.POSIXct()
      QCold$Height %<>% as.numeric()
      QCold$Hg..ng. %<>% as.numeric()
    }else{ # falls Daten nicht vorhanden, anlegen einer neuen Datei
      QCold <- QC
      write.csv(QC, file = PathToWrite,row.names=FALSE)
    }
    
    # Schreiben neuer Daten
    ####################################################
    if(nrow(QCold %>% filter(StartTime %in% QC$StartTime)) == 0){ # Prüfung auf Dublikat
      QCtowrite <- rbind(QCold,QC) 
      QCtowrite <- QCtowrite[order(QCtowrite$StartTime,decreasing = T),] # Sortierung nach Messzeitpunkt
      write.csv(QCtowrite, file = PathToWrite,row.names=FALSE)
    }else{QCtowrite <- QCold}
    


    
  } else if(Handling == "QC") {
    ##############################################################################################
    # Handling für QC-Proben
    ##############################################################################################
    
    QC <- QC_DF %>% select(SampleNames.Simple, 
                           StartTime, 
                           CalFile,
                           MethodFile,
                           Height,
                           Hg..ng.,
                           Concentration,
                           CalFactor,
                           WFR) 
   
    
    # Auslesen ggf. vorhandener Daten
    ####################################################
    if(file.exists(PathToWrite) & KillOld == F){
      QCold <- read.csv2(PathToWrite,sep = ",",stringsAsFactors = F) %>% `names<-`(names(QC))
      QCold$StartTime %<>% strptime(format ="%Y-%m-%d %H:%M:%S")%<>% as.POSIXct()
      QCold$WFR %<>% as.numeric()
      QCold$Concentration %<>% as.numeric()
    }else{QCold <- QC
    write.csv(QC, file = PathToWrite,row.names=FALSE)}
    
    # Schreiben neuer Daten
    ####################################################
    if( nrow(QCold %>% filter(StartTime %in% QC$StartTime)) == 0){
      QCtowrite <- rbind(QCold,QC)
      QCtowrite <- QCtowrite[order(QCtowrite$StartTime,decreasing = T),]
      write.csv(QCtowrite, file = PathToWrite,row.names=FALSE)
    }else{QCtowrite <- QCold}
  }
  
  QCtowrite
   
}
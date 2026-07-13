myQCHandling_iCapQ_eQuant <- function(myList = myList,
                         QCFilter = "^M.+?QC VBW",
                         ColumnFilter = "195",
                         Handling = "VBW",
                         FileToWrite = paste0(normalizePath(getwd()),"\\QC\\Regelkarten\\eQuant_RegelKartenTest_VBW.csv"),
                         TargetValue = NA,
                         KillOld = FALSE){
  
   
  if (Handling == "VBW") { 
    ##############################################################################################
    # Handling für Verfahrensblindwerte
    ##############################################################################################
    
    
    # Extraktion der VBW Daten aus den Gesamten Messdaten
    ####################################################
    
    QC <- myList$Header %>% select(Index,Labels,StartTime) %>% 
      merge(.,myList$Raw.Data$eQuant$Raw$Raw.Average$Data %>% select(Index,Labels,contains(ColumnFilter)),by = c("Index","Labels")) %>%
      merge(.,myList$Concentration$Concentration %>% select(Index,Labels,contains(ColumnFilter)),by = c("Index","Labels")) %>%
      filter(str_detect(Labels,QCFilter)) %>%
      select(-Index)
    
    names(QC) <- c("Labels","StartTime","Intensity","Concentration")
  
    # Auslesen ggf. vorhandener Daten
    ####################################################
    if(file.exists(FileToWrite) & KillOld == F){
      QCold <- read.csv2(FileToWrite,sep = ",",stringsAsFactors = F) %>% `names<-`(names(QC))
      QCold$StartTime %<>% strptime(format ="%Y-%m-%d %H:%M:%S")%<>% as.POSIXct()
      QCold$Intensity %<>% as.numeric()
      QCold$Concentration %<>% as.numeric()
    }else{ # falls Daten nicht vorhanden, anlegen einer neuen Datei
      QCold <- QC
      write.csv(QC, file = FileToWrite,row.names=FALSE)
    }
    
    
  } else if(Handling == "QC") {
    ##############################################################################################
    # Handling für QC-Proben
    ##############################################################################################
    
    
    # Extraktion der QC Daten aus den Gesamten Messdaten
    ####################################################
    
    Conc.QC <- myList$Concentration$Conc.multiply.VF %>% 
      filter(str_detect(Labels,QCFilter)) %>% 
      select(Labels,contains(ColumnFilter)) %>% 
      `names<-`(c("Labels","Values"))
    
    if (nrow(Conc.QC) > 0) {
      WFR.QC <- Conc.QC
      
      
      if (is.na(TargetValue)) { # ist ein Sollwert vorhanden?
        WFR.QC$Values <- NA
      } else {
        WFR.QC$Values <- Conc.QC$Values/as.numeric(TargetValue)*100
      }
      
      QC <- cbind(myList$Header %>% filter(str_detect(Labels,QCFilter)) %>% select(Labels,StartTime),
                  Conc.QC$Values,
                  WFR.QC$Values,
                  TargetValue)
    
      names(QC) <- c("Labels","StartTime","Concentration","WFR","Defined")
      # nur der Standard, der näher am Zielwert liegt, wird berücksichtigt
      #QC <- QC[which.min(abs(QC$Concentration-as.numeric(TargetValue))),]
      
    } else { # Falls keine passenden Daten in der Sequenz vorhanden sind
      QC <- data.frame("Labels" = as.character(),
                       "StartTime" = as.character(),
                       "Concentration" = as.numeric(),
                       "WFR" = as.numeric(),
                       "Defined" = as.numeric())
    }
    
    
    # Auslesen ggf. vorhandener Daten
    ####################################################
    if(file.exists(FileToWrite) & KillOld == F){
      QCold <- read.csv2(FileToWrite,sep = ",",stringsAsFactors = F) %>% `names<-`(names(QC))
      QCold$StartTime %<>% strptime(format ="%Y-%m-%d %H:%M:%S")%<>% as.POSIXct()
      QCold$WFR %<>% as.numeric()
      QCold$Concentration %<>% as.numeric()
    }else{QCold <- QC
    write.csv(QC, file = FileToWrite,row.names=FALSE)}
    
  }
  
  # Schreiben neuer Daten
  ####################################################
  if(nrow(QCold %>% filter(StartTime %in% QC$StartTime)) == 0){ # Prüfung auf Dublikat
    QCtowrite <- rbind(QCold,QC) 
    QCtowrite <- QCtowrite[order(QCtowrite$StartTime,decreasing = T),] # Sortierung nach Messzeitpunkt
    write.csv(QCtowrite, file = FileToWrite,row.names=FALSE)
  }else{QCtowrite <- QCold}
  
  QCtowrite
   
}
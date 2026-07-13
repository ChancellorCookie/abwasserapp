myQCHandling <- function(myList = myList,
                         Intensity = Int_Mean,
                         Concentration = conc,
                         QCFilter = "^M.+?QC VBW",
                         ColumnFilter = "195",
                         Handling = "VBW",
                         PathToWrite = "\\\\10.1.8.210\\gc-ms-fid\\Analysen\\2018\\Berichte\\iCAPQ Messdaten\\Pt Gesamt\\QC\\Pt-Gesamt_QCVBW.csv",
                         TargetValue = NA,
                         KillOld = FALSE){
  
  #### NUR FÜR TESTZWECKE ###
  ###########################
  # myList <- myList
  # Intensity <- Corr_Intensity
  # Concentration <- conc_VF
  # QCFilter <- "^M.+?QC MM3"
  # ColumnFilter <- "Cis"
  # Handling <- "QC"
  # PathToWrite <- "\\\\10.1.8.210\\gc-ms-fid\\Analysen\\2018\\Berichte\\iCAPQ Messdaten\\Pt Spezies\\QC\\Pt-Spezies_Cis_QCMM3.csv"
  # TargetValue <- 5
  # KillOld <- FALSE
  ##########################
  
  
  dfMeasTimes <- myList$Data$StartTime
  dfMeasTimes$StartTime <- as.POSIXct(dfMeasTimes$StartTime)
   
  if (Handling == "VBW") { 
    ##############################################################################################
    # Handling für Verfahrensblindwerte
    ##############################################################################################
    
    
    # Extraktion der VBW Daten aus den Gesamten Messdaten
    ####################################################
    
    QC <- cbind(dfMeasTimes %>% filter(str_detect(Labels,QCFilter)) %>% select(Labels,StartTime),
                   Intensity %>% filter(str_detect(Labels,QCFilter)) %>% select(contains(ColumnFilter)),
                    Concentration %>% filter(str_detect(Labels,QCFilter)) %>% select(contains(ColumnFilter))
    )
    names(QC) <- c("Labels","StartTime","Intensity","Concentration")
  
    # Auslesen ggf. vorhandener Daten
    ####################################################
    if(file.exists(PathToWrite) & KillOld == F){
      QCold <- read.csv2(PathToWrite,sep = ",",stringsAsFactors = F) %>% `names<-`(names(QC))
      QCold$StartTime %<>% strptime(format ="%Y-%m-%d %H:%M:%S")%<>% as.POSIXct()
      QCold$Intensity %<>% as.numeric()
      QCold$Concentration %<>% as.numeric()
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
    
    
    # Extraktion der VBW Daten aus den Gesamten Messdaten
    ####################################################
    
    Conc.QC <- Concentration %>% filter(str_detect(Labels,QCFilter)) %>% select(Labels,contains(ColumnFilter)) %>% `names<-`(c("Labels","Values"))
    if (nrow(Conc.QC) > 0) {
      WFR.QC <- Conc.QC
      
      
      if (is.na(TargetValue)) { # ist ein Sollwert vorhanden?
        WFR.QC$Values <- NA
      } else {
        WFR.QC$Values <- Conc.QC$Values/as.numeric(TargetValue)*100
      }
      
      QC <- cbind(dfMeasTimes %>% filter(str_detect(Labels,QCFilter)) %>% select(Labels,StartTime),
                  Conc.QC$Values,
                  WFR.QC$Values,
                  TargetValue)
    
      names(QC) <- c("Labels","StartTime","Concentration","WFR","Defined")
      # nur der Standard, der näher am Zielwert liegt, wird berücksichtigt
      QC <- QC[which.min(abs(QC$Concentration-as.numeric(TargetValue))),]
      
    } else { # Falls keine passenden Daten in der Sequenz vorhanden sind
      QC <- data.frame("Labels" = as.character(),
                       "StartTime" = as.character(),
                       "Concentration" = as.numeric(),
                       "WFR" = as.numeric(),
                       "Defined" = as.numeric())
    }
    
    
    # Auslesen ggf. vorhandener Daten
    ####################################################
    if(file.exists(PathToWrite) & KillOld == F){
      QCold <- read.csv2(PathToWrite,sep = ",",stringsAsFactors = F) %>% `names<-`(names(QC))
      QCold$StartTime %<>% strptime(format ="%Y-%m-%d %H:%M:%S")%<>% as.POSIXct()
      QCold$WFR %<>% as.numeric()
      QCold$Concentration %<>% as.numeric()
    }else{QCold <- QC
    write.csv(QC, file = PathToWrite,row.names=FALSE)}
    
    if( nrow(QCold %>% filter(StartTime %in% QC$StartTime)) == 0){
      QCtowrite <- rbind(QCold,QC)
      QCtowrite <- QCtowrite[order(QCtowrite$StartTime,decreasing = T),]
      write.csv(QCtowrite, file = PathToWrite,row.names=FALSE)
    }else{QCtowrite <- QCold}
  }
  
  QCtowrite
   
}
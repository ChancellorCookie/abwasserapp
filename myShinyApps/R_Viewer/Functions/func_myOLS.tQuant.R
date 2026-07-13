myOLS.tQuant <- function(myList){
  
  Analytes <- myList$Chromatography$PeakArea$Analytes
  
  Corr.Average <- myList$Chromatography$PeakArea$Data
  Std.Concentration <- myList$Calibration$Std.Concentration
  Cal.Index <- Std.Concentration$Index
  
  # Exception for changed SN Calculation by additional exported value BaselineHeight
  if (myList$Input.Parameter$BGMethod == "SN" && is.null(myList$Chromatography$BaselineHeight)) {
    myList$Input.Parameter$BGMethod <- "SN_2020_09"
  }
  
  BGMethod <- myList$Input.Parameter$BGMethod
   
  # Tabelle für NWGs und BGs
  OLS.List <-list()
  
  for (v in Analytes) { # Schleife für Anzahl definierter Elemente
    
    if(BGMethod == "DIN32645"){
      ###########################
      ### Kalibriergraden-Methode
      ###########################
      
      if(is.null(myList$Header$Main.Runs)){
        m <- 1
      }else{
        m <- min(as.numeric(myList$Header %>% filter(Index %in% Cal.Index) %>% select(Main.Runs) %>% pull()))
      }
      OLS <- myBEN_DIN(x = myList$Calibration$Std.Concentration[[v]],
                       y = myList$Calibration$Std.PeakArea[[v]],
                       k = 3,
                       alpha = myList$Input.Parameter$Alpha,
                       m = m,
                       Method = "Kal",
                       sl = NULL,
                       Titel = v) # Ordinary Least Square (OLS) Methode nach DIN
      
    }  else if(BGMethod == "SN_2020_06"){
      ################################
      ### Signal/Rausch - Methode
      ################################
      
      # Index des ersten Standards bei dem ein Peak detektiert wurde
      for (i in 1:nrow(myList$Calibration$Std.Concentration)) {
        if(!is.na(myList$Chromatography$PeakArea$Data[i,v])){
          Idx.1Std <- i
          break
        }
      }
      
      # Ordinary Least Squares
      OLS <- myBEN_SN_2020_06(x = myList$Calibration$Std.Concentration[[v]],
                              y = myList$Calibration$Std.PeakArea[[v]],
                              m = 1,
                              alpha = myList$Input.Parameter$Alpha,
                              Titel = v,
                              
                              Chromatogram.Blk = myList$Chromatography$Chromatogramm$SortedChromatogramm %>%     #Extraktion des Blanks und gewählter Massespur
                                filter(Index %in% myList$Chromatography$Chromatogramm$Data$Index[1])%>%
                                select(Index,Labels,contains(myList$Input.Parameter$Masse)) %>% 
                                `names<-`(.,c("Index","Labels","Counts","Time")),
                              
                              Chromatogram.1Std = myList$Chromatography$Chromatogramm$SortedChromatogramm %>%    #Extraktion des kleinsten Standards und gewählter Massespur
                                filter(Index %in% myList$Chromatography$Chromatogramm$Data$Index[Idx.1Std])%>%
                                select(Index,Labels,contains(myList$Input.Parameter$Masse)) %>% 
                                `names<-`(.,c("Index","Labels","Counts","Time")),
                              
                              PeakStart.1Std = unlist(myList$Chromatography$PeakStart$Data[Idx.1Std,v]),
                              
                              PeakEnd.1Std = unlist(myList$Chromatography$PeakEnd$Data[Idx.1Std,v]),
                              
                              Concentration.1Std = myList$Calibration$Std.Concentration[Idx.1Std,v])
    }  else if(BGMethod == "SN_2020_09"){
      ################################
      ### Signal/Rausch - Methode
      ################################
      
      # Index des ersten Standards bei dem ein Peak detektiert wurde
      for (i in 1:nrow(myList$Calibration$Std.Concentration)) {
        if(!is.na(myList$Chromatography$PeakArea$Data[i,v])){
          Idx.1Std <- i
          break
        }
      }
      
      # Ordinary Least Squares
      OLS <- myBEN_SN_2020_09(x = myList$Calibration$Std.Concentration[[v]],
                              y = myList$Calibration$Std.PeakArea[[v]],
                              m = 1,
                              alpha = myList$Input.Parameter$Alpha,
                              Titel = v,
                              
                              Chromatogram.1Std = myList$Chromatography$Chromatogramm$SortedChromatogramm %>%    #Extraktion des kleinsten Standards und gewählter Massespur
                                filter(Index %in% myList$Chromatography$Chromatogramm$Data$Index[Idx.1Std])%>%
                                select(Index,Labels,contains(myList$Input.Parameter$Masse)) %>% 
                                `names<-`(.,c("Index","Labels","Counts","Time")),
                              PeakStart.1Std = unlist(myList$Chromatography$PeakStart$Data[Idx.1Std,v]),
                              PeakEnd.1Std = unlist(myList$Chromatography$PeakEnd$Data[Idx.1Std,v]),
                              Concentration.1Std = myList$Calibration$Std.Concentration[Idx.1Std,v],
                              Retention.1Std = unlist(myList$Chromatography$Retention$Data[Idx.1Std,v]))
      
    } else if(BGMethod == "SN"){
      ################################
      ### Signal/Rausch - Methode
      ################################
      
      # Index des ersten Standards bei dem ein Peak detektiert wurde
      for (i in 1:nrow(myList$Calibration$Std.Concentration)) {
        if(!is.na(myList$Chromatography$PeakArea$Data[i,v])){
          Idx.1Std <- i
          break
        }
      }
      
      OLS <- myBEN_SN(x = myList$Calibration$Std.Concentration[[v]],
                      y = myList$Calibration$Std.PeakArea[[v]],
                      m = 1,
                      alpha = myList$Input.Parameter$Alpha,
                      Titel = v,
                      Concentration.1Std = myList$Calibration$Std.Concentration[Idx.1Std,v],
                      PeakHeight.1Std = unlist(myList$Chromatography$PeakHeight$Data[Idx.1Std,v]),
                      Retention.1Std = unlist(myList$Chromatography$Retention$Data[Idx.1Std,v]),
                      BaselineHeight.1Std = unlist(myList$Chromatography$BaselineHeight$Data[Idx.1Std,v]))
      
    } else {
      OLS <- NULL
    }
      
    OLS.List[[v]]<- OLS
  }
  
  OLS.List
}
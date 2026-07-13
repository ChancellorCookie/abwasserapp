myOLS <- function(myList){
  
  Analytes <- myList$tQuant$Chromatography$PeakArea$Analytes
  
  Corr.Average <- myList$tQuant$Chromatography$PeakArea$Data
  Std.Concentration <- myList$tQuant$Calibration$Std.Concentration
  Cal.Index <- Std.Concentration$Index
  
  BGMethod <- myList$Input.Parameter$BGMethod
  SD <- myList$eQuant$Raw$Raw.STD$Data
  
  # Tabelle für NWGs und BGs
  OLS.List <-list()
  
  
  for (v in Analytes) { # Schleife für Anzahl definierter Elemente
    
    # DataFrame der Kalibrationsdaten
    
    df <- data.frame("Index" = Cal.Index,
                     "xVal" = Std.Concentration[[v]],
                     "yVal" = Corr.Average[Cal.Index,v])
    # Doppelte Eckige Klammern, um "yVal" als Vektor zu extrahieren. Sonst wir der ColName nicht überschrieben
    
    # Wenn NA vorhanden sind, sollen diese entfernt werden
    df %<>% filter(complete.cases(df))
    
    # Suche nach Blanks für Kaiser
    which.Blank.Idx <- df %>% filter(xVal %in% 0) %>% select(Index) %>% pull()
    singleSD <- 0
    
    if(!length(which.Blank.Idx) == 0){
      singleSD <- SD[which.Blank.Idx,v]}
    
    df <- df[order(df$xVal),] # Sortieren der Datenpunkte nach aufsteigender Konzentration der Standards
    
    ## Berechnung der BEN Kenngrößen
    vecX <- df$xVal#[1:3]
    vecY <- df$yVal#[1:3]
    #########################
    
    
    if(BGMethod == "DIN32645"){
      ###########################
      ### Kalibriergraden-Methode
      ###########################
      
      if(is.null(myList$Header$Main.Runs)){
        m <- 1
      }else{
        m <- min(as.numeric(myList$Header %>% filter(Index %in% Cal.Index) %>% select(Main.Runs) %>% pull()))
      }
      OLS <- myBEN_DIN(vecX,vecY,k = 3,alpha = myList$Input.Parameter$Alpha,m=m,Method = "Kal",sl = NULL,Titel = v) # Ordinary Least Square (OLS) Methode nach DIN
      
    } else if(BGMethod == "Leer"){
      ###########################
      ### Leerwert-Methode
      ###########################
      
      if(is.null(myList$Header$Main.Runs)){
        m <- 1
      }else{
        m <- as.numeric(myList$Header %>% filter(Index %in% which.Blank.Idx) %>% select(Main.Runs) %>% pull())
      }
      if (m < 3) {m <-3}
      OLS <- myBEN_DIN(vecX,vecY,k = 3,alpha = myList$Input.Parameter$Alpha,m=m,Method = "Leer",singleSD,Titel = v) # Ordinary Least Square (OLS) Methode nach DIN
      
    } else if(BGMethod == "Kaiser"){
      ################################
      ### Schnellschätzung nach Kaiser
      ################################
      
      if(is.null(myList$Header$Main.Runs)){
        m <- 1
      }else{
        m <- as.numeric(myList$Header %>% filter(Index %in% which.Blank.Idx) %>% select(Main.Runs) %>% pull())
      }
      if (m < 3) {m <-3}
      OLS <- myBEN_DIN(vecX,vecY,k = 3,alpha = myList$Input.Parameter$Alpha,m=m,Method = "Kaiser",singleSD,Titel = v) # Ordinary Least Square (OLS) Methode nach DIN
      
    } else if(BGMethod == "SN"){
      ################################
      ### Signal/Rausch - Methode
      ################################
      
      OLS <- myBEN_SN(x = myList$tQuant$Calibration$Std.Concentration[[v]],
                      y = myList$tQuant$Calibration$Std.PeakArea[[v]],
                      m = 1,
                      alpha = myList$Input.Parameter$Alpha,
                      Titel = v,
                      
                      Chromatogram.Blk = myList$tQuant$Chromatography$Chromatogramm$SortedChromatogramm %>%     #Extraktion des Blanks und gewählter Massespur
                        filter(Index %in% myList$tQuant$Chromatography$Chromatogramm$Data$Index[1])%>%
                        select(Index,Labels,contains(myList$Input.Parameter$Masse)) %>% 
                        `names<-`(.,c("Index","Labels","Counts","Time")),
                      
                      Chromatogram.1Std = myList$tQuant$Chromatography$Chromatogramm$SortedChromatogramm %>%    #Extraktion des kleinsten Standards und gewählter Massespur
                        filter(Index %in% myList$tQuant$Chromatography$Chromatogramm$Data$Index[2])%>%
                        select(Index,Labels,contains(myList$Input.Parameter$Masse)) %>% 
                        `names<-`(.,c("Index","Labels","Counts","Time")),
                      
                      PeakStart.1Std = unlist(myList$tQuant$Chromatography$PeakStart$Data[2,v]),
                      
                      PeakEnd.1Std = unlist(myList$tQuant$Chromatography$PeakEnd$Data[2,v]),
                      
                      Concentration.1Std = myList$tQuant$Calibration$Std.Concentration[2,v])
    }
    OLS.List[[v]]<- OLS
  }
  
  OLS.List
}
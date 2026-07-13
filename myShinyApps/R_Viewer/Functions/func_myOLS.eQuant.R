myOLS.eQuant <- function(Analytes = myList$Raw.Data$eQuant$ExtCal$Which.Analytes$Analytes,
                         Corr.Average = myList$Raw.Data$eQuant$Corrected.Data$Corr.Average[myList$Raw.Data$eQuant$Std.Concentration$Index,],
                         Std.Concentration = myList$Raw.Data$eQuant$Std.Concentration,
                         BGMethod = myList$Input.Parameter$BGMethod,
                         alpha = myList$Input.Parameter$Alpha,
                         SD = myList$Raw.Data$eQuant$Raw$Raw.STD$Data,
                         m = min(as.numeric(myList$Header %>% filter(Index %in% Cal.Index) %>% select(Main.Runs) %>% pull())) # Only eQuant
                         ){
  
  # Tabelle für NWGs und BGs
  OLS.List <-list()
  Notes <- ""
  
  for (v in Analytes) { # Schleife für Anzahl definierter Elemente
    
    # Suche nach Blanks für Kaiser
    which.Blank.Idx <- which(Std.Concentration[[v]] == 0)
    
    #________________________________
    # Vor-Definition der Standardabweichung, sodass bei Fehlern in der Abfrage die SD nicht betrachtet wird
    #________________________________
    singleSD <- NULL
    
    if(!length(which.Blank.Idx) == 0){
      singleSD <- SD[which.Blank.Idx,v]}
    
    if (is.na(singleSD)) {
      BGMethod == "DIN32645"
      Notes <- c(Notes,"Die Kalibriergraden-Methode wurde automatisch ausgewählt, da der Blindwert scheinbar nicht gemessen wurde!")
    }
    #________________________________

    
    if(BGMethod == "DIN32645"){
      ###########################
      ### Kalibriergraden-Methode
      ###########################
      
      OLS <- myBEN_DIN(Std.Concentration[[v]],Corr.Average[,v],k = 3,alpha = alpha,m=m,Method = "Kal",sl = NULL,Titel = v,Notes = Notes) # Ordinary Least Square (OLS) Methode nach DIN
      
    } else if(BGMethod == "Leer"){
      ###########################
      ### Leerwert-Methode
      ###########################
      
      OLS <- myBEN_DIN(Std.Concentration[[v]],Corr.Average[,v],k = 3,alpha = alpha,m=m,Method = "Leer",singleSD,Titel = v,Notes = Notes) # Ordinary Least Square (OLS) Methode nach DIN
      
    } else if(BGMethod == "Kaiser"){
      ################################
      ### Schnellschätzung nach Kaiser
      ################################
      
      OLS <- myBEN_DIN(Std.Concentration[[v]],Corr.Average[,v],k = 3,alpha = alpha,m=m,Method = "Kaiser",singleSD,Titel = v,Notes = Notes) # Ordinary Least Square (OLS) Methode nach DIN
      
    } 
    OLS.List[[v]]<- OLS
  }
  
  OLS.List
}
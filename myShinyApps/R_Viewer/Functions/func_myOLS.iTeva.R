myOLS.iTeva <- function(Analytes = myList$Calibration$Info$Analytes,
                        Corr.Average = myList$Measured$Corrected.Means,
                        Std.Concentration = myList$Calibration$Definition,
                        BGMethod.Init = myList$Input.Parameter$BGMethod,
                        SD = myList$Measured$SD.Outliers,
                        m = 1,
                        alpha = myList$Input.Parameter$Alpha,
                        outlier.test = myList$Input.Parameter$outlier){
  
  # Tabelle für NWGs und BGs
  OLS.List <-list()
  
  
  for (v in Analytes) { # Schleife für Anzahl definierter Elemente
    # Suche nach Blanks für Kaiser
    which.Blank.Idx <- which(Std.Concentration[[v]] == 0)
    
    # Vor-Definition der Standardabweichung, sodass bei Fehlern in der Abfrage die SD nicht betrachtet wird
    singleSD <- NULL
    
    if(!length(which.Blank.Idx) == 0){
      singleSD <- SD[which.Blank.Idx,v]}
    
    BGMethod <- BGMethod.Init
    Notes <- NULL
    if (is.na(singleSD)) {
      BGMethod <- "DIN32645"
      Notes <- c(Notes,"Die Kalibriergraden-Methode wurde automatisch ausgewählt, da der Blindwert scheinbar nicht gemessen wurde!")
    }
    
    if(BGMethod == "DIN32645"){
      ###########################
      ### Kalibriergraden-Methode
      ###########################
      
      OLS <- myBEN_DIN(x = Std.Concentration[[v]],y = Corr.Average[Std.Concentration$Index,v],k = 3,alpha = alpha,m=m,Method = "Kal",sl = NULL,Titel = v,outlier.test = outlier.test,Notes = Notes) # Ordinary Least Square (OLS) Methode nach DIN
      
    } else if(BGMethod == "Leer"){
      ###########################
      ### Leerwert-Methode
      ###########################
      if (m < 3) {m <-3}
      OLS <- myBEN_DIN(x = Std.Concentration[[v]],y = Corr.Average[Std.Concentration$Index,v],k = 3,alpha = alpha,m=m,Method = "Leer",sl = singleSD,Titel = v,outlier.test = outlier.test,Notes = Notes) # Ordinary Least Square (OLS) Methode nach DIN
      
    } else if(BGMethod == "Kaiser"){
      ################################
      ### Schnellschätzung nach Kaiser
      ################################
      
      if (m < 3) {m <-3}
      OLS <- myBEN_DIN(x = Std.Concentration[[v]],y = Corr.Average[Std.Concentration$Index,v],k = 3,alpha = alpha,m=m,Method = "Kaiser",sl = singleSD,Titel = v,outlier.test = outlier.test,Notes = Notes) # Ordinary Least Square (OLS) Methode nach DIN
      
    } 
    OLS.List[[v]]<- OLS
  }
  
  OLS.List
}
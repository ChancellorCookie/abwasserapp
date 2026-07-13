myiCAPQeval <- function() {
  source('func_csvImport_Sort.R', encoding = 'UTF-8') # Datei, die die Import Funktion beinhaltet
  source("func_myOutlier.R")  # Datei, die Funktion zur Detektion von Ausreißern beinhaltet
  
  myList <- readCSV_func()
 
  
  # Extraktion der Variablen
  # myList besteht aus:
  # massTrace                       -> Vektor mit den verschiedenen MasseSpuren 
  # dataSorted                      -> df mit den Sortierten Daten
  # BrigidMS                        -> df mit den definierten Kalibrations-Standards und
  #                                     Verdünnungsfaktoren
  # ExtCal_StandardConcentration    -> df mit definierten Konzentration der
  #                                     Kalibrations-Standards
  # data_Units                      -> Vektor mit Einheiten der Intensitäten und
  #                                     Konzentrationen        
  
  
  Raw_Intensity <- myList[[2]]                # Eigentliche Messdaten
  massTraces <- myList[[1]]                   # Massespuren
  sampleList <- unique(Raw_Intensity$Index)   # Eineindeutige Indizes für die Zuordnung der Wiederholungsmessungen
  Sonstiges <- myList[[3]]                    # Daten aus der Qtegra-SampleList
  CalDef <- myList[[4]]                       # Definition der Konzentrationen der Kalibration
  myUnits <-  myList[[5]]                     # Auflistung der Einheiten für Plots und Results-Tabelle
  acqData <- myList[[6]]                      # Datum der Analyse
  acqName <- myList[[7]]
  ######################################################
  
  # Berechnung der Interner-Standard Korrektur
  
  # Automatische Erkennung der Spalten mit internen Standards 
  # Die Erkennung funktioniert über fehlende Information über Kalibrations-Definitionen (NA Spalten)
  massTraceIntStd <- sapply(CalDef[5:nrow(CalDef),],function(i)all(is.na(i)))
  # Ermittlung des Spalten Labels (steht in der 3ten  Zeile)
  massTraceIntStd <- names(CalDef[massTraceIntStd])
  
  # Berechnung der IntStd-Faktoren.
  # Trennung der IntStd-Spalten von den restlichen Daten
  IntStd <- Raw_Intensity[names(Raw_Intensity) %in% c("Index","Labels",massTraceIntStd)]
  # Berechnung der Faktoren (Normierung auf die Werte in der ersten Zeile)
  IntStd[,massTraceIntStd] <- IntStd[,massTraceIntStd]/c(IntStd[1,massTraceIntStd]) 
  
  ######################################################
  
  # Trennung der Massespuren nach Methode.
  # Prüfung, ob alle Methoden einen entsprechenden Internen Standard haben
  # Vergleich der Spaltennamen in "Raw_Intensity" mit Vektor "massTraceIntStd"
  
  # str_split_fixed() ist eine Funktion des Package "stringr" und bewirkt eine Trennung des Strings "massTraceIntStd" in zwei Strings mit "." als Trennzeichen. [,2] gibt nur den zweiten Wert aus.
  methodNamesIntStd <- unique(str_split_fixed(massTraceIntStd,"[.]",2)[,2]) 
  # Extraktion der Methoden aus der Raw_Intensity Tabelle OHNE die Internen Standard-Spalten
  methodNamesRawInt <- unique(str_split_fixed(massTraces[!massTraces %in% massTraceIntStd],"[.]",2)[,2])
  
  # Prüfung auf Übereinstimmung
  if(identical(sort(methodNamesIntStd),sort(methodNamesRawInt))){
    methodNames <- methodNamesIntStd
    rm(methodNamesIntStd,methodNamesRawInt)
  } else{
    print("Die Anzahl der Methoden stimmt nicht mit der Anzahl der internen Standards überein!")
  }
  
  
  ######################################################
  
  ########## BUG ######### 
  # Wenn mehrere Interne Standards mit der gleichen Methode gemessen wurden, muss der USER manuell zuweisen.
  ########################
  
  ######################################################
  
  # Zuweisung der Korrekturfaktoren im IntStd DataFrame anhand der Methodnames
  
  # Die Variable "massTraceVec" bildet nun einen Vektor, der in Schleifen als Prüfparameter verwendet werden kann, so   dass Befehle nur auf die hier als "True" markierten indizes angewendet werden. Sie untere "If"-Schleife.
  
  massTraceVec <- sapply(Raw_Intensity,function(i)all(is.numeric(i)-is.integer(i)))
  # integers müssen explizit ausgenommen werden. Das erfolgt am einfachsten über ein "-".
  
  
  Corr_Intensity <- Raw_Intensity # Neues DF für Speicherung der normierten Werte
  
  for(i in 1:length(Raw_Intensity)){
    if(massTraceVec[i]){
      c_name <- names(Raw_Intensity)[i]
      c_meth <- as.character(str_split_fixed(c_name,"[.]",2)[,2])
      int_meth <- IntStd %>% select(contains(c_meth)) %>% pull() # pull - DataFrame column as vector
      Corr_Intensity[,i] <- Raw_Intensity[,i]/int_meth
    }
  }
  rm(c_name,c_meth,int_meth,i)
  
  ######################################################
  
  # Extraktion der Kalibrationsdaten aus der "Sonstiges", "Raw_Intensity" und "CalDef" Tabellen.
  
  # Ermittlung der Probenindizes, die in der Qtegra Sequenz als STD definiert wurden
  StdIndex <- Sonstiges$Index[Sonstiges$`Sample Type` == "STD"]
  StdIndex <- StdIndex[!StdIndex %in% c(NA,"")]
  
  # Das gleiche gilt für die Proben als "UNKNOWN"
  SmplIndex <- Sonstiges$Index[Sonstiges$`Sample Type` == "UNKNOWN"]
  SmplIndex <- SmplIndex[!SmplIndex %in% c(NA,"")]
  
  # Die Indizes dienen der Eineindeutigen Zuordnung der Wiederholungsmessungen einer Probe, während die "Labels" sich in der Messsequenz wiederholen dürfen.
  
  CalY <- Corr_Intensity[Corr_Intensity$Index %in% as.integer(StdIndex),]
  #CalY <- CalY[!massTraceIntStd] # Die Internen Standards können nicht in der Kalibration ausgewertet werden und werden daher bereinigt
  
  # Die Lineare Regression funktioniert logischerweise nicht bei Spalten mit ausschließlich NA auf einer Achse
  # Entfernung der Spalten des Internen Standards aus CalY
  CalY <- drop(CalY[!names(CalY) %in% massTraceIntStd])
  
  # Bereinigung der CalDef Tabelle, damit keine unnötigen Zeilen vorhanden sind
  CalDef_Clean <- CalDef[CalDef$Index %in% StdIndex,]
  CalDef_Clean <- drop(CalDef_Clean[!names(CalDef_Clean) %in% massTraceIntStd])
  
  #CalDef <- CalDef[!massTraceIntStd]
  
  # Die Anzahl der Daten in Calibration_DF und CalDef stimmen noch nicht überein, da CalDef die Wiederholungsmessungen nicht berücksichtigt. Zwei Möglichkeiten die Dimensionen anzugleichen.
  # Reduktion der Calibration_DF durch mean(), oder
  # Erweiterung der CalDef durch rep()
  
  ######################################################
  
  CalX <- CalY 
  for(i in 1:nrow(CalDef_Clean)){
    CalX_Vec <- CalY$Index == CalDef_Clean[i,"Index"]
    CalX[CalX_Vec,4:ncol(CalX)] <- CalDef_Clean[i,3:ncol(CalDef_Clean)]
    # Nicht vergessen, dass CalY bzw. CalX die Spalte "Wiederholungen" zusätzlich hat.
  }
  rm(CalX_Vec)
  
  ######################################################
  
  # Plotten der Regression

  source("func_myCaliGraph.R")
  for (i in 4:ncol(CalX)) {
    df <- data.frame(CalX[,c(1,3,i)],CalY[,i])
    
    names(df)<-c(names(CalX)[c(1,3)],"xVal","yVal")
    g <- myCaliGraph(df,names(CalX[i]),myUnits) # Beinhaltet auch die Berechnung der Lineraren Regression
    ##ggsave(paste0("./Plots/",names(CalX[i]),".png"),width = 16,height = 9,units = "cm")
    print(g)
  }  
  
  ######################################################
  
  # Berechnung der Mittelwerte, Standardabw. und rel. Standardabw.
  
  Corr_Intensity_Clean <- drop(Corr_Intensity[!names(Corr_Intensity) %in% massTraceIntStd])
  massTraceClean <- names(Corr_Intensity_Clean[,4:ncol(Corr_Intensity_Clean)])
  
  rsd <- function(i){sd(i)/mean(i)} # Die Funktion ist in R nicht vordefiniert.
  Int_Mean <- Corr_Intensity_Clean %>% select(-Wiederholung) %>% group_by(Index,Labels) %>% summarise_all(funs(mean)) %>% as.data.frame()  # eigentliche Berechung über summarise().
  Int_SD <- Corr_Intensity_Clean %>% select(-Wiederholung) %>% group_by(Index,Labels) %>% summarise_all(funs(sd)) %>% as.data.frame()  # eigentliche Berechung über summarise().
  Int_RSD <- Corr_Intensity_Clean %>% select(-Wiederholung) %>% group_by(Index,Labels) %>% summarise_all(funs(rsd)) %>% as.data.frame()  # eigentliche Berechung über summarise().
  
  ######################################################
  
  # Ermittlung des passenden Kalibrierbereichs, in Abhängigkeit der Intensität der Probe
  # Matrix mit   (Anzahl Proben) x (Anzahl Massen)   Dimension
  
  CalIndex   <- Int_Mean[SmplIndex,]
  OutOfRange <- Int_Mean[SmplIndex,]
  
  CalIndex[1:nrow(CalIndex),3:ncol(CalIndex)] <- 0
  OutOfRange[1:nrow(OutOfRange),3:ncol(OutOfRange)] <- 0
  # Abfrage für die Schleife
  for (i in 1:length(SmplIndex)) { # Schleife für Anzahl an Proben
    for (j in 3:ncol(Int_Mean)) { # Schleife für Anzahl an Massen
      for (k in length(StdIndex):3) { # Schleife für Anzahl an Standards
        
        smpl <- Int_Mean[SmplIndex[i],j]
        std2 <- Int_Mean[StdIndex[k],j] 
        std1 <- Int_Mean[StdIndex[k-1],j]
        
        test_low    <- smpl < std2   
        test_high   <- smpl > std1 
        test_higher <- smpl > std2 
        
        if (k == length(StdIndex) & smpl > std2) { # wenn größer als höster Standard
          CalIndex[i,j] <- k
          OutOfRange[i,j] <- 1
          break
        } else if(std1 < smpl & smpl < std2) {
          CalIndex[i,j] <- k
          break
        } else if(k == 3){ # wenn kleiner als der dritte standard
          CalIndex[i,j] <- k
          break
        }
      }
    }
  }
  
  ######################################################
  
  # Berechnung der Konzentrationen
  
  source("func_myConcCalc.R")
  source("func_myLinReg.R")
  
  conc <- CalIndex
  conc[1:nrow(conc),3:ncol(conc)] <- 0
  
  VF <- Sonstiges$`Dilution Factor`[SmplIndex]
  
  for (i in 4:ncol(CalX)) { # Schleife für Anzahl an Massen
    
    # DataFrame der Kalibrationsdaten
    df <- data.frame(CalX[,c(1,3,i)],CalY[,i])
    names(df)<-c(names(CalX)[c(1,3)],"xVal","yVal")
    df <- df[order(df[3]),] # Sortieren der Datenpunkte nach aufsteigender Konzentration der Standards
    
    nAllStds <- max(df$Index)
    BG <- Int_SD[1,i-1]*10/myLinReg(df,nAllStds)[[2]] # Berechnung der Bestimmungsgrenze
    
    for (j in 1:nrow(CalIndex)) { # Schleife für Anzahl an Proben
      
      if (Int_Mean[SmplIndex[j],(i-1)] <= Int_SD[1,i-1]*10) { # Prüfung auf BG
        c <- BG
      } else {
        nStds <- CalIndex[j,(i-1)]
        lr <- myLinReg(df,nStds)
        c <- (Int_Mean[SmplIndex[j],(i-1)] - lr$b0)/lr$b1
        
        while (lr$rSquare < .99 & nStds < nAllStds ) {
          nStds <- nStds + 1
          lr <- myLinReg(df,nStds)
          c <- (Int_Mean[SmplIndex[j],(i-1)] - lr$b0)/lr$b1
        }
      }
      
      if (c<=BG) {
        c <-BG
      }
      
      # Verechnen mit dem Verdünnungsfaktor
      c <- c*VF[j]
      
      
      n <- 2  # Anzahl Signifikanter Stellen
      c <- round(c,n-1-floor(log10(c)))
      BG <- round(BG,n-1-floor(log10(BG)))
      
      if (c==BG | c == BG*VF[j]) {
        c <- paste("<",c)
      }
      
      conc[j,(i-1)] <- c
      
    }
  }

  ######################################################
  
  # Ausgabe der Ergebnisse
  
  conc_clean <- conc %>% filter(str_detect(Labels,"^M")) %>% select(Labels,`195Pt.mp_STD`) %>% `names<-`(.,c("Labels",paste0("Pt ","(",myUnits$Units[3],")")))
  LaTeX <- xtable(conc_clean)
  return(LaTeX)
  
}
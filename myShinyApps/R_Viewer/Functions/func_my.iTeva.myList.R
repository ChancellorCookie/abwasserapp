my.iTeva.myList <- function(myParams = NULL,myList = NULL){
  
  # Working Directory
  # Der Ordner, der die Messdaten enthällt
  
  
  
  # Notwendige Packages für das Ausführen des Skripts
  
  require(dplyr)
  require(tidyr)
  require(magrittr)
  require(stringr)
  require(ggplot2)
  require(outliers)
  require(RColorBrewer)
  require(kableExtra)
  require(xlsx)
  require(DT)
  #require(tcltk2)
  
  #Unterfunktionen werden geladen
  
  # Directory Structure
  # Ordner Struktur
  path.root <- normalizePath(getwd(),winslash = "\\")
  path.QC <- normalizePath(paste0(path.root,"/QC"),winslash = "\\")
  path.TargetValues <- normalizePath(paste0(path.QC,"/Akzeptanzkriterien"),winslash = "\\")
  path.QCtoWrite <- normalizePath(paste0(path.QC,"/Regelkarten"),winslash = "\\")
  path.Functions <- normalizePath(paste0(path.root,"/Functions"),winslash = "\\")
  path.Calibration <- normalizePath(paste0(path.root,"/Calibration"),winslash = "\\")
  
  
  # Regular Expression "^func.*.R$" defines that only R-Files beginning by "func" and ending with ".R" are loaded
  # Sonderzeichen, die variable Zeichenketten definieren. Siehe CheatSheet
  funcs <- normalizePath(dir(path.Functions,"^func.*.R$",full.names = T))
  for (i in funcs) {
    source(i,encoding = 'UTF-8')
  }
  
  #removes temporary *.cpt files in rmd-File containing directory
  # Im Server-Betrieb wird jede Datei erstmal in einen temporären Ordner geladen und mit einem temporären Dateinamen versehen  
  myTempFileRemove(path.root,FilePattern = ".cpt") 
  
  # Load Data ----
   if (is.null(myList)) {
    myList <- suppressWarnings(iCAP6000_process(myParams))
  }

  # Search for Outliers in single Values ----
  myList$Measured[["Means.Outliers"]] <- myList$Measured$Means
  myList$Measured[["SD.Outliers"]] <- myList$Measured$SD
  myList$Measured[["RSD.Outliers"]] <- myList$Measured$RSD
  myList$Measured[["Outlier.Test"]] <- list()
  
  myList$IntStd[["Means.Outliers"]] <- myList$IntStd$Means
  myList$IntStd[["SD.Outliers"]] <- myList$IntStd$SD
  myList$IntStd[["RSD.Outliers"]] <- myList$IntStd$RSD
  myList$IntStd[["Outlier.Test"]] <- list()
  wdh <- myList$Header$Messwiederh.
  n1 <- cumsum(wdh)-wdh+1
  n2 <- cumsum(wdh)
  if (myList$Input.Parameter$UseMeans) {
    outlier.test <- FALSE
  } else {
    outlier.test <- TRUE
  }
  alpha <- 0.1
  Nalimov <- F
  
  for (i in 1:length(wdh)) { # Schleife für Anzahl an Proben
    current.sample <- paste(i,myList$Header$Probenname[i],sep = ".")
    myCurrentSampleOutlierList <- list("Perform.Outlier.Test" = outlier.test,
                                       "Alpha" = alpha,
                                       "Nalimov" = Nalimov)
    
    
    for (j in 1:length(myList$Measured$Analytes)){# Schleife für Anzahl an Analyt
      vec <- myList$Measured$Values[n1[i]:n2[i],j]
      current.Analyte <- myList$Measured$Analytes[j]
      
      myTempResults <- suppressWarnings(myMean(vec = vec,
                                               outlier.test = outlier.test,
                                               alpha = alpha,TestName = "Grubbs",
                                               Nalimov = Nalimov))
      
      myList$Measured$Means.Outliers[i,j] <- round(myTempResults$mean,1)
      myList$Measured$SD.Outliers[i,j] <- myTempResults$sd
      myList$Measured$RSD.Outliers[i,j] <- myTempResults$rsd
      myCurrentSampleOutlierList[[current.Analyte]] <- myTempResults$Outlier.Test
    }
    
    myList$Measured$Outlier.Test[[current.sample]] <- myCurrentSampleOutlierList
    
    
    if(myList$IntStd$Perform.IntStd){
      myCurrentSampleOutlierList <- list("Perform.Outlier.Test" = outlier.test,
                                         "Alpha" = alpha,
                                         "Nalimov" = Nalimov)
      
      for (k in 1:length(myList$IntStd$Standards)){
        vec <- myList$IntStd$Values[n1[i]:n2[i],k]
        current.Analyte <- myList$IntStd$Standards[k]
        
        myTempResults <- suppressWarnings(myMean(vec = vec,
                                                 outlier.test,
                                                 alpha,
                                                 Nalimov))
        
        myList$IntStd$Means.Outliers[i,k] <- round(myTempResults$mean,1)
        myList$IntStd$SD.Outliers[i,k] <- myTempResults$sd
        myList$IntStd$RSD.Outliers[i,k] <- myTempResults$rsd
        myCurrentSampleOutlierList[[current.Analyte]] <- myTempResults$Outlier.Test
      }
      myList$IntStd$Outlier.Test[[current.sample]] <- myCurrentSampleOutlierList
    }
    
  }
  
  # Internal Correction ---- 
  if(myList$IntStd$Perform.IntStd){
    #Berechnung der Faktoren des Internen Standards
    IntFactDF_func <- function(df){ # Erfordert einen einspaltigen DataFrame
      Denominator <- df[1]
      dfFact <- df/Denominator
      dfFact
    }
    
    IntStdFactorDF <- myList$IntStd$Means.Outliers
    IntStdFactorDF[myList$IntStd$Standards] <- sapply(IntStdFactorDF[myList$IntStd$Standards], function(i)IntFactDF_func(i))
    myList$IntStd[["Int.Corr.Factors"]] <- IntStdFactorDF
    
  }
  
  
  if(myList$IntStd$Perform.IntStd){ # Wenn jedem Messwert ein InternerStandard zugewiesen ist
    trace.Analyte <- myList$Measured$Analytes
    if (myList$IntStd$IntCorr.Selection.WL == "auto") {
      trace.IntStd <- unlist(myList$Measured$ISRef)
    }else{ # Es gibt derzeit keine Möglichkeit den IS vorzugeben
      trace.IntStd <- unlist(myList$Measured$ISRef)
    }
    
    data.Analyte <- myList$Measured$Means.Outliers
    data.IntStd <- myList$IntStd$Int.Corr.Factors
    corr.Analyte <- data.Analyte
    
    for (i in 1:length(trace.Analyte)) { # Verrechnung dazugehörenden Internen Standards
      corr.Analyte[trace.Analyte[i]] <- data.Analyte[trace.Analyte[i]]/data.IntStd[trace.IntStd[i]]
    }
    
    myList$Measured[["Corrected.Means"]] <- corr.Analyte
    
    rm(trace.Analyte,trace.IntStd,data.Analyte,data.IntStd,corr.Analyte)
  }else{
    myList$Measured[["Corrected.Means"]] <- myList$Measured$Means.Outliers
  }
  
 
  
  # Calculation of Concentration ---- 
  #_________________________________________________________________________________________________________________________________
  # Prüfung ob Anzahl der im Header -> Probentyp definierten Standards kleiner ist als die im User-Input.
  # Der umgekehrte Fall, dass mehr Standards gemessen, als definiert wurden bemerkt man in der Regel nur daran, 
  # dass weniger Kalibrationspunkte vorhanden sind als erwartet wurden.
  if (length(myList$Header %>% filter(Probentyp %in% "Kal") %>% pull()) < length(myList$Calibration$Definition$Index)) {  
    # Wenn die User Definition länger ist, kann diese einfach gekürzt werden.
    # Falls auch die Konzentrationen nicht passen, sieht man das spätestens an der Kalibrierkurve. 
    # Im schlimmsten Fall ist die Kalibration dennoch Linear, aber man sieht es an den QC Werten
    myList$Calibration$Definition <- myList$Calibration$Definition[1:length(myList$Header %>% filter(Probentyp %in% "Kal") %>% pull()),]
  }
  
  myList <- my.iTeva.Concentration.Evaluation(myList)
  

  # Report ----
  myList$Report[["Raw"]] <- data.frame("Labels"=myList$Header$Probenname,myList$Measured$Corrected.Means)
  myList$Report[["Used.Standards"]] <- myList$Concentration$UsedStandards
  myList$Report[["Concentration"]] <- myList$Concentration$Concentration
  myList$Report[["Conc.less.BG"]] <- myList$Concentration$Conc.less.BG
  myList$Report[["Conc.DF"]] <- myList$Concentration$Conc.multiply.VF
  myList$Report[["Conc.Final"]] <- myList$Concentration$Conc.Final
  
  names(myList$Header$Messzeit) <- "StartTime"
  
  myList[["Solids"]] <- data.frame("Index" = myList$Header$Index,
                                   "Labels" = myList$Header$Probenname,
                                   "Amount" = as.character(rep("",length(myList$Header$Probenname))),
                                   "Unit.Amount" = c(rep("mg",length(myList$Header$Probenname))),
                                   "Volume" = as.character(rep("",length(myList$Header$Probenname))),
                                   "Unit.Volume" = c(rep("mL",length(myList$Header$Probenname))),
                                   stringsAsFactors = FALSE)
  
  return(myList)
}
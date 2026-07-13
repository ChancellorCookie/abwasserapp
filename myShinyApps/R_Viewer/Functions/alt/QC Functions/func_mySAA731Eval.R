mySAA731Eval <- function(myParams){
  require(dplyr)
  require(tidyr)
  require(stringr)
  require(ggplot2)
  require(RColorBrewer)
  require(xlsx)
  
  # Define Path of Working Directory to store variables in it
  path.root <- normalizePath(getwd(),winslash = "\\")
  path.QC <- normalizePath(paste0(path.root,"/QC"),winslash = "\\")
  path.TargetValues <- normalizePath(paste0(path.QC,"/Akzeptanzkriterien"),winslash = "\\")
  path.QCtoWrite <- normalizePath(paste0(path.QC,"/Regelkarten"),winslash = "\\")
  path.Functions <- normalizePath(paste0(path.root,"/Functions"),winslash = "\\")
  path.Calibration <- normalizePath(paste0(path.root,"/Calibration"),winslash = "\\")
  
  
  # Regular Expression "^func.*.R$" defines that only R-Files beginning by "func" and ending with ".R" are loaded
  funcs <- normalizePath(dir(path.Functions,"^func.*.R$",full.names = T))
  for (i in funcs) {
    source(i,encoding = 'UTF-8')
  }
  
  
  # Definition of initial parameters
  
  if (is.null(myParams)){
    #Standard Einstellung für Aufruf außerhalb von Shiny
    myParams$Operator<-"Dronov"
    myParams$BGMethod<-"DIN32645" #   "DIN32645"  "Kaiser"
    myParams$PipettenList <- c(1067,1097,1099)
    myParams$QCSoll_731_MM2 <- 50
    myParams$QCSoll_731_MM3 <- 8216
    myParams$UseIntCorr <-T
    myParams$UseMeans <-T
    myParams$BG.SAA <-T
    myParams$Alpha <- 0.01
    myParams$Masse <- "195"
    myParams$Significant.Digits <- 2
    myParams$PathRegelKarteVBW <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.1_VBW.csv"),winslash = "\\")
    myParams$PathRegelKarteMM2 <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.1_MM2_QC.csv"),winslash = "\\")
    myParams$PathRegelKarteMM3 <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.1_MM3_QC.csv"),winslash = "\\")
    myParams$PathZielwertMM2 <- normalizePath(paste0(path.TargetValues,"/SAA_7.3.1_MM2_Zielwert.csv"),winslash = "\\")
    myParams$PathZielwertMM3 <- normalizePath(paste0(path.TargetValues,"/SAA_7.3.1_MM3_Zielwert.csv"),winslash = "\\")
    
    ## Öffnet das Dialogfenster zum Auslesen des vollständigen Dateinamen, der Messdaten
    myParams$inFile <- tk_choose.files(filters = matrix( c("CSV Dateien","*.csv","Alle Dateien","*.*"),
                                                         nrow = 2,
                                                         ncol = 2,
                                                         byrow = T,
                                                         dimnames = list(c("csv","All"),c("",""))))
    ## Auslesen der Zielwert-Daten
    myParams$ZielwertMM2 <-read.csv(myParams$PathZielwertMM2,
                                    header = T,
                                    stringsAsFactors = F)
    
    myParams$ZielwertMM3 <-read.csv(myParams$PathZielwertMM3,
                                    header = T,
                                    stringsAsFactors = F)
  } else {
    
    #Standard Einstellung für Aufruf außerhalb von Shiny
    if(myParams$Operator =="NA"){myParams$Operator<-"Dronov"}
    if(myParams$BGMethod =="NA"){myParams$BGMethod<-"DIN32645"} #   "DIN32645"  "Kaiser"
    if(myParams$PipettenList =="NA"){myParams$PipettenList <- c(1067,1097,1099)}
    if(myParams$QCSoll_731_MM2 =="NA"){myParams$QCSoll_731_MM2 <- 50}    else  {as.numeric(myParams$QCSoll_731_MM2)}
    if(myParams$QCSoll_731_MM3 =="NA"){myParams$QCSoll_731_MM3 <- 8216}  else  {as.numeric(myParams$QCSoll_731_MM3)}
    if(myParams$UseIntCorr == "NA"){myParams$UseIntCorr <-T}
    if(myParams$UseMeans == "NA"){myParams$UseMeans <-T}
    if(myParams$BG.SAA == "NA"){myParams$BG.SAA <-T}
    if(myParams$Alpha == "NA"){myParams$Alpha <- 0.01}else{as.numeric(myParams$Alpha)}
    if(myParams$Masse == "NA"){myParams$Masse <- "195"}
    if(myParams$Significant.Digits == "NA"){myParams$Significant.Digits <- 2}else{as.numeric(myParams$Significant.Digits)}
    if(myParams$PathRegelKarteVBW =="NA"){myParams$PathRegelKarteVBW <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.1_VBW.csv"),winslash = "\\")}
    if(myParams$PathRegelKarteMM2 =="NA"){myParams$PathRegelKarteMM2 <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.1_MM2_QC.csv"),winslash = "\\")}
    if(myParams$PathRegelKarteMM3 =="NA"){myParams$PathRegelKarteMM3 <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.1_MM3_QC.csv"),winslash = "\\")}
    if(myParams$PathZielwertMM2 =="NA"){myParams$PathZielwertMM2 <- normalizePath(paste0(path.TargetValues,"/SAA_7.3.1_MM2_Zielwert.csv"),winslash = "\\")}
    if(myParams$PathZielwertMM3 =="NA"){myParams$PathZielwertMM3 <- normalizePath(paste0(path.TargetValues,"/SAA_7.3.1_MM3_Zielwert.csv"),winslash = "\\")}
    
    ## Öffnet das Dialogfenster zum Auslesen des vollständigen Dateinamen, der Messdaten
    if(myParams$inFile =="NA"){myParams$inFile <- tk_choose.files(filters = matrix( c("CSV Dateien","*.csv","Alle Dateien","*.*"),
                                                                                    nrow = 2,
                                                                                    ncol = 2,
                                                                                    byrow = T,
                                                                                    dimnames = list(c("csv","All"),c("",""))))}
    ## Auslesen der Zielwert-Daten
    myParams$ZielwertMM2 <-read.csv(myParams$PathZielwertMM2,
                                    header = T,
                                    stringsAsFactors = F)
    
    myParams$ZielwertMM3 <-read.csv(myParams$PathZielwertMM3,
                                    header = T,
                                    stringsAsFactors = F)
  }
  
  ##############################
  
  myPreList <- suppressWarnings(readCSV_iCAPQ_short(myParams$inFile))
  
  myList <- iCapQ_eQuant(myPreList,myParams)
  
  myList$eQuant[["Corrected.Data"]] <- myInternalCorrection(myList)
  
  myList <- my.eQuant.Concentration.Evaluation(myList)
  
  myList[["QC"]] <- myQC.SAA731(myList)
  
  save.myList(myList)
  
  ##############################
}
my.tQuant.myList <- function(myParams = NULL,myList = NULL){
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
  
  
  
  
  
  # Only for Debugging purpose
  
  # ini <- FALSE
  # if (is.null(myList)){ # Wenn noch keine myList vorhanden ist bzw. als Parameter übergeben wurde
  #   if (is.null(myParams$inFile)) { # Wenn auch keine myParams als Input.Parameter übergeben wurden, kann myList nicht erstellt werden.
  #     #warning("Entweder myParams oder myList müssen als Initiale Parameter übergeben werden")
  #     ini <- TRUE
  #     myParams <- myParams.Initial.SAA732()
  #   }
  #   myPreList <- suppressWarnings(readCSV_iCAPQ_short(myParams$inFile))
  # 
  #   myList <- iCapQ_eQuant(myPreList,myParams)
  # 
  # }else{
  #   myList$Input.Parameter$FileName <- basename(myList$Input.Parameter$inFile)
  #   myList$Input.Parameter$FilePath <- gsub(myList$Input.Parameter$FileName,"",myList$Input.Parameter$inFile)
  # }
  
  
  # Only for AppRun 
  if (is.null(myList)){ # Wenn noch keine myList vorhanden ist bzw. als Parameter übergeben wurde
    myPreList <- suppressWarnings(readCSV_iCAPQ_short(myParams$inFile,c("","0")))
    # myPreList <- readCSV_iCAPQ_short(myParams$inFile,c("","0")) # Only for Debugging
    myList <- iCapQ_tQuant(myPreList,myParams)
  }
  
  
  myList$Chromatography$Chromatogramm[["SortedChromatogramm"]] <- iCapQ_tQuant_Gather(Chromatogram = myList$Chromatography$Chromatogramm$Data,
                                                                                      massTraces = unique(myList$Chromatography$Chromatogramm$Analytes))
  
  myList <- my.tQuant.Concentration.Evaluation(myList)
  
  if(length(myList$Chromatography$Chromatogramm$SortedChromatogramm) != 0){
    myList$Chromatography[["Chromatogramm.Plots"]] <- my.tQuant.Chromatogramm.Plot(myList)
  }
  
  myList$Report[["Raw"]] <- myList$Chromatography$PeakArea$Data
  myList$Report[["Used.Standards"]] <- myList$Concentration$UsedStandards
  myList$Report[["Concentration"]] <- myList$Concentration$Concentration
  myList$Report[["Conc.less.BG"]] <- myList$Concentration$Conc.less.BG
  myList$Report[["Conc.DF"]] <- myList$Concentration$Conc.multiply.VF
  myList$Report[["Conc.Final"]] <- myList$Concentration$Conc.Final
  
  # if (ini) { # Only for Debugging
  #   myList <- myListQC.Initial.SAA731(myList)
  # }
  
  myList
  
}
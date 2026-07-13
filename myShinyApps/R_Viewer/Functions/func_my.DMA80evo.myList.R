my.DMA80evo.myList <- function(myParams = NULL,myList = NULL){
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
  
  
  if (is.null(myList)){ # Wenn noch keine myList vorhanden ist bzw. als Parameter übergeben wurde
    myList <- myDMA80(myParams)
  }
  
  myList <- my.DMA80evo.Concentration.Evaluation(myList)
  
  myList[["Samples"]] <- myDMA80.Smpl(df = myList$RawData$RawClean,
                                      Smpl.filter = "^M.+?[[:digit:]]/[[:digit:]].+?")
  
  myList$Samples[["Report"]] <- myDMA80.Report(df.Smpl = myList$Samples$Samples.Average %>% select(-SD),
                                               SigDigits = myList$Input.Parameter$Significant.Digits,
                                               BG = myList$Concentration$BG.Report[[1]])
  
  myList$Report[["Raw"]] <- myList$RawData$Raw
  myList$Report[["Used.Standards"]] <- data.frame("Ups" = "No data available")
  myList$Report[["Concentration"]] <- myList$Samples$Samples.Data %>% select(SampleName,Remark,Height,Hg..ng.,Concentration,Unit.2,CreationDate,Pos,Amount,Unit,Cell)
  myList$Report[["Conc.less.BG"]] <- data.frame("Ups" = "No data available")
  myList$Report[["Conc.DF"]] <- data.frame("Ups" = "No data available")
  myList$Report[["Conc.Final"]] <- myList$Samples$Report
  
  myList[["Solids"]] <- data.frame("Index" =  myList$RawData$Raw$Nr,
                                   "Labels" = myList$RawData$Raw$SampleName,
                                   "Amount" = myList$RawData$Raw$Amount,
                                   "Unit.Amount" = myList$RawData$Raw$Unit,
                                   "Volume" = rep(NA,length(myList$RawData$Raw$SampleName)),
                                   "Unit.Volume" = rep("mL",length(myList$RawData$Raw$SampleName)),
                                   stringsAsFactors = FALSE)
  
  return(myList)
  
}
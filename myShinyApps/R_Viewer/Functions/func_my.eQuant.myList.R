my.eQuant.myList <- function(myParams = NULL,myList = NULL){
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
    #myPreList <- suppressWarnings(readCSV_iCAPQ_short(myParams$inFile))
     myPreList <- readCSV_iCAPQ_short(myParams$inFile) # For Debugging
    if(myParams$System == "eQuant (MS)"){
      myList <- iCapQ_eQuant(myPreList,myParams)
    } else if (myParams$System == "eQuant (OES)"){
      myList <- iCapQ_eQuant_OES(myPreList,myParams)
    }
  }
  
  # Edit: Split Averaged and Single Intensitey and remove Container
  myList$Raw.Data$eQuant[["Corrected.Data"]] <- myInternalCorrection(myList)
   
  # Edit: Remove Container and Split in subfunctions
  myList <- my.eQuant.Concentration.Evaluation(myList)
  
  
  # Edit: Remove doubles 
  myList$Report[["Raw"]] <- myList$Raw.Data$eQuant$Raw$Raw.Average$Data
  myList$Report[["Used.Standards"]] <- myList$Concentration$UsedStandards
  myList$Report[["Concentration"]] <- myList$Concentration$Concentration
  myList$Report[["Conc.less.BG"]] <- myList$Concentration$Conc.less.BG
  myList$Report[["Conc.DF"]] <- myList$Concentration$Conc.multiply.VF
  myList$Report[["Conc.Final"]] <- myList$Concentration$Conc.Final
  
  
  if(myParams$System == "eQuant (MS)"){
    myList[["Solids"]] <- myList.eQuant.GetSolids(BrigidMS.Solids = myList$Raw.Data$PreSorted$BrigidMS$Data %>% select(Index,Labels,Amount,`Final Quantity`))
  } else if (myParams$System == "eQuant (OES)"){
    myList[["Solids"]] <- myList.eQuant.GetSolids(BrigidMS.Solids = myList$Raw.Data$PreSorted$iCapOES$Data %>% select(Index,Labels,Amount,`Final Quantity`))
  }
   
  myList
  
}
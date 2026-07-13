myeQuantEval <- function(inFile,myParams = NULL){
  
  if (missing(inFile)) {
    stop("inFile must specify a file with data")
  }
  
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
    myParams$Operator<-"Dronov"
    myParams$BGMethod<-"DIN32645"
    myParams$Alpha<-0.01
    myParams$PipettenList <- c(1067,1097,1099)
    myParams$Significant.Digits <- 2
    myParams$UseMeans <-T
    myParams$UseIntCorr <-T
    if (is.null(inFile)) {
      myParams$inFile <- tk_choose.files(filters = matrix( c("CSV Dateien","*.csv","Alle Dateien","*.*"),
                                                           nrow = 2,
                                                           ncol = 2,
                                                           byrow = T,
                                                           dimnames = list(c("csv","All"),c("",""))))
    } else {myParams$inFile <- inFile}
    
    
    
    
  } else {
    #Standard Einstellung für Betrieb außerhalb von Shiny
    if(myParams$Operator =="NA"){myParams$Operator<-"Dronov"}
    if(myParams$BGMethod =="NA"){myParams$BGMethod<-"DIN32645"}
    if(myParams$Alpha =="NA"){myParams$Alpha<-0.01}
    if(myParams$PipettenList =="NA"){myParams$PipettenList <- c(1067,1097,1099)}
    if(myParams$Significant.Digits == "NA"){myParams$Significant.Digits <- 2}
    if(myParams$UseMeans == "NA"){myParams$UseMeans <-T}
    if(myParams$UseIntCorr == "NA"){myParams$UseIntCorr <-T}
    if(myParams$inFile =="NA"){myParams$inFile <- tk_choose.files(filters = matrix( c("CSV Dateien","*.csv","Alle Dateien","*.*"),
                                                                                    nrow = 2,
                                                                                    ncol = 2,
                                                                                    byrow = T,
                                                                                    dimnames = list(c("csv","All"),c("",""))))}
  }  
  
  
  ##############################
  
  myPreList <- suppressWarnings(readCSV_iCAPQ_short(myParams$inFile))
  
  myList <- iCapQ_eQuant(myPreList,myParams)
  
  myList$eQuant[["Corrected.Data"]] <- myInternalCorrection(myList)
  
  myList <- my.eQuant.Concentration.Evaluation(myList)
  
  # Funtioniert nicht mit Shiny-Server
  if(serverInfo()$shinyServer){save.myList(myList)}
     
  
  return(myList)
  
  ##############################
}
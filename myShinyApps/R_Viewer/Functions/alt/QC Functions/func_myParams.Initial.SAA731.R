myParams.Initial.SAA731 <- function(){
  
  
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
  
  
  myParams <- list()
  
  
  myParams$Operator <- "Sadlowski"
  myParams$BGMethod <- "Kaiser"
  myParams$PipettenList <- c("123","456","789","","")
  myParams$UseIntCorr <- TRUE
  myParams$UseMeans <- TRUE
  myParams$BG.SAA <- TRUE
  myParams$Alpha <- 0.01
  myParams$Masse <- c("195")
  myParams$Significant.Digits <- 2
  
  
  myParams$inFile <- tk_choose.files(filters = matrix( c("CSV Dateien","*.csv","Alle Dateien","*.*"),
                                                       nrow = 2,
                                                       ncol = 2,
                                                       byrow = T,
                                                       dimnames = list(c("csv","All"),c("",""))))
  myParams
}

myListQC.Initial.SAA731 <- function(myList){
  myList$QC$Input.Parameter$Blanks$VBW$PathControlChart        <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.1_VBW.csv"),winslash = "\\")
  myList$QC$Input.Parameter$Blanks$VBW$PathZielwert            <- normalizePath(paste0(path.TargetValues,"/SAA_7.3.1_VBW_Zielwert.csv"),winslash = "\\")
  myList$QC$Input.Parameter$Blanks$VBW$DataZielwert            <- read.csv(myList$QC$Input.Parameter$Blanks$VBW$PathZielwert,header = T,stringsAsFactors = F)
  
  myList$QC$Input.Parameter$Standards$MM2$QCSoll.731.MM2          <- 50
  myList$QC$Input.Parameter$Standards$MM2$PathControlChart        <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.1_MM2_QC.csv"),winslash = "\\")
  myList$QC$Input.Parameter$Standards$MM2$PathZielwert            <- normalizePath(paste0(path.TargetValues,"/SAA_7.3.1_MM2_Zielwert.csv"),winslash = "\\")
  myList$QC$Input.Parameter$Standards$MM2$DataZielwert            <- read.csv(myList$QC$Input.Parameter$Standards$MM2$PathZielwert,header = T,stringsAsFactors = F)
  
  myList$QC$Input.Parameter$Standards$MM3$QCSoll.731.MM3          <- 8216
  myList$QC$Input.Parameter$Standards$MM3$PathControlChart        <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.1_MM3_QC.csv"),winslash = "\\")
  myList$QC$Input.Parameter$Standards$MM3$PathZielwert            <- normalizePath(paste0(path.TargetValues,"/SAA_7.3.1_MM3_Zielwert.csv"),winslash = "\\")
  myList$QC$Input.Parameter$Standards$MM3$DataZielwert            <- read.csv(myList$QC$Input.Parameter$Standards$MM3$PathZielwert,header = T,stringsAsFactors = F)
  
  QCEval <- myQC.SAA731(myList)
  myList$QC$Control.Chart.Data    <- QCEval$Control.Chart.Data
  myList$QC$Control.Chart.Plots   <- QCEval$Control.Chart.Plots
  
  myList
}

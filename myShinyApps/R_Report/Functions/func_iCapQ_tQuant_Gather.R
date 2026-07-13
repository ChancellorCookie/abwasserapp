iCapQ_tQuant_Gather <- function(myList){
  
  Chromatogram <- myList$tQuant$Chromatography$Chromatogramm$Data
  massTraces <- unique(myList$tQuant$Chromatography$Chromatogramm$Analytes)
  n.massTraces <- length(massTraces)
  
  if(length(Chromatogram)!=0){
    n <- (ncol(Chromatogram) - 2)/(2*n.massTraces)
    chromLabels <- c("counts","time")
    
    massTracesChromLabels <- NULL
    for (i in 1:length(massTraces)) {
      for (j in 1:length(chromLabels)){
        massTracesChromLabels <- c(massTracesChromLabels,paste0(massTraces[i],".",chromLabels[j]))
      }
    }
    
    linseq <- rep(1:n, each=2*n.massTraces)
    numLabels <- paste0(massTracesChromLabels,".",linseq)
    names(Chromatogram) <- c("Index","Labels",numLabels)
    
    SortedChromatogram <- suppressWarnings(dfGather_iCAPQ(Chromatogram,massTracesChromLabels))
    return(SortedChromatogram)
  }
  
}




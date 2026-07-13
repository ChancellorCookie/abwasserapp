my.tQuant.Chromatogramm.Plot <- function(myList){
  
  Analytes <- myList$tQuant$Calibration$Info$Analytes
  SortedChromatogramm <- myList$tQuant$Chromatography$Chromatogramm$SortedChromatogramm
  
  # Welche Massespur?
  PlotMass <- myList$Input.Parameter$Masse # In SAA definiert
  
  # Farbskala für den Plot
  MarkColors <- c(brewer.pal(9,"Set1"),brewer.pal(8,"Accent")) 
  FillColors <- c(brewer.pal(9,"Pastel1"),brewer.pal(8,"Accent")) 
  
  Plot.List <- list()
  
  for (i in 1:nrow(myList$Header)) {
    
    #Extraktion der i-ten Probe und gewählter Massespur
    plotData <- SortedChromatogramm %>% 
      filter(Index %in% myList$Header$Index[i])%>% 
      select(Index,Labels,contains(PlotMass))
    
    # Sortierung der Daten nach der Zeit-Achse
    plotData <- plotData[order(plotData[[2]]),] %>% `names<-`(.,c("Index","Labels","Counts","Time"))
    
    # Aus Rohdaten Extraktion der Peak-Markierungen
    PeakStart <- unlist(myList$tQuant$Chromatography$PeakStart$Data[i,Analytes])
    PeakEnd <- unlist(myList$tQuant$Chromatography$PeakEnd$Data[i,Analytes])
    PeakMax <- unlist(myList$tQuant$Chromatography$Retention$Data[i,Analytes])
    PeakArea <- unlist(myList$tQuant$Chromatography$PeakArea$Data[i,Analytes])
    PeakHight <- unlist(myList$tQuant$Chromatography$PeakHeight$Data[i,Analytes])
    MaxChrom <- max(plotData$Counts)
    
    
    g <- ggplot(plotData,aes(Time, Counts)) +
      geom_line()+
      theme_minimal() + # weißer Hintergrund mit Gittellinien
      labs(y = "Intensität (CPS)", # Achsenbeschriftungen, 
           x = "Time (s)",
           title = "Pt-Spezies",
           subtitle = myList$Header$Labels[i]) 
    
    
    for (j in 1:length(Analytes)) {
      
      if (PeakArea[j] > 0) {
        
        
        # #----  BUG im GEOM! -------
        # # # Peak-Ausfüllen
        # g <- g + geom_area(aes(x=ifelse(
        #   Time > PeakStart[j] & Time < PeakEnd[j], Time,PeakStart[j]),
        #   y=ifelse(
        #     Time > PeakStart[j] & Time < PeakEnd[j],Counts,0)),
        #   fill = FillColors[j])
        
        
        # Start -> End Markierung
        g <- g + annotate("segment", x = PeakStart[j], xend = PeakStart[j], y = 0, yend = MaxChrom*0.1, colour = MarkColors[j],size = 1)
        g <- g + annotate("segment", x = PeakEnd[j], xend = PeakEnd[j], y = 0, yend = MaxChrom*0.1, colour = MarkColors[j],size = 1)
        
        # Komponenten Beschriftung
        g <- g + annotate("text", x = PeakMax[j], y = PeakHight[j]*(1+0.05), label = names(PeakMax)[j])
      }
    }
    
    
    Plot.List[[myList$Header$Labels[i]]] <- g
  }
  
  Plot.List
  
}
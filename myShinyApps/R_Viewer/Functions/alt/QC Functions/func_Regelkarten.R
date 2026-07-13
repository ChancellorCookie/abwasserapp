
#### Graphik ####
myRegelkartenPlot <- function(plotData = plotData, Grenzwert,title = title,yLabel = yLabel,xLabel = xLabel){

  
  plotData <- plotData %>% filter(!is.na(plotData$Value))
  
  
  mw <- mean(plotData$Value,na.rm = T)
  
  OWG <- mw + 2*Grenzwert
  UWG <- mw - 2*Grenzwert
  OKG <- mw + 3*Grenzwert
  UKG <- mw - 3*Grenzwert
  
  ggplot(plotData,aes(Index, Value)) + geom_point(na.rm = T) + geom_line(na.rm = T) +
    
    geom_hline(yintercept = OWG,colour = "blue",linetype="dashed",na.rm = TRUE)+
    geom_hline(yintercept = UWG,colour = "blue",linetype="dashed",na.rm = TRUE)+
    geom_hline(yintercept = OKG,colour = "red",linetype="dashed",na.rm = TRUE)+
    geom_hline(yintercept = UKG,colour = "red",linetype="dashed",na.rm = TRUE)+
    geom_hline(yintercept = mw,colour = "black",na.rm = TRUE)+
    
    
    theme_minimal() +
    labs(y = yLabel,
         x = xLabel,
         title = title)
}

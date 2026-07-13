
#### Graphik ####
myZielwertkartenPlot <- function(plotData = plotData, Zielwert = 100, Unsicherheit = 26 ,title = title,yLabel = yLabel,xLabel = xLabel){

  
  plotData <- plotData %>% filter(!is.na(plotData$Value))

  OKG <- Zielwert + Unsicherheit
  UKG <- Zielwert - Unsicherheit
  
  ggplot(plotData,aes(Index, Value)) + geom_point(na.rm = T) + geom_line(na.rm = T) +
    
    geom_hline(yintercept = OKG,colour = "red",linetype="dashed",na.rm = TRUE)+
    geom_hline(yintercept = UKG,colour = "red",linetype="dashed",na.rm = TRUE)+
    geom_hline(yintercept = Zielwert,colour = "black",na.rm = TRUE)+
    
    
    theme_minimal() +
    labs(y = yLabel,
         x = xLabel,
         title = title)
}


#### Graphik ####
myBlindwertkartenPlot <- function(plotData = plotData, BWKrit=NA,title = "Blindwertkarte",yLabel = "Intensität",xLabel = "Messzeitpunkt"){

  
  plotData <- plotData %>% filter(!is.na(plotData$Value))
  
    ggplot(plotData,aes(Index, Value)) + geom_point(na.rm = T) + geom_line(na.rm = T) +
    
    
    geom_hline(yintercept = BWKrit,colour = "red",linetype="dashed",na.rm = TRUE)+
    scale_y_continuous(expand = c(0, 0), limits = c(0,max(c(max(plotData$Value),BWKrit)*1.3))) +
    theme_minimal() +
    labs(y = yLabel,
         x = xLabel,
         title = title)
}

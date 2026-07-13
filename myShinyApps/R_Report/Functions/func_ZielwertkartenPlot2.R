
#### Graphik ####
myZielwertkartenPlot2 <- function(datapath_df, datapath_Value, title = "Zielwertkarte", xLabel = "Messzeitpunkt", yLabel = "Wiederfindungsrate (%)"){

  
  ### Einlesen gespeicherter Daten
  QC <- read.csv(datapath_df,header = TRUE)
  Zielwert <- read.csv(datapath_Value,header = T)
  
  ### Konversion des Datumformats
  QC$StartTime %<>% strptime(format ="%Y-%m-%d %H:%M:%S") %<>% as.POSIXct()

  OKG <- Zielwert$Zielwert + Zielwert$Unsicherheit
  UKG <- Zielwert$Zielwert - Zielwert$Unsicherheit
  
  ggplot(QC,aes(StartTime, WFR)) + geom_point(na.rm = T) + geom_line(na.rm = T) +
    
    geom_hline(yintercept = OKG,colour = "red",linetype="dashed",na.rm = TRUE)+
    geom_hline(yintercept = UKG,colour = "red",linetype="dashed",na.rm = TRUE)+
    geom_hline(yintercept = Zielwert$Zielwert,colour = "black",na.rm = TRUE)+
    
    
    theme_minimal() +
    labs(y = yLabel,
         x = xLabel,
         title = title)
}

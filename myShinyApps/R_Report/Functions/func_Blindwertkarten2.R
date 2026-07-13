
#### Graphik ####
myBlindwertkartenPlot2 <- function(datapath_df, datapath_Value,InputVar = 1){
  
  
  ### Einlesen gespeicherter Daten
  VBW <- read.csv(datapath_df,header = TRUE)
  BWKrit <- read.csv(datapath_Value,header = T)
  
  
  yData <- VBW$Conentration
  yLabel <- "Konzentration (ng/L)"
  xLabel <- "Messzeitpunkt"
  title = "Blindwertkarte"
  
  
  ### Konversion des Datumformats
  VBW$StartTime %<>% strptime(format ="%Y-%m-%d %H:%M:%S") %<>% as.POSIXct()
  
  ggplot(VBW,aes(StartTime,Concentration)) + geom_point(na.rm = T) + geom_line(na.rm = T) +
    
    
    geom_hline(yintercept = BWKrit$LimitBG,colour = "red",linetype="dashed",na.rm = TRUE)+
    
    #scale_y_continuous(limits = c(min(c(max(VBW$Concentration),BWKrit$LimitBG)*1.3),max(c(max(VBW$Concentration),BWKrit$LimitBG)*1.3))) +
    theme_minimal() +
    labs(y = yLabel,
         x = xLabel,
         title = title)
}

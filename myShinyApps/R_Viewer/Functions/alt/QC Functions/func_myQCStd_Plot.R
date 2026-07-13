myQCStd <- function(datapath_df, datapath_Value, Plottitle = "Zielwertkarte", xLabel = "Messzeitpunkt", yLabel = "Wiederfindungsrate (%)"){
  
  datapath_df <- "\\\\10.1.8.210\\gc-ms-fid\\Analysen\\2018\\Berichte\\iCAPQ Messdaten\\Pt Spezies\\QC\\Pt-Spezies_Cis_QCMM3.csv"
  datapath_Value <- "\\\\10.1.8.210\\gc-ms-fid\\Analysen\\2018\\Berichte\\iCAPQ Messdaten\\Pt Spezies\\QC\\Pt-Spezies_ZielwertMM3.csv"
  
  QC <- read.csv(datapath_df,header = TRUE)
  Zielwert <- read.csv(datapath_Value,header = T)
  
  ### Konversion des Datumformats
  QC$StartTime %<>% strptime(format ="%Y-%m-%d %H:%M:%S") %<>% as.POSIXct()
  
  ### Erstellung des Plots
  myZielwertkartenPlot(plotData = data.frame("Index"=QC$StartTime,"Value" = QC$WFR),
                       Zielwert = Zielwert$Zielwert,
                       Unsicherheit = Zielwert$Unsicherheit,
                       title = Plottitle,
                       yLabel = yLabel,
                       xLabel = xLabel)
  
  
}
myQC.SAA732 <- function(myList){
  
  
  QC.Cis.VBWtowrite <- myQCHandling_iCapQ_tQuant(myList = myList,
                                                 QCFilter = "^M.+?QC VBW",
                                                 ColumnFilter = "Cis",
                                                 Handling = "VBW",
                                                 FileToWrite = myList$Input.Parameter$PathRegelKarte.Cis.VBW,
                                                 TargetValue = NA,
                                                 KillOld = F)
  
  QC.Cis.MM3towrite <- myQCHandling_iCapQ_tQuant(myList = myList,
                                                 QCFilter = "^M.+?QC MM3",
                                                 ColumnFilter = "Cis",
                                                 Handling = "QC",
                                                 FileToWrite = myList$Input.Parameter$PathRegelKarte.Cis.QC,
                                                 TargetValue = myList$Input.Parameter$QCSoll.Cis,
                                                 KillOld = F)
  
  QC.Carbo.VBWtowrite <- myQCHandling_iCapQ_tQuant(myList = myList,
                                                   QCFilter = "^M.+?QC VBW",
                                                   ColumnFilter = "Carbo",
                                                   Handling = "VBW",
                                                   FileToWrite = myList$Input.Parameter$PathRegelKarte.Carbo.VBW,
                                                   TargetValue = NA,
                                                   KillOld = F)
  
  QC.Carbo.MM3towrite <- myQCHandling_iCapQ_tQuant(myList = myList,
                                                   QCFilter = "^M.+?QC MM3",
                                                   ColumnFilter = "Carbo",
                                                   Handling = "QC",
                                                   FileToWrite = myList$Input.Parameter$PathRegelKarte.Carbo.QC,
                                                   TargetValue = myList$Input.Parameter$QCSoll.Carbo,
                                                   KillOld = F)
  
  QC.Oxali.VBWtowrite <- myQCHandling_iCapQ_tQuant(myList = myList,
                                                   QCFilter = "^M.+?QC VBW",
                                                   ColumnFilter = "Oxali",
                                                   Handling = "VBW",
                                                   FileToWrite = myList$Input.Parameter$PathRegelKarte.Oxali.VBW,
                                                   TargetValue = NA,
                                                   KillOld = F)
  
  QC.Oxali.MM3towrite <- myQCHandling_iCapQ_tQuant(myList = myList,
                                                   QCFilter = "^M.+?QC MM3",
                                                   ColumnFilter = "Oxali",
                                                   Handling = "QC",
                                                   FileToWrite = myList$Input.Parameter$PathRegelKarte.Oxali.QC,
                                                   TargetValue = myList$Input.Parameter$QCSoll.Oxali,
                                                   KillOld = F)
  
  
  
  
  plotData <- data.frame("Index"=QC.Cis.VBWtowrite$StartTime,"Value" = QC.Cis.VBWtowrite$Concentration)
  
  Cis.VBW.Plot <- myBlindwertkartenPlot(plotData = plotData,
                                        BWKrit = myList$Input.Parameter$Zielwerte$`Cis-Platin`$LimitBG,
                                        title = "Cis-Platin Blindwert-Karte",
                                        yLabel = "Konzentration (µg/L)",
                                        xLabel = "Messzeitpunkt")
  
  
  
  
  plotData <- data.frame("Index"=QC.Carbo.VBWtowrite$StartTime,"Value" = QC.Carbo.VBWtowrite$Concentration)
  
  Carbo.VBW.Plot <- myBlindwertkartenPlot(plotData = plotData,
                                          BWKrit = myList$Input.Parameter$Zielwerte$`Carbo-Platin`$LimitBG,
                                          title = "Carbo-Platin Blindwert-Karte",
                                          yLabel = "Konzentration (µg/L)",
                                          xLabel = "Messzeitpunkt")
  
  plotData <- data.frame("Index"=QC.Oxali.VBWtowrite$StartTime,"Value" = QC.Oxali.VBWtowrite$Concentration)
  
  Oxali.VBW.Plot <- myBlindwertkartenPlot(plotData = plotData,
                                          BWKrit = myList$Input.Parameter$Zielwerte$`Oxali-Platin`$LimitBG,
                                          title = "Oxali-Platin Blindwert-Karte",
                                          yLabel = "Konzentration (µg/L)",
                                          xLabel = "Messzeitpunkt")
  
  
  plotData <- data.frame("Index"=QC.Cis.MM3towrite$StartTime,"Value" = QC.Cis.MM3towrite$WFR)
  
  Cis.QC.Plot <- myZielwertkartenPlot(plotData = plotData,
                                      Zielwert = myList$Input.Parameter$Zielwerte$`Cis-Platin`$Zielwert,
                                      Unsicherheit = myList$Input.Parameter$Zielwerte$`Cis-Platin`$Unsicherheit,
                                      title = "Cis-Platin Zielwert-Karte",
                                      yLabel = "Wiederfindungsrate (%)",
                                      xLabel = "Messzeitpunkt")
  
  plotData <- data.frame("Index"=QC.Carbo.MM3towrite$StartTime,"Value" = QC.Carbo.MM3towrite$WFR)
  
  Carbo.QC.Plot <- myZielwertkartenPlot(plotData = plotData,
                                        Zielwert = myList$Input.Parameter$Zielwerte$`Carbo-Platin`$Zielwert,
                                        Unsicherheit = myList$Input.Parameter$Zielwerte$`Carbo-Platin`$Unsicherheit,
                                        title = "Carbo-Platin Zielwert-Karte",
                                        yLabel = "Wiederfindungsrate (%)",
                                        xLabel = "Messzeitpunkt")
  
  plotData <- data.frame("Index"=QC.Oxali.MM3towrite$StartTime,"Value" = QC.Oxali.MM3towrite$WFR)
  
  Oxali.QC.Plot <- myZielwertkartenPlot(plotData = plotData,
                                        Zielwert = myList$Input.Parameter$Zielwerte$`Oxali-Platin`$Zielwert,
                                        Unsicherheit = myList$Input.Parameter$Zielwerte$`Oxali-Platin`$Unsicherheit,
                                        title = "Oxali-Platin Zielwert-Karte",
                                        yLabel = "Wiederfindungsrate (%)",
                                        xLabel = "Messzeitpunkt")
  
  
  
  
  
  list("TargetValues" = list("Akzeptanzkriterien" = myList$Input.Parameter$Zielwerte,
                             "Path.TargetValues.MM2" = myList$Input.Parameter$PathZielwert.Cis,
                             "Path.TargetValues.MM3" = myList$Input.Parameter$PathZielwert.Carbo,
                             "Path.TargetValues.MM3" = myList$Input.Parameter$PathZielwert.Oxali),
       
       "QC.Definition" = list("Cis-Platin" = myList$Input.Parameter$QCSoll.Cis,
                              "Carbo-Platin" = myList$Input.Parameter$QCSoll.Carbo,
                              "Oxali-Platin" = myList$Input.Parameter$QCSoll.Oxali),
       
       "Control.Chart.Data" = list("VBW.Cis" = QC.Cis.VBWtowrite,
                                   "VBW.Carbo" = QC.Carbo.VBWtowrite,
                                   "VBW.Oxali" = QC.Oxali.VBWtowrite,
                                   "Path.VBW.Cis" = myList$Input.Parameter$PathRegelKarteVBW,
                                   "Path.VBW.Carbo" = myList$Input.Parameter$PathRegelKarteMM2,
                                   "Path.VBW.Oxali" = myList$Input.Parameter$PathRegelKarteMM3,
                                   "QC.Cis" = QC.Cis.MM3towrite,
                                   "QC.Carbo" = QC.Cis.MM3towrite,
                                   "QC.Oxali" = QC.Cis.MM3towrite,
                                   "Path.QC.Cis" = myList$Input.Parameter$PathRegelKarteVBW,
                                   "Path.QC.Carbo" = myList$Input.Parameter$PathRegelKarteMM2,
                                   "Path.QC.Oxali" = myList$Input.Parameter$PathRegelKarteMM3),
       "Control.Chart.Plots" = list("VBW.Cis" = Cis.VBW.Plot,
                                    "VBW.Carbo" = Carbo.VBW.Plot,
                                    "VBW.Oxali" = Oxali.VBW.Plot,
                                    "QC.Cis" = Cis.QC.Plot,
                                    "QC.Carbo" = Carbo.QC.Plot,
                                    "QC.Oxali" = Oxali.QC.Plot))
  
}


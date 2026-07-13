myQC.SAA732 <- function(myList){
  
  ### Automatisierung möglich!
  ### Problem:  Für 60 Analyten nicht anwendbar!
  ### Lösung:   For-Loop mit variabler Eingabe der Komponente.
  ###           VBW wird auch als QC behandelt 
  
  QC.Cis.VBWtowrite <- myQCHandling_iCapQ_tQuant(myList = myList,
                                                 QCFilter = "^M.+?QC VBW",
                                                 ColumnFilter = "Cis",
                                                 Handling = "VBW",
                                                 FileToWrite = myList$QC$Input.Parameter$Blanks$CisPt.VBW$PathControlChart,
                                                 TargetValue = NA,
                                                 KillOld = F)
  
  QC.Cis.MM3towrite <- myQCHandling_iCapQ_tQuant(myList = myList,
                                                 QCFilter = "^M.+?QC MM3",
                                                 ColumnFilter = "Cis",
                                                 Handling = "QC",
                                                 FileToWrite = myList$QC$Input.Parameter$Standards$CisPt.QC$PathControlChart,
                                                 TargetValue = myList$QC$Input.Parameter$Standards$CisPt.QC$QCSoll.732.Cis,
                                                 KillOld = F)
  
  QC.Carbo.VBWtowrite <- myQCHandling_iCapQ_tQuant(myList = myList,
                                                   QCFilter = "^M.+?QC VBW",
                                                   ColumnFilter = "Carbo",
                                                   Handling = "VBW",
                                                   FileToWrite = myList$QC$Input.Parameter$Blanks$CarboPt.VBW$PathControlChart,
                                                   TargetValue = NA,
                                                   KillOld = F)
  
  QC.Carbo.MM3towrite <- myQCHandling_iCapQ_tQuant(myList = myList,
                                                   QCFilter = "^M.+?QC MM3",
                                                   ColumnFilter = "Carbo",
                                                   Handling = "QC",
                                                   FileToWrite = myList$QC$Input.Parameter$Standards$CarboPt.QC$PathControlChart,
                                                   TargetValue = myList$QC$Input.Parameter$Standards$CarboPt.QC$QCSoll.732.Carbo,
                                                   KillOld = F)
  
  QC.Oxali.VBWtowrite <- myQCHandling_iCapQ_tQuant(myList = myList,
                                                   QCFilter = "^M.+?QC VBW",
                                                   ColumnFilter = "Oxali",
                                                   Handling = "VBW",
                                                   FileToWrite = myList$QC$Input.Parameter$Blanks$OxaliPt.VBW$PathControlChart,
                                                   TargetValue = NA,
                                                   KillOld = F)
  
  QC.Oxali.MM3towrite <- myQCHandling_iCapQ_tQuant(myList = myList,
                                                   QCFilter = "^M.+?QC MM3",
                                                   ColumnFilter = "Oxali",
                                                   Handling = "QC",
                                                   FileToWrite = myList$QC$Input.Parameter$Standards$OxaliPt.QC$PathControlChart,
                                                   TargetValue = myList$QC$Input.Parameter$Standards$OxaliPt.QC$QCSoll.732.Oxali,
                                                   KillOld = F)
  
  
  
  
  plotData <- data.frame("Index"=QC.Cis.VBWtowrite$StartTime,"Value" = QC.Cis.VBWtowrite$Concentration)
  
  Cis.VBW.Plot <- myBlindwertkartenPlot(plotData = plotData,
                                        BWKrit = myList$QC$Input.Parameter$Blanks$CisPt.VBW$DataZielwert$LimitBG,
                                        title = "Cis-Platin Blindwert-Karte",
                                        yLabel = "Konzentration (µg/L)",
                                        xLabel = "Messzeitpunkt")
  
  
  
  
  plotData <- data.frame("Index"=QC.Carbo.VBWtowrite$StartTime,"Value" = QC.Carbo.VBWtowrite$Concentration)
  
  Carbo.VBW.Plot <- myBlindwertkartenPlot(plotData = plotData,
                                          BWKrit = myList$QC$Input.Parameter$Blanks$CarboPt.VBW$DataZielwert$LimitBG,
                                          title = "Carbo-Platin Blindwert-Karte",
                                          yLabel = "Konzentration (µg/L)",
                                          xLabel = "Messzeitpunkt")
  
  plotData <- data.frame("Index"=QC.Oxali.VBWtowrite$StartTime,"Value" = QC.Oxali.VBWtowrite$Concentration)
  
  Oxali.VBW.Plot <- myBlindwertkartenPlot(plotData = plotData,
                                          BWKrit = myList$QC$Input.Parameter$Blanks$OxaliPt.VBW$DataZielwert$LimitBG,
                                          title = "Oxali-Platin Blindwert-Karte",
                                          yLabel = "Konzentration (µg/L)",
                                          xLabel = "Messzeitpunkt")
  
  
  plotData <- data.frame("Index"=QC.Cis.MM3towrite$StartTime,"Value" = QC.Cis.MM3towrite$WFR)
  
  Cis.QC.Plot <- myZielwertkartenPlot(plotData = plotData,
                                      Zielwert = myList$QC$Input.Parameter$Standards$CisPt.QC$DataZielwert$Zielwert,
                                      Unsicherheit = myList$QC$Input.Parameter$Standards$CisPt.QC$DataZielwert$Unsicherheit,
                                      title = "Cis-Platin Zielwert-Karte",
                                      yLabel = "Wiederfindungsrate (%)",
                                      xLabel = "Messzeitpunkt")
  
  plotData <- data.frame("Index"=QC.Carbo.MM3towrite$StartTime,"Value" = QC.Carbo.MM3towrite$WFR)
  
  Carbo.QC.Plot <- myZielwertkartenPlot(plotData = plotData,
                                        Zielwert = myList$QC$Input.Parameter$Standards$CarboPt.QC$DataZielwert$Zielwert,
                                        Unsicherheit = myList$QC$Input.Parameter$Standards$CarboPt.QC$DataZielwert$Unsicherheit,
                                        title = "Carbo-Platin Zielwert-Karte",
                                        yLabel = "Wiederfindungsrate (%)",
                                        xLabel = "Messzeitpunkt")
  
  plotData <- data.frame("Index"=QC.Oxali.MM3towrite$StartTime,"Value" = QC.Oxali.MM3towrite$WFR)
  
  Oxali.QC.Plot <- myZielwertkartenPlot(plotData = plotData,
                                        Zielwert = myList$QC$Input.Parameter$Standards$OxaliPt.QC$DataZielwert$Zielwert,
                                        Unsicherheit = myList$QC$Input.Parameter$Standards$OxaliPt.QC$DataZielwert$Unsicherheit,
                                        title = "Oxali-Platin Zielwert-Karte",
                                        yLabel = "Wiederfindungsrate (%)",
                                        xLabel = "Messzeitpunkt")
  
  
  
  
  
  list("Control.Chart.Data" = list("VBW.Cis" = QC.Cis.VBWtowrite,
                                   "VBW.Carbo" = QC.Carbo.VBWtowrite,
                                   "VBW.Oxali" = QC.Oxali.VBWtowrite,
                                   "QC.Cis" = QC.Cis.MM3towrite,
                                   "QC.Carbo" = QC.Carbo.MM3towrite,
                                   "QC.Oxali" = QC.Oxali.MM3towrite),
       "Control.Chart.Plots" = list("VBW.Cis" = Cis.VBW.Plot,
                                    "VBW.Carbo" = Carbo.VBW.Plot,
                                    "VBW.Oxali" = Oxali.VBW.Plot,
                                    "QC.Cis" = Cis.QC.Plot,
                                    "QC.Carbo" = Carbo.QC.Plot,
                                    "QC.Oxali" = Oxali.QC.Plot))
  
}


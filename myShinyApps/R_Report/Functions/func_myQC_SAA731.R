myQC.SAA731 <- function(myList){

  QCVBWtowrite <- myQCHandling_iCapQ_eQuant(myList = myList,
                                            QCFilter = "^M.+?QC VBW",
                                            ColumnFilter = myList$Input.Parameter$Masse,
                                            Handling = "VBW",
                                            FileToWrite = myList$Input.Parameter$PathRegelKarteVBW,
                                            TargetValue = NA,
                                            KillOld = F)
  
  QCMM2towrite <- myQCHandling_iCapQ_eQuant(myList = myList,
                                            QCFilter = "^M.+?QC MM2",
                                            ColumnFilter = myList$Input.Parameter$Masse,
                                            Handling = "QC",
                                            FileToWrite = myList$Input.Parameter$PathRegelKarteMM2,
                                            TargetValue = myList$Input.Parameter$QCSoll_731_MM2,
                                            KillOld = F)
  
  QCMM3towrite <- myQCHandling_iCapQ_eQuant(myList = myList,
                                            QCFilter = "^M.+?QC MM3",
                                            ColumnFilter = myList$Input.Parameter$Masse,
                                            Handling = "QC",
                                            FileToWrite = myList$Input.Parameter$PathRegelKarteMM3,
                                            TargetValue = myList$Input.Parameter$QCSoll_731_MM3,
                                            KillOld = F)
  
  
  plotData.VBW <- data.frame("Index"=QCVBWtowrite$StartTime,"Value" = QCVBWtowrite$Concentration)
  
  VBWPlot <- myBlindwertkartenPlot(plotData = plotData.VBW,
                                   BWKrit = myList$Input.Parameter$ZielwertMM2$LimitBG,
                                   title = "Blindwert-Regelkarte",
                                   yLabel = "Konzentration (ng/L)",
                                   xLabel = "Messzeitpunkt")
  
  plotData.MM2 <- data.frame("Index"=QCMM2towrite$StartTime,"Value" = QCMM2towrite$WFR)
  
  MM2Plot <- myZielwertkartenPlot(plotData = plotData.MM2,
                                  Zielwert = myList$Input.Parameter$ZielwertMM2$Zielwert,
                                  Unsicherheit = myList$Input.Parameter$ZielwertMM2$Unsicherheit,
                                  title = "MM2-Zielwert-Karte",
                                  yLabel = "Wiederfindungsrate (%)",
                                  xLabel = "Messzeitpunkt")
  
  plotData.MM3 <- data.frame("Index"=QCMM3towrite$StartTime,"Value" = QCMM3towrite$WFR)
  
  MM3Plot <- myZielwertkartenPlot(plotData = plotData.MM3,
                                  Zielwert = myList$Input.Parameter$ZielwertMM3$Zielwert,
                                  Unsicherheit = myList$Input.Parameter$ZielwertMM3$Unsicherheit,
                                  title = "MM3-Zielwert-Karte",
                                  yLabel = "Wiederfindungsrate (%)",
                                  xLabel = "Messzeitpunkt")
  
  
  
  list("TargetValues" = list("VBW" = myList$Input.Parameter$ZielwertMM2$LimitBG,
                             "MM2.Target" = myList$Input.Parameter$ZielwertMM2$Zielwert,
                             "MM2.Uncertainty" = myList$Input.Parameter$ZielwertMM2$Unsicherheit,
                             "MM3.Target" = myList$Input.Parameter$ZielwertMM3$Zielwert,
                             "MM3.Uncertainty" = myList$Input.Parameter$ZielwertMM3$Unsicherheit,
                             "Path.TargetValues.MM2" = myList$Input.Parameter$PathZielwertMM2,
                             "Path.TargetValues.MM3" = myList$Input.Parameter$PathZielwertMM3),
       
       "QC.Definition" = list("MM2" = myList$Input.Parameter$QCSoll_731_MM2,
                              "MM3" = myList$Input.Parameter$QCSoll_731_MM3),
       
       "Control.Chart.Data" = list("VBW" = QCVBWtowrite,
                                   "MM2" = QCMM2towrite,
                                   "MM3" = QCMM3towrite,
                                   "Path.VBW" = myList$Input.Parameter$PathRegelKarteVBW,
                                   "Path.MM2" = myList$Input.Parameter$PathRegelKarteMM2,
                                   "Path.MM3" = myList$Input.Parameter$PathRegelKarteMM3),
       "Control.Chart.Plots" = list("VBW" = VBWPlot,
                                    "MM2" = MM2Plot,
                                    "MM3" = MM3Plot))
  
}


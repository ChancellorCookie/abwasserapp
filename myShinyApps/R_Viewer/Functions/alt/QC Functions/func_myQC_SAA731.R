myQC.SAA731 <- function(myList){
  # Extraction of QC relevant Data from samplelist
  QCVBWtowrite <- myQCHandling_iCapQ_eQuant(myList = myList,
                                            QCFilter = "^M.+?QC VBW",
                                            ColumnFilter = myList$Input.Parameter$Masse,
                                            Handling = "VBW",
                                            FileToWrite = myList$QC$Input.Parameter$Blanks$VBW$PathControlChart,
                                            TargetValue = NA,
                                            KillOld = F)
  
  QCMM2towrite <- myQCHandling_iCapQ_eQuant(myList = myList,
                                            QCFilter = "^M.+?QC MM2",
                                            ColumnFilter = myList$Input.Parameter$Masse,
                                            Handling = "QC",
                                            FileToWrite = myList$QC$Input.Parameter$Standards$MM2$PathControlChart,
                                            TargetValue = myList$QC$Input.Parameter$Standards$MM2$QCSoll.731.MM2,
                                            KillOld = F)
  
  QCMM3towrite <- myQCHandling_iCapQ_eQuant(myList = myList,
                                            QCFilter = "^M.+?QC MM3",
                                            ColumnFilter = myList$Input.Parameter$Masse,
                                            Handling = "QC",
                                            FileToWrite = myList$QC$Input.Parameter$Standards$MM3$PathControlChart ,
                                            TargetValue = myList$QC$Input.Parameter$Standards$MM3$QCSoll.731.MM3,
                                            KillOld = F)
  
  
  plotData.VBW <- data.frame("Index"=QCVBWtowrite$StartTime,"Value" = QCVBWtowrite$Concentration)
  
  VBWPlot <- myBlindwertkartenPlot(plotData = plotData.VBW,
                                   BWKrit = myList$QC$Input.Parameter$Blanks$VBW$DataZielwert$LimitBG,
                                   title = "Blindwert-Regelkarte",
                                   yLabel = "Konzentration (ng/L)",
                                   xLabel = "Messzeitpunkt")
  
  plotData.MM2 <- data.frame("Index"=QCMM2towrite$StartTime,"Value" = QCMM2towrite$WFR)
  
  MM2Plot <- myZielwertkartenPlot(plotData = plotData.MM2,
                                  Zielwert = myList$QC$Input.Parameter$Standards$MM2$DataZielwert$Zielwert,
                                  Unsicherheit = myList$QC$Input.Parameter$Standards$MM2$DataZielwert$Unsicherheit,
                                  title = "MM2-Zielwert-Karte",
                                  yLabel = "Wiederfindungsrate (%)",
                                  xLabel = "Messzeitpunkt")
  
  plotData.MM3 <- data.frame("Index"=QCMM3towrite$StartTime,"Value" = QCMM3towrite$WFR)
  
  MM3Plot <- myZielwertkartenPlot(plotData = plotData.MM3,
                                  Zielwert = myList$QC$Input.Parameter$Standards$MM3$DataZielwert$Zielwert,
                                  Unsicherheit = myList$QC$Input.Parameter$Standards$MM3$DataZielwert$Unsicherheit,
                                  title = "MM3-Zielwert-Karte",
                                  yLabel = "Wiederfindungsrate (%)",
                                  xLabel = "Messzeitpunkt")
  
  
  
  list("Control.Chart.Data" = list("VBW" = QCVBWtowrite,
                                   "MM2" = QCMM2towrite,
                                   "MM3" = QCMM3towrite),
       "Control.Chart.Plots" = list("VBW" = VBWPlot,
                                    "MM2" = MM2Plot,
                                    "MM3" = MM3Plot))
  
}


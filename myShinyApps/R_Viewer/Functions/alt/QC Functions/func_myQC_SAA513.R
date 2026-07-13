myQC.SAA513 <- function(myList){

  ## Extract the QC Information, calculate WFR and store in list ####
  #
  QC <- list("VBW.Zelle0" = myDMA80.QC(df = myList$RawData$RawClean %>% filter(Cell %in% 0),
                                       QC.filter = "VBW",
                                       TargetValue = myList$QC$Input.Parameter$Standards$NIST1632d$QCSoll.513),
             "QC.All" = myDMA80.QC(df = myList$RawData$RawClean,
                                   QC.filter = "^NIST.+?[[:digit:]].+?[[:lower:]]",
                                   TargetValue = myList$QC$Input.Parameter$Standards$NIST1632d$QCSoll.513),
             "QC.Zelle0" = myDMA80.QC(df = myList$RawData$RawClean %>% filter(Cell %in% 0),
                                      QC.filter = "^NIST.+?[[:digit:]].+?[[:lower:]]",
                                      TargetValue = myList$QC$Input.Parameter$Standards$NIST1632d$QCSoll.513),
             "QC.Zelle1" =  myDMA80.QC(df = myList$RawData$RawClean %>% filter(Cell %in% 1),
                                       QC.filter = "^NIST.+?[[:digit:]].+?[[:lower:]]",
                                       TargetValue = myList$QC$Input.Parameter$Standards$NIST1632d$QCSoll.513),
             "QC.Zelle2" =  myDMA80.QC(df = myList$RawData$RawClean %>% filter(Cell %in% 2),
                                       QC.filter = "^NIST.+?[[:digit:]].+?[[:lower:]]",
                                       TargetValue = myList$QC$Input.Parameter$Standards$NIST1632d$QCSoll.513))
  
  
  
  ## Write the QC Data on a Charttable ####
  
  #### VBW ###
  QCVBWtowrite <- myQCHandling_DMA80(QC_DF = QC$VBW.Zelle0$QC.Data,
                                     Handling = "VBW",
                                     PathToWrite = myList$QC$Input.Parameter$Blanks$VBW$PathControlChart,
                                     KillOld = F)
  
  ### QC ###
  QCNISTZ0towrite <- myQCHandling_DMA80(QC_DF = QC$QC.Zelle0$QC.Data,
                                 Handling = "QC",
                                 PathToWrite = myList$QC$Input.Parameter$Standards$NIST1632dZ0$PathControlChart,
                                 KillOld = F)
  
  
  QCNISTZ1towrite <- myQCHandling_DMA80(QC_DF = QC$QC.Zelle1$QC.Data,
                                 Handling = "QC",
                                 PathToWrite = myList$QC$Input.Parameter$Standards$NIST1632dZ1$PathControlChart,
                                 KillOld = F)
  
  
  
  ## Plot the ChartTable ####
  
  plotVBW  <- myBlindwertkartenPlot(plotData = data.frame("Index"=QCVBWtowrite$StartTime,
                                                          "Value" = QCVBWtowrite$Hg..ng.),
                                    BWKrit = myList$QC$Input.Parameter$Blanks$VBW$DataZielwert$LimitBG/100,
                                    title = "Blindwertkarte",
                                    yLabel = "absolute Konzentration (ng)",
                                    xLabel = "Messzeitpunkt")
  
  
  plotNISTZ0  <- myZielwertkartenPlot(plotData = data.frame("Index"=QCNISTZ0towrite$StartTime,
                                                            "Value" = QCNISTZ0towrite$WFR),
                                      Zielwert = myList$QC$Input.Parameter$Standards$NIST1632d$DataZielwert$Zielwert,
                                      Unsicherheit = myList$QC$Input.Parameter$Standards$NIST1632d$DataZielwert$Unsicherheit,
                                      title = "Zielwertkarte Zelle 0",
                                      yLabel = "Wiederfindungsrate (%)",
                                      xLabel = "Messzeitpunkt")
  
  plotNISTZ1  <- myZielwertkartenPlot(plotData = data.frame("Index"=QCNISTZ1towrite$StartTime,
                                                            "Value" = QCNISTZ1towrite$WFR),
                                      Zielwert = myList$QC$Input.Parameter$Standards$NIST1632d$DataZielwert$Zielwert,
                                      Unsicherheit = myList$QC$Input.Parameter$Standards$NIST1632d$DataZielwert$Unsicherheit,
                                      title = "Zielwertkarte Zelle 1",
                                      yLabel = "Wiederfindungsrate (%)",
                                      xLabel = "Messzeitpunkt")
  
  
  
  list("Control.Chart.Data" = list("VBW" = QCVBWtowrite,
                                   "NISTZ0" = QCNISTZ0towrite,
                                   "NISTZ1" = QCNISTZ1towrite),
       "Control.Chart.Plots" = list("VBW" = plotVBW,
                                    "NISTZ0" = plotNISTZ0,
                                    "NISTZ1" = plotNISTZ1))
  
}


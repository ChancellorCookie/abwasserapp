my.tQuant.Concentration.Evaluation <- function(myList = myList){
  ExtCal.which.Analytes <- myList$tQuant$Chromatography$Concentration$Analytes
  ExtCal.which.Units <- myUnitsFormat(myList$tQuant$Chromatography$Concentration$Units)
  SampleDF <-myList$tQuant$Chromatography$PeakArea$Data[c("Index","Labels",ExtCal.which.Analytes)]
  StdDef <- myList$tQuant$Calibration$Std.Concentration
  StdIndex <- StdDef$Index
  
  
  SmplCaliAssign.Average <- mySmplCaliAssign(SampleDF,
                                             StdIndex,
                                             ExtCal.which.Analytes,
                                             OutOfRange.Factor = 1.2)
  
  ### Linear Regression (OLS)
  
  
  
  # Ausreißer Tests
  
  
  
  # Concentrationen
  Corr.Average.Conc <- myConcentration(SampleDF,
                                       StdDef,
                                       SmplCaliAssign.Average$CalIndex,
                                       ExtCal.which.Analytes)
  # Store in myList
  myList$tQuant[["Evaluation"]] <- Corr.Average.Conc
  
  # Calibration Plots
  Corr.Average.Plots <- myCaliGraph.list(Analytes = ExtCal.which.Analytes,
                                         Units = "µg/L",
                                         Corr.Average = myList$tQuant$Calibration$Std.PeakArea,
                                         StdDef = myList$tQuant$Calibration$Std.Concentration,
                                         StdIndex = StdDef$Index)
  
  # Calibration Statistics
  OLS <- myOLS(myList)
  
  
  BG.vec <- myOLS_BG(OLS,"xBG")
  # Extract BG Values
  if (myList$Input.Parameter$BG.SAA) {  ## wenn die Bestimmungsgrenze im Report mit den Akzeptanzkriterien der SAA abgeglichen werden soll
    for (v in ExtCal.which.Analytes) {
      Zielwert <- myList$Input.Parameter$Zielwerte[[v]]
      if (BG.vec[v] < Zielwert$LimitBG) { # Prüfung, ob die erreichte BG kleiner der Akzeptanz BG ist.
        BG.vec[v] <- Zielwert$LimitBG
      }
    }
  }
 
  # Replace concentration < BG
  Corr.BG.Conc <- myConc.less.BG(Corr.Average.Conc$Concentration,BG.vec)
  
  # Evaluate conc * dilution factor
  Corr.VF.Conc <- myConc.multiply.VF(Corr.BG.Conc,myList$Header %>% 
                                       select(Index,Labels,`Dilution Factor`))
  
  
  #### OUTPUT für eQuant ####
  
  myList$tQuant$Calibration[["Info"]] <- list("Analytes" = ExtCal.which.Analytes,
                                              "Units" = ExtCal.which.Units)
  myList$tQuant$Calibration[["CalIndex"]] <- SmplCaliAssign.Average$CalIndex
  myList$tQuant$Calibration[["OutOfRange"]] <- SmplCaliAssign.Average$OutOfRange
  myList$tQuant$Calibration[["Cali.Plots"]] <- Corr.Average.Plots
  myList$tQuant$Calibration[["OLS"]] <- OLS
  
  Corr.Final <- myConcFinal(conc = Corr.VF.Conc,
                            Analytes = ExtCal.which.Analytes,
                            SignifDigits = myList$Input.Parameter$Significant.Digits,
                            BGs = BG.vec)
  
  myList$tQuant$Evaluation[["Conc.less.BG"]] <- Corr.BG.Conc
  myList$tQuant$Evaluation[["BG.Report"]] <- BG.vec
  myList$tQuant$Evaluation[["Conc.multiply.VF"]] <- Corr.VF.Conc
  myList$tQuant$Evaluation[["Conc.Final"]] <- Corr.Final
  
  
  
  myList
  
}
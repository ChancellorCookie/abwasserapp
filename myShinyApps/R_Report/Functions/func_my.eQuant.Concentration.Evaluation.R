my.eQuant.Concentration.Evaluation <- function(myList = myList){
  ExtCal.which.Analytes <- myList$eQuant$ExtCal$Which.Analytes$Analytes
  SampleDF <-myList$eQuant$Corrected.Data$Corr.Average[c("Index","Labels",ExtCal.which.Analytes)]
  StdDef <- myList$eQuant$Std.Concentration
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
  myList$eQuant[["Evaluation"]] <- Corr.Average.Conc
  
  
  # Calibration Plots
  Corr.Average.Plots <- myCaliGraph.list(Analytes = myList$eQuant$ExtCal$Which.Analytes$Analytes,
                                         Units = unique(myList$eQuant$ExtCal$Which.Analytes$Units),
                                         Corr.Average = myList$eQuant$Corrected.Data$Corr.Average,
                                         StdDef = myList$eQuant$Std.Concentration,
                                         StdIndex = StdDef$Index)
  
  # Calibration Statistics
  OLS <- myOLS(myList)
  BG.vec <- myOLS_BG(OLS,"xBG")
  # Extract BG Values
  if (myList$Input.Parameter$BG.SAA) {  ## wenn die Bestimmungsgrenze im Report mit den Akzeptanzkriterien der SAA abgeglichen werden soll
    if (BG.vec[str_detect(names(BG.vec),myList$Input.Parameter$Masse)] < myList$Input.Parameter$ZielwertMM2$LimitBG) {
      BG.vec[str_detect(names(BG.vec),myList$Input.Parameter$Masse)] <- myList$Input.Parameter$ZielwertMM2$LimitBG
    }
  }
 
  # Replace concentration < BG
  Corr.BG.Conc <- myConc.less.BG(Corr.Average.Conc$Concentration,BG.vec)
  
  # Evaluate conc * dilution factor
  Corr.VF.Conc <- myConc.multiply.VF(Corr.BG.Conc ,myList$Header %>% 
                                       select(Index,Labels,`Dilution Factor`))
  
  
  #### OUTPUT für eQuant ####
  
  myList$eQuant[["Calibration"]] <- list("Info" = myList$eQuant$ExtCal$Which.Analytes,
                                  "CalIndex" = SmplCaliAssign.Average$CalIndex,
                                  "OutOfRange" = SmplCaliAssign.Average$OutOfRange,
                                  "Cali.Plots" = Corr.Average.Plots,
                                  "OLS" = OLS)
  
  Corr.Final <- myConcFinal(conc = Corr.VF.Conc,
                            Analytes = myList$eQuant$Calibration$Info$Analytes,
                            SignifDigits = myList$Input.Parameter$Significant.Digits,
                            BGs = BG.vec)
  
  myList$eQuant$Evaluation[["Conc.less.BG"]] <- Corr.BG.Conc
  myList$eQuant$Evaluation[["BG.Report"]] <- BG.vec
  myList$eQuant$Evaluation[["Conc.multiply.VF"]] <- Corr.VF.Conc
  myList$eQuant$Evaluation[["Conc.Final"]] <- Corr.Final
  
  
  
  myList$eQuant
  
}
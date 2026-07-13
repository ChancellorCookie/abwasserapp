my.tQuant.Concentration.Evaluation <- function(myList = myList){
  
  ExtCal.which.Analytes <- myList$Chromatography$Concentration$Analytes
  ExtCal.which.Units <- myUnitsFormat(myList$Chromatography$Concentration$Units)
  SampleDF <- myList$Chromatography$PeakArea$Data[c("Index","Labels",ExtCal.which.Analytes)]
  StdDef <- myList$Calibration$Std.Concentration
  
  StdDef %<>% filter(complete.cases(StdDef))
  SmplCaliAssign.Average <- mySmplCaliAssign(SampleDF,
                                             StdDef,
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
  myList$Concentration <- Corr.Average.Conc
  
  # Calibration Plots
  Corr.Average.Plots <- myCaliGraph.list(Analytes = ExtCal.which.Analytes,
                                         Units = ExtCal.which.Units,
                                         Corr.Average = myList$Calibration$Std.PeakArea,
                                         StdDef = myList$Calibration$Std.Concentration,
                                         StdIndex = StdDef$Index)
  
  # Calibration Statistics
  OLS <- myOLS.tQuant(myList)
  
  
  
  
  # Extraction of BG Values calculated by OLS function and stored in myList container
  BG.vec <- signif(myOLS_BG(OLS,"xBG"),myList$Input.Parameter$Significant.Digits)
  # Fehler bei BG = NaN
  BG.vec[is.na(BG.vec)] <- Inf
  # If User selected, the usage of SAA defined BG Values, a comparison for underscore must be performed
  if (myList$Input.Parameter$BG.SAA) {
    # Get defaults vector from defaults.csv
    BG.SAA <- read.defaults() %>% 
      filter(SAA.Menue %in% myList$Input.Parameter$SAA.Selected) %>% 
      filter(QC.Kind %in% "VBW") %>% 
      select(LimitBG) %>% pull() %>% 
      `names<-`(read.defaults() %>% 
                  filter(SAA.Menue %in% myList$Input.Parameter$SAA.Selected) %>% 
                  filter(QC.Kind %in% "VBW") %>% 
                  select(Analyt) %>% pull())
    # Function to return BG for Report.
    BG.vec <- BG.Report(BG.vec,BG.SAA)
  }
  

  # Replace concentration < BG
  Corr.BG.Conc <- myConc.less.BG(Corr.Average.Conc$Concentration,BG.vec[!is.na(BG.vec)]) 
    # BG.vec kann ggf einen NA enthalten (meist bei fehlerhafter Kalibration). In diesem Fall wird die BG nicht berücksichtigt
  
  # Evaluate conc * dilution factor
  Corr.VF.Conc <- myConc.multiply.VF(Corr.BG.Conc,myList$Header %>% 
                                       select(Index,Labels,`Dilution Factor`))
  
  
  #### OUTPUT für tQuant ####
  
  myList$Calibration[["Info"]] <- list("Analytes" = ExtCal.which.Analytes,
                                              "Units" = ExtCal.which.Units)
  myList$Calibration[["CalIndex"]] <- SmplCaliAssign.Average$CalIndex
  myList$Calibration[["OutOfRange"]] <- SmplCaliAssign.Average$OutOfRange
  myList$Calibration[["Cali.Plots"]] <- Corr.Average.Plots
  myList$Calibration[["OLS"]] <- OLS
  
  Corr.Final <- myConcFinal(conc = Corr.VF.Conc,
                            Analytes = ExtCal.which.Analytes,
                            SignifDigits = myList$Input.Parameter$Significant.Digits,
                            BGs = BG.vec)
  
  myList$Concentration[["Conc.less.BG"]] <- Corr.BG.Conc
  myList$Concentration[["BG.Report"]] <- BG.vec
  myList$Concentration[["Conc.multiply.VF"]] <- Corr.VF.Conc
  myList$Concentration[["Conc.Final"]] <- Corr.Final
  
  
  
  return(myList)
  
}

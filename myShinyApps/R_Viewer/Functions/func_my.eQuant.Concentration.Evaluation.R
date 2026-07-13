my.eQuant.Concentration.Evaluation <- function(myList = myList){
  ExtCal.which.Analytes <- myList$Raw.Data$eQuant$ExtCal$Which.Analytes$Analytes
  SampleDF <-myList$Raw.Data$eQuant$Corrected.Data$Corr.Average[c("Index","Labels",ExtCal.which.Analytes)]
  StdDef <- myList$Raw.Data$eQuant$Std.Concentration
  
  StdIndex <- StdDef$Index[order(StdDef$Index)] # Sortierung nach Aufsteigendem index
  
  
  SmplCaliAssign.Average <- mySmplCaliAssign02(SampleDF,
                                             StdDef,
                                             ExtCal.which.Analytes,
                                             OutOfRange.Factor = 1.2)
  
  ### Linear Regression (OLS)
  
  
  
  # Ausreißer Tests
  
  
  
  # Concentrationen
  Corr.Average.Conc <- myConcentration(Corr.Average = SampleDF,
                                       StdDef = StdDef,
                                       CalIndex = SmplCaliAssign.Average$CalIndex,
                                       Analytes = ExtCal.which.Analytes)
  # Store in myList
  myList$Concentration[["Concentration"]] <- Corr.Average.Conc$Concentration
  myList$Concentration[["UsedStandards"]] <- Corr.Average.Conc$UsedStandards
  
  
  # Calibration Plots
  Corr.Average.Plots <- myCaliGraph.list(Analytes = myList$Raw.Data$eQuant$ExtCal$Which.Analytes$Analytes,
                                         Units = unique(myList$Raw.Data$eQuant$ExtCal$Which.Analytes$Units),
                                         Corr.Average = myList$Raw.Data$eQuant$Corrected.Data$Corr.Average,
                                         StdDef = myList$Raw.Data$eQuant$Std.Concentration,
                                         StdIndex = StdDef$Index)
  
  # Calibration Statistics
  if (min(as.numeric(myList$Header %>% filter(Index %in% myList$Raw.Data$eQuant$Std.Concentration$Index) %>% select(Main.Runs) %>% pull())) < 3) {
    m <- 3
    
  } else {
    m <- min(as.numeric(myList$Header %>% filter(Index %in% myList$Raw.Data$eQuant$Std.Concentration$Index) %>% select(Main.Runs) %>% pull()))
  }
  OLS <- myOLS.eQuant(Analytes = myList$Raw.Data$eQuant$ExtCal$Which.Analytes$Analytes,
                      Corr.Average = myList$Raw.Data$eQuant$Corrected.Data$Corr.Average[myList$Raw.Data$eQuant$Std.Concentration$Index,],
                      Std.Concentration = myList$Raw.Data$eQuant$Std.Concentration,
                      BGMethod = myList$Input.Parameter$BGMethod,
                      alpha = myList$Input.Parameter$Alpha,
                      SD = myList$Raw.Data$eQuant$Raw$Raw.STD$Data,
                      m = m)
  
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
  Corr.BG.Conc <- myConc.less.BG(Corr.Average.Conc$Concentration,BG.vec)
  
  # Evaluate conc * dilution factor
  Corr.VF.Conc <- myConc.multiply.VF(Corr.BG.Conc ,myList$Header %>% 
                                       select(Index,Labels,`Dilution Factor`))
  
  
  #### OUTPUT für eQuant ####
  
  myList$Calibration[["Info"]] <- myList$Raw.Data$eQuant$ExtCal$Which.Analytes
  myList$Calibration[["CalIndex"]] <- SmplCaliAssign.Average$CalIndex
  myList$Calibration[["OutOfRange"]] <- SmplCaliAssign.Average$OutOfRange
  myList$Calibration[["Cali.Plots"]] <- Corr.Average.Plots
  myList$Calibration[["OLS"]] <- OLS
  
  Corr.Final <- myConcFinal(conc = Corr.VF.Conc,
                            Analytes = myList$Calibration$Info$Analytes,
                            SignifDigits = as.numeric(myList$Input.Parameter$Significant.Digits),
                            BGs = BG.vec)
  
  myList$Concentration[["Conc.less.BG"]] <- Corr.BG.Conc
  myList$Concentration[["BG.Report"]] <- BG.vec
  myList$Concentration[["Conc.multiply.VF"]] <- Corr.VF.Conc
  myList$Concentration[["Conc.Final"]] <- Corr.Final
  
  
  
  myList
  
}

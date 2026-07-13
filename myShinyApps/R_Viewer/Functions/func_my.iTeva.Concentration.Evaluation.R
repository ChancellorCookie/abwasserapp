my.iTeva.Concentration.Evaluation <- function(myList = myList){
  
  ExtCal.which.Analytes <- myList$Calibration$Info$Analytes
  SampleDF <- data.frame("Index" = myList$Header$Index,
                         "Labels"=myList$Header$Probenname,
                         myList$Measured$Corrected.Means %>% select(-Index),
                         stringsAsFactors = F)
  StdDef <- myList$Calibration$Definition
  StdIndex <- StdDef$Index
  Units <- myList$Calibration$Unit
  SignifDigits <- as.numeric(myList$Input.Parameter$Significant.Digits)
  
  
  SmplCaliAssign.Average <- mySmplCaliAssign(SampleDF,
                                             StdDef,
                                             ExtCal.which.Analytes,
                                             OutOfRange.Factor = 1.2)
  
  ### Linear Regression (OLS)
   # Concentrationen
  Corr.Average.Conc <- myConcentration(Corr.Average = SampleDF,
                                       StdDef = StdDef,
                                       CalIndex = SmplCaliAssign.Average$CalIndex,
                                       Analytes = ExtCal.which.Analytes)
  
  
  # Calibration Plots
  Corr.Average.Plots <- myCaliGraph.list(Analytes = ExtCal.which.Analytes,
                                         Units = myList$Calibration$Unit,
                                         Corr.Average = SampleDF,
                                         StdDef = StdDef,
                                         StdIndex = StdIndex)
  
  # Calibration Statistics
  OLS <- myOLS.iTeva(Analytes = ExtCal.which.Analytes,
                     Corr.Average = myList$Measured$Corrected.Means,
                     Std.Concentration = myList$Calibration$Definition,
                     BGMethod.Init = myList$Input.Parameter$BGMethod,
                     SD = myList$Measured$SD.Outliers,
                     m = if (myList$Input.Parameter$BGMethod == "DIN32645") {min(myList$Header$Messwiederh.)}else{1},
                     alpha = myList$Input.Parameter$Alpha,
                     outlier.test = myList$Input.Parameter$outlier)
  
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
  Corr.VF.Conc <- myConc.multiply.VF(Corr.BG.Conc ,myList$Header  %>% 
                                       select(Index,Probenname,KorrFaktor) %>% `names<-`(c("Index","Labels","Dilution Factor")))
  
  
  #### OUTPUT für eQuant ####
  
  myList$Calibration[["CalIndex"]] <- SmplCaliAssign.Average$CalIndex
  myList$Calibration[["OutOfRange"]] <- SmplCaliAssign.Average$OutOfRange
  myList$Calibration[["Cali.Plots"]] <- Corr.Average.Plots
  myList$Calibration[["OLS"]] <- OLS
  
  Corr.Final <- myConcFinal(conc = Corr.VF.Conc,
                            Analytes = ExtCal.which.Analytes,
                            SignifDigits = SignifDigits,
                            BGs = BG.vec)
  # Store in myList
  myList$Concentration[["Concentration"]] <- Corr.Average.Conc$Concentration
  myList$Concentration[["UsedStandards"]] <- Corr.Average.Conc$UsedStandards
  myList$Concentration[["Conc.less.BG"]] <- Corr.BG.Conc
  myList$Concentration[["BG.Report"]] <- BG.vec
  myList$Concentration[["Conc.multiply.VF"]] <- Corr.VF.Conc
  myList$Concentration[["Conc.Final"]] <- Corr.Final
  
  
  
  return(myList)
  
}

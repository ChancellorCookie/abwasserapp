myBEN_SN <- function(x,y,m,alpha,Titel,Chromatogram.Blk,Chromatogram.1Std,PeakStart.1Std,PeakEnd.1Std,Concentration.1Std){
  
  ######################################
  # KALIBRATION
  ######################################
  x.input <- x
  y.input <- y
  # Check for NA in x or y
  na.x <- sapply(x,function(i)all(is.na(i)))
  na.y <- sapply(y,function(i)all(is.na(i)))
  na <- na.x | na.y
  
  x <- x[!na]
  y <- y[!na]
  
  CaliEval.List <- myCaliEval(x,y,m=1,alpha,SubTitle = Titel)
  CaliEval.List.Stored <- CaliEval.List
  
  
  if (sum(CaliEval.List$Outliers$Outlier)>0){
    CaliEval.List <- myCaliEval(x[!CaliEval.List$Outliers$Outlier],y[!CaliEval.List$Outliers$Outlier],m,alpha,SubTitle = Titel)
  }
  
  a <- CaliEval.List$b0
  b <- CaliEval.List$b1
  sx0 <- CaliEval.List$sx0
  sxy <- CaliEval.List$sxy
  Qxx <- CaliEval.List$Qxx
  
  ######################################
  
  
  # Die Spalten der Chromatogramme müssen mit "Time" und "Counts" beschriftet sein 
  
  Time.Diff <- PeakEnd.1Std-PeakStart.1Std
  
  Peak.1Std <- Chromatogram.1Std %>% filter(Time > PeakStart.1Std & Time < PeakEnd.1Std)
  Area.1Std <- mean(Peak.1Std$Counts)*Time.Diff
  
  Peak.Blk <- Chromatogram.Blk %>% filter(Time > PeakStart.1Std & Time < PeakEnd.1Std)
  Area.Blk <- mean(Peak.Blk$Counts)*Time.Diff
  
  SN <- Area.1Std/Area.Blk
  
  BG <- Concentration.1Std * 10 / SN
  EG <- Concentration.1Std * 6 / SN
  NG <- Concentration.1Std * 3 / SN
  
  
  OLS <- list("Methode" = "Signal/Rausch - Methode",
              "x.input" = x.input,
              "y.input" = y.input,
              "na.x" = na.x,
              "na.y" = na.y,
              "x" = x,
              "y" = y,
              "Qxx" = CaliEval.List$Qxx,
              "Qyy" = CaliEval.List$Qyy,
              "Qxy" = CaliEval.List$Qxy,
              "sxy" = CaliEval.List$sxy,
              "sx0" = CaliEval.List$sx0,
              "b0"=CaliEval.List$b0,
              "b1"= CaliEval.List$b1,
              "sb0"=CaliEval.List$sb0,
              "sb1"= CaliEval.List$sb1,
              "R"= CaliEval.List$R,
              "m" = m,
              "n" = CaliEval.List$n,
              "Alpha" = alpha,
              "ta" = CaliEval.List$ta,
              "yUp" = CaliEval.List$yUp,
              "yDown" = CaliEval.List$yDown,
              "y_fit" = CaliEval.List$y_fit,
              "Residuen" = CaliEval.List.Stored$Residuen,
              "Outliers" = CaliEval.List.Stored$Outliers,
              "Plot_fit" = CaliEval.List$Plot_fit,
              "Plot_res" = CaliEval.List.Stored$Plot_res,
              "Chromatogram.Blk" = Chromatogram.Blk,
              "Chromatogram.1Std" = Chromatogram.1Std,
              "PeakStart.1Std" = PeakStart.1Std,
              "PeakEnd.1Std" = PeakEnd.1Std,
              "Time.Diff" = Time.Diff,
              "Peak.Blk" = Peak.Blk,
              "Peak.1Std" = Peak.1Std,
              "Area.Blk" = Area.Blk,
              "Area.1Std" = Area.1Std,
              "Concentration.1Std" = Concentration.1Std,
              "SN" = SN,
              "xNG" = NG,
              "xEG" = EG,
              "xBG" = BG)
}
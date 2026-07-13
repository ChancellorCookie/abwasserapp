myBEN_DIN <- function(x,y,k = 3,m = 5,alpha = 0.01,Method = "Kal",sl = NULL,Titel="Ein Analyt",outlier.test = TRUE,Notes = ""){
  
  # # NUR FÜR TESTZWECKE ##
  # #--------------------------------
  # # BEISPIELDATEN AUS DIN32645
  # #--------------------------------
  # x <- c(0,2.5,5,7.5,10,12.5,15)
  # y <- c(0.5,336,718,1127,1539,1950,2290)
  # m <- 11
  # k <- 3
  # alpha <- 0.01
  # Method <- "Kaiser"
  # sl <- y[1]*0.25
  # Titel="Ein Analyt"
  # #--------------------------------
  
  x.input <- x
  y.input <- y
  # Check for NA in x or y
  na.x <- sapply(x,function(i)all(is.na(i)))
  na.y <- sapply(y,function(i)all(is.na(i)))
  na <- na.x | na.y
  
  x <- x[!na]
  y <- y[!na]
  
  # Berechnung der Kalibrationswerte nach DIN 32645
  CaliEval.List <- myCaliEval(x,y,m,alpha,SubTitle = Titel)
  CaliEval.List.Stored <- CaliEval.List
  
  # Neuberechnung der Regressionswerte ohne Betrachtung der Ausreißer 
  if (outlier.test & sum(CaliEval.List$Outliers$Outlier$out)>0){
    CaliEval.List <- myCaliEval(x[!CaliEval.List$Outliers$Outlier$out],y[!CaliEval.List$Outliers$Outlier$out],m,alpha,SubTitle = Titel)
  }
    
  a <- CaliEval.List$b0
  b <- CaliEval.List$b1
  sx0 <- CaliEval.List$sx0
  sxy <- CaliEval.List$sxy
  Qxx <- CaliEval.List$Qxx
  
  n <- length(x)
  f <- n - 2
  ta <- qt(1-alpha,f)
  ta2 <- qt(1-alpha/2,f)
  
  # Gleichung
  yk <- a + sxy*ta*sqrt(1/n + 1/m + mean(x)^2/Qxx)
  
  ## Kalibriergradenmethode
  if(Method == "Kal"){
    #___________________________________________________________________________________________________
    # Berechnung nach DIN 32645 nach Gleichung (4) in Unterabschnitt 6.3
    #___________________________________________________________________________________________________
    
    xNG <- (yk-a)/b
    xEG <- 2*xNG
    xBG <- k*sx0*ta2*sqrt(1/m + 1/n + (k*xNG-mean(x))^2/Qxx)
    Method <- "Kalibriergraden-Methode"
  }
  
  ## Leerwertmethode
  if(Method == "Leer"){
    #___________________________________________________________________________________________________
    # Berechnung nach DIN 32645 nach Gleichung (4) in Unterabschnitt 6.2.2
    #___________________________________________________________________________________________________
    if(f<2){f<-2} # Negative Freiheitsgrade sind nicht definiert
    ta <- qt(1-alpha,f)
    ta2 <- qt(1-alpha/2,f)
    
    if(is.null(sl)){
      # Gleichung 
      yk <- y[1] + sx0*b * ta * sqrt(1/m + 1/n)
      Notes <- c(Note,"Eine gemessene Stabw ist nicht verfügbar. Der Wert wurde als mittels sx0*b geschätzt.")
    }else{
      yk <- y[1] + sl * ta * sqrt(1/m + 1/n)
    }
    
    xNG <- (yk-y[1])/b
    xEG <- 2*xNG
    xBG <- k*sx0*ta2*sqrt(1/m+1/n+(k*xNG-mean(x))^2/Qxx)
    Method <- "Leerwertmethode"
  }
 
  ## Schnellschätzung nach Kaiser
  if(Method == "Kaiser"){
    #___________________________________________________________________________________________________
    # Berechnung nach DIN 32645 nach Gleichung (23) in Unterabschnitt 6.4.4
    #___________________________________________________________________________________________________
    if(is.null(sl)){
      sl <-  sx0*b
      Notes <- c(Note,"Eine gemessene Stabw ist nicht verfügbar. Der Wert wurde als mittels sx0*b geschätzt.")
    }
    xNG <- 3*sl/b
    xEG <- 6*sl/b
    xBG <- 10*sl/b
    Method <- "Schätzung nach Kaiser (3s)"
  }
  if(is.na(xBG)){xBG <- xNG *3}
  
  list("Methode" = Method,
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
       "k" = k,
       "m" = m,
       "n" = n,
       "Alpha" = alpha,
       "ta" = ta,
       "ta/2" = ta2,
       "sl" = sl,
       "yk" = yk,
       "xNG"=xNG,
       "xEG"=xEG,
       "xBG"=xBG,
       "yUp" = CaliEval.List$yUp,
       "yDown" = CaliEval.List$yDown,
       "y_fit" = CaliEval.List$y_fit,
       "Residuen" = CaliEval.List.Stored$Residuen,
       "Outliers" = CaliEval.List.Stored$Outliers,
       "Plot_fit" = CaliEval.List$Plot_fit,
       "Plot_res" = CaliEval.List.Stored$Plot_res,
       "Notes" = unique(Notes))
}
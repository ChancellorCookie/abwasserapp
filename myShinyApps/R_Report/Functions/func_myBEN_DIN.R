myBEN_DIN <- function(x,y,k = 3,m = 5,alpha = 0.01,Method = "Kal",sl = NULL,Titel="Ein Analyt"){
  
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
  
  CaliEval.List <- myCaliEval(x,y,m,alpha,SubTitle = Titel)
  CaliEval.List.Stored <- CaliEval.List
  
  
  if (sum(CaliEval.List$Outliers$Outlier)>0){
    CaliEval.List <- myCaliEval(x[!CaliEval.List$Outliers$Outlier],y[!CaliEval.List$Outliers$Outlier],m,alpha,SubTitle = Titel)
  }
    
  a <- CaliEval.List$b0
  b <- CaliEval.List$b1
  sx0 <- CaliEval.List$sx0
  sxy <- CaliEval.List$sxy
  Qxx <- CaliEval.List$Qxx
  
  ## Kalibriergradenmethode
  if(Method == "Kal"){
    n <- length(x)
    f <- n - 2
    ta <- qt(1-alpha,f)
    ta2 <- qt(1-alpha/2,f)
    
    yk <- a + sxy*ta*sqrt(1/n + 1/m + mean(x)^2/Qxx)
    xNG <- (yk-a)/b
    xEG <- 2*xNG
    xBG <- k*sx0*ta2*sqrt(1/m + 1/n + (k*xNG-mean(x))^2/Qxx)
    Method <- "Kalibriergraden-Methode"
  }
  
  ## Leerwertmethode
  if(Method == "Leer"){
    n <- m
    f <- m - 1
    if(f<2){f<-2} # Negative Freiheitsgrade sind nicht definiert
    ta <- qt(1-alpha,f)
    ta2 <- qt(1-alpha/2,f)
    
    if(is.null(sl)){
      yk <- y[1] + sx0*b * ta * sqrt(1/m + 1/n)
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
    n <- m
    f <- m - 1
    if(f<2){f<-2} # Negative Freiheitsgrade sind nicht definiert
    ta <- qt(1-alpha,f)
    ta2 <- qt(1-alpha/2,f)
    
    if(is.null(sl)){
      yk <- y[1] + sx0*b * ta * sqrt(1/m + 1/n)
    }else{
      yk <- y[1] + sl * ta * sqrt(1/m + 1/n)
    }
    
    xNG <- (yk-y[1])/b
    xEG <- 2*xNG
    xBG <- k*xNG
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
       "Plot_res" = CaliEval.List.Stored$Plot_res)
}
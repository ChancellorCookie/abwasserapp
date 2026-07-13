myBEN_DIN <- function(x,y,k = 3,m = 5,alpha = 0.01){
  
  # # NUR FÜR TESTZWECKE ##
  # #--------------------------------
  # x <- c(0.05,0.1,0.15,0.20,0.25,0.3,0.35,0.40,0.45,0.5)
  # y <- c(3060,3522,3707,4280,5058,5510,5703,6205,7156,7178)
  # m <- 11
  # k <- 3
  # alpha <- 0.01
  # #--------------------------------
  
  n <- length(x)
  
  
  qxx <- sum(x^2) - sum(x)^2/n
  qyy <- sum(y^2) - sum(y)^2/n
  qxy <- sum(x*y) - sum(x)*sum(y)/n
  
  
  
  # y = a + bx
  b <- qxy/qxx # Slope
  a <- mean(y)-b*mean(x) # Intercept
  
  rsqr <- (sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))^2)*sum((y-mean(y))^2)))^2
  
  # y^
  y_fit <- b*x + a
  
  # Residuen
  res <- y - y_fit
  
  # Ausreißertest nach Grubbs
  
  outliers <- myOutlier(res,alpha,Nalimov = F)
  
  # Fehlerbetrachtung
  sxy <- sqrt((qyy-qxy^2/qxx)/(n-2))
  
  sx0 <- sxy/b
  
  
  
  # Vorhersageintervall
  
    # t-Verteilung
  
  t <- qt(1-alpha,n-2)
  tBG <- qt(1-alpha/2,n-2)
  yUp <- y_fit + sxy*t*sqrt(1/n + 1/m + (x-mean(x))^2/qxx)
  
  yDown <- y_fit - sxy*t*sqrt(1/n + 1/m + (x-mean(x))^2/qxx)
  
  g_fit <- qplot(x,y) +
    geom_line(aes(x,y_fit),colour="blue")+ 
    geom_line(aes(x,yDown),colour="red")+ 
    geom_line(aes(x,yUp),colour="red") 
    
  dfRes <- data.frame("y_fit" = y_fit, "Residuen" = res)
  
  g_res <- ggplot(dfRes, x=, y=Residuen, aes(seq(1,n,1), Residuen)) +
    geom_bar(stat="identity", position=position_dodge())
 
  yk <- a + sxy*t*sqrt(1/n + 1/m + mean(x)^2/qxx)
  
  xNG <- (yk-a)/b
  
  xEG <- 2*xNG
  
  
  # kk <- k*sx0*tBG
  # aa <- n*m*(qxx-kk^2)
  # pp <- 2*kk*n*m*mean(x)
  # qq <- kk^2*(qxx*n+qxx*m+n*m*mean(x)^2)
  # 
  # xBG <- (-pp+sqrt(pp^2+4*aa*qq))/(2*aa)
  xBG <- k*sx0*tBG*sqrt(1/m+1/n+(k*xNG-mean(x))^2/qxx)
  
  if(is.na(xBG)){xBG <- xNG *3}
  
  list("x" = x,
       "y" = y,
       "Qxx" = qxx,
       "Qyy" = qyy,
       "Qxy" = qxy,
       "sxy" = sxy,
       "sx0" = sx0,
       "b0"=a,
       "b1"= b,
       "R"= rsqr,
       "k" = k,
       "m" = m,
       "n" = n,
       "Alpha" = alpha,
       "ta" = t,
       "ta/2" = tBG,
       "yk" = yk,
       "xNG"=xNG,
       "xEG"=xEG,
       "xBG"=xBG,
       "yUp" = yUp,
       "yDown" = yDown,
       "y_fit" = y_fit,
       "Outliers" = outliers,
       "Plot_fit" = g_fit,
       "Plot_res" = g_res)
  
}
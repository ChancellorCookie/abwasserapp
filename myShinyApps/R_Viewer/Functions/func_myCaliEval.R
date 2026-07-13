myCaliEval <- function(x,y,m = 3,alpha = 0.01,Titel = "Kalibrierung", SubTitle = "Ein Analyt"){
  
  n <- length(x)
  
  
  qxx <- sum(x^2) - sum(x)^2/n
  qyy <- sum(y^2) - sum(y)^2/n
  qxy <- sum(x*y) - sum(x)*sum(y)/n
  
  # y = a + bx
  b <- qxy/qxx # Steigung
  a <- mean(y)-b*mean(x) # Achsenabschnitt
  
  # Bestimmtheitsmaß
  rsqr <- (sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))^2)*sum((y-mean(y))^2)))^2
  
  # y^
  y_fit <- b*x + a
  
  # Residuen
  res <- y - y_fit
  
  
  # Standardfehler der Steigung
  var.b <- sum(res^2)/qxx/(n-2)
  sb <- sqrt(var.b)
  # Standardfehler des Achsenabschnitts
  var.a <- var.b/n*sum(x^2)
  sa <- sqrt(var.a)
  
  
  # Ausreißertest nach Grubbs
  outliers <- myOutlier(res,alpha,Nalimov = F)
  
  # Fehlerbetrachtung
  sxy <- sqrt((qyy-qxy^2/qxx)/(n-2))
  
  sx0 <- sxy/b
  
  # Vorhersageintervall
  
  # t-Verteilung
  
  t <- qt(1-alpha,n-2)
  yUp <- y_fit + sxy*t*sqrt(1/n + 1/m + (x-mean(x))^2/qxx)
  
  yDown <- y_fit - sxy*t*sqrt(1/n + 1/m + (x-mean(x))^2/qxx)
  
  dffit <- data.frame("x" = x, "y" = y)
  
  g_fit <-  ggplot(dffit,aes(x,y)) +
    geom_point()+
    geom_line(aes(x,y_fit),colour="blue")+ 
    geom_line(aes(x,yDown),colour="red")+ 
    geom_line(aes(x,yUp),colour="red")+
    theme_minimal() + # weißer Hintergrund mit Gittellinien
    labs(y = "Intensität",                   # Achsenbeschriftungen, 
         x = "Konzentration",         
         title = Titel,
         subtitle = SubTitle)
  
  dfRes <- data.frame("y_fit" = y_fit, "Residuen" = res)
  
  g_res <- ggplot(dfRes,aes(seq(1,n,1), Residuen)) +
    geom_bar(stat="identity", position=position_dodge())+
    theme_minimal() + # weißer Hintergrund mit Gittellinien
    labs(y = "Residuen",                   # Achsenbeschriftungen, 
         x = "Kalibrierpunkt",         
         title = Titel,
         subtitle = SubTitle)

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
       "sb0" = sa,
       "sb1" = sb,
       "m" = m,
       "n" = n,
       "Alpha" = alpha,
       "ta" = t,
       "yUp" = yUp,
       "yDown" = yDown,
       "y_fit" = y_fit,
       "Residuen" = res,
       "Outliers" = outliers,
       "Plot_fit" = g_fit,
       "Plot_res" = g_res)
}
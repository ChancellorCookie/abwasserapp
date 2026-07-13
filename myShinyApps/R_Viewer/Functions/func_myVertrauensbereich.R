myVertrauensbereich <- function(smpl_y,OLS){
  # Diese Funktion erfordert die das Intensitätssignal der unbekannten Probe (smpl_y)
  # resultierende Liste der funktion myBEN_DIN()
  
  
  # # NUR FÜR TESTZWECKE ##
  # #--------------------------------
  
  # x <- c(0.05,0.1,0.15,0.20,0.25,0.3,0.35,0.40,0.45,0.5)
  # y <- c(3060,3522,3707,4280,5058,5510,5703,6205,7156,7178)
  # m <- 1
  # k <- 3
  # alpha <- 0.01
  # OLS <- myBEN_DIN(x,y,k,m,alpha)
  # 
  # smpl_y <- 4000
  
  # #--------------------------------
  
  
  if(is.na(OLS$xBG)){OLS$xBG <- OLS$xNG *3}
  
  smpl_x <- data.frame("Signal"= smpl_y,
                       "Gehalt" = as.numeric(0),
                       "Vertrauensbereich minus" = as.numeric(0),
                       "Vertrauensbereich plus" = as.numeric(0),
                       "Anmerkung"  = as.character(""),
                       "yk" = OLS$yk,
                       "Nachweisgrenze" = OLS$xNG,
                       "Bestimmungsgrenze" = OLS$xBG,
                       "Kalibrationsmethode" = OLS$Methode,
                       stringsAsFactors = F)
  
  smpl_x$Gehalt <- (smpl_y-OLS$b0)/OLS$b1
  smpl_x$Vertrauensbereich.minus <- (smpl_y - OLS$sxy*OLS$ta*sqrt(1/OLS$n + 1/OLS$m + (smpl_x$Gehalt-mean(OLS$x))^2/OLS$Qxx) - OLS$b0)/OLS$b1
  smpl_x$Vertrauensbereich.plus <- (smpl_y + OLS$sxy*OLS$ta*sqrt(1/OLS$n + 1/OLS$m + (smpl_x$Gehalt-mean(OLS$x))^2/OLS$Qxx) - OLS$b0)/OLS$b1
  
  if( smpl_x$Gehalt < OLS$xBG){
    smpl_x$Gehalt <- 0
    smpl_x$Vertrauensbereich.minus <- 0
    smpl_x$Vertrauensbereich.plus <- 0
    smpl_x$Anmerkung <- "< BG"
    }
  
  return(smpl_x)
  
}
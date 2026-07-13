myConc.less.BG <- function(conc,BG){
  
  # BG ist ein Vector mit so vielen benannten Elementen wie Benannte Spalten in conc

  for (v in names(BG)){
    conc[is.na(conc[[v]]),v] <- 0 # Setzt NA = 0
    conc[conc[[v]] < BG[v],v] <- 0   
  }
  conc
}
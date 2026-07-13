myConc.less.BG <- function(conc,BG){
  # Funktion zu suchen nach Werten, die kleiner Bestimmungsgrenze. Diese Werte werden durch Null ersetzt
  # BG ist ein Named-Vector mit den Bestimmungsgrenzen.
  # Die Namen in BG müssen mit den Spaltennamen conc übereinstimmen

  for (v in names(BG)){
    conc[is.na(conc[[v]]),v] <- 0 # Setzt NA = 0
    conc[conc[[v]] < BG[v],v] <- 0   # Setzt x < BG  = 0
  }
  conc
}
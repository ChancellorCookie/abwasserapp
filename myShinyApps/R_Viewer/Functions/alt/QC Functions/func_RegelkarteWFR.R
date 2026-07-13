Regelkarte.WFR <- function(df,SollWert = 50,StandardFehler = F){
  
  k.Warn <- 2 # 95% Sicherheit
  k.Kontoll <- 3 # 99% Sicherheit
  
  IstWerte <- df$Wert # Alle Werte
  n <- nrow(df) # Anzahl der Werte
  
  # Basis Funktionswerte
  WFR <- IstWerte/SollWert # Wiederfindungsrate
  MW <- mean(WFR) # Mittelwert aller Werte
  SD <- sd(WFR) # Standardabweichung
  SE <- SD/sqrt(n) # Standard Fehler (Standardabweichung des Mittelwerts)
  RSD <- SD/MW # relative Standardabweichung
  RSE <- SE/MW # relative Standard Fehler
  
  # Mittelwert aller Wiederfindungsraten
  Bezugswert <- mean(WFR)
  # Warnbereich
  
  E <- SD
  if (StandardFehler) {E <- SE}
  
  OWG <- Bezugswert + k.Warn*E
  UWG <- Bezugswert - k.Warn*E
  
  # Kontrollbereich
  OKG <- Bezugswert + k.Kontoll*E
  UKG <- Bezugswert - k.Kontoll*E
  
  
  plotData <- FileDF
  plotData$WFR <- WFR
  plotData
}
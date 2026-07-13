myOutlier <- function(Data,alpha = 0.01,Nalimov = TRUE){
  
  
  ## Dies ist ein Außreißer-Test nach Nalimov, der eine Modifikation des Grubbs Tests ist
  # alpha <- 0.05
  # Data <- c(2342,1241,2452,2352,2342,4564)
  # Nalimov <- F
  # 
  
  #a <- Data # Zwischenspeichern der ursprünglichen Daten für die Ausgabe, zur Vermeidung von versehentlichem überschreiben)

  nal <- 1

  
  # Initiale Definition für alternative Betrachtung der Nullhypothese (keine Ausreißer)
  
  b <- data.frame("val" = Data,
                  "abs" = abs(Data-mean(Data,na.rm = T)),
                  "max" = abs(Data-mean(Data,na.rm = T)) == max(abs(Data-mean(Data,na.rm = T)),na.rm = T),
                  "out" = is.na(Data), # NA's werden als Außreißer deklariert
                  "za" =  rep(NA,length(Data)),
                  "G" = rep(NA,length(Data)),
                  "Loop" = rep(NA,length(Data)),
                  stringsAsFactors = F)
  
  # Initial Variable zum Zählen der Schleifendurchgänge
  loop <- 1
  # Anwendung des Nalimov Korrekturfaktors? Wird in der Regel bei Einfluss nimmt ab je mehr Werte im Vector betrachtet werden
  Test.Method <- "Grubbs"
  if (Nalimov) {
    nal <- sqrt(n/(n-1)) # Faktor in Abhängigkeit von der Anzahl der Werte
    Test.Method <- "Nalimov"
  }
  
  output <- list("Test.Method" = Test.Method,"Outlier.Check" = b[,c("val","out","za","G","Loop")])
  
  if (sum(!b$out) < 3 | sd(b$val[!b$out],na.rm = T) == 0) {
    output$Test.Method <- "Fehler bei der Anzahl verschiedener Werte (n<3 oder sd=0)"
    return(output)
  } 
  
  while(1){ # Schleife ohne Abbruchkriterium. Abbruch erfolgt in der Schleife selbst
    # Bei weiniger als 3 Werten kann keine Prüfung gemacht werden
    # Wenn die Werte identisch sind (Standardabweichung = 0) kann der Test ebenfalls übersprungen werden
    if (sum(!b$out) < 3 | sd(b$val[!b$out],na.rm = T) == 0) {
      return(output)
    } 
    
    # Prüfgröße G die mit dem Tabellenwert za verglichen wird
    b$G[b$max & !b$out] <- b$abs[b$max & !b$out]/sd(b$val[!b$out],na.rm = T)*nal
    b$za[b$max & !b$out] <- myGrubbs_Gval(alpha = alpha,n = sum(!b$out))
    # Laufindex für den Output um die Reihenfolge der Prüfwerte rückverfolgen zu können
    b$Loop[b$max & !b$out] <- loop
    if (b$G[b$max & !b$out] > b$za[b$max & !b$out]) {
      # Markieren des Ausreißers und Speichern der relevanten Prüfkriterien zur Rückverfolgung
      b$out[b$max & !b$out] <- TRUE # muss als letzter Parameter geändert werden, sonst passt der b$...[b$max & !b$out] Filter nicht mehr
      # Speichern der Daten in der output Liste
      output <- list("Test.Method" = Test.Method,"Outlier.Check" = b[,c("val","out","za","G","Loop")])
      # Neuberechnung der absoluten Differenzen zum neuen Mittelwert und Markierung des neuen Maximums
      b$abs[!b$out] <- abs(b$val[!b$out]-mean(b$val[!b$out],na.rm = T))
      b$max[!b$out] <- abs(b$val[!b$out]-mean(b$val[!b$out],na.rm = T)) == max(abs(b$val[!b$out]-mean(b$val[!b$out],na.rm = T)))
    }else{ # wenn kein Ausreißer (mehr) vorliegt
      output <- list("Test.Method" = Test.Method,"Outlier.Check" = b[,c("val","out","za","G","Loop")])
      return(output)
    }
    loop <- loop + 1
   }
}
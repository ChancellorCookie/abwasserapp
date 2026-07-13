myOutlier <- function(Data,alpha = .05,Nalimov = FALSE){
  
  
  ## Dies ist ein Außreißer-Test nach Nalimov, der eine Modifikation des Grubbs Tests ist
  # require("outliers")
  # a <- Raw_Intensity$`187Re.mp_STD`[1:10]
  # alpha <- 1
  # a<- Data
  a <- Data # Zwischenspeichern der ursprünglichen Daten für die Ausgabe,
         #zur Vermeidung von versehentlichem überschreiben)
  nalimov <- 1
  
  
  while(1){ # Schleife ohne Abbruchkriterium. Abbruch erfolgt in der Schleife selbst
    
    n <- length(a) # länge des Vektors (Anzahl Werte)
    
    # Try-Catch Bedingung:
    # Wenn n = 2, dann erfolgt der der folgende Aufruf
    # qt(alpha,0) -> ERROR
    if (n <= 2) {
      print("Ups: Zu wenige Freiheitsgrade. Die Länge des Vektors muss mindestens n=3 betragen!")
      return(a)} 
    
    # Diese Formel für die Tabellenwerte kann man bei Wikipädia nachlesen
    # qt(alpha, Freiheitsgrad) gibt die Fläche der t-Verteilung zurück
    za <- (n-1)/sqrt(n)*sqrt(qt(alpha/2/n,n-2)^2/(n-2+qt(alpha/2/n,n-2)^2))
    # za ist der Prüfwert der Nullhypothese (kein Ausßreißer)
    
    # Potentielle Ausreißer können nur das Maximum oder Minimum des Vektors sein 
    a_max <- max(a) # Maximal Wert
    a_min <- min(a) # Minimal Wert
    a_max_loc <- which(Data==a_max) # Position (Index) des potenziellen Maximums
    a_min_loc <- which(Data==a_min) # oder Minimums im Vektor
    
    # Parameter für die Berechnung des Test-Werts
    a_mean <- mean(a) # Mittelwert
    a_sd <- sd(a) # Standardabweichung
    
    
    # Anwendung des Nalimov Korrekturfaktors?
    if (Nalimov) {
      nalimov <- sqrt(n/(n-1)) # Faktor in Abhängigkeit von der Anzahl der Werte
    }
    
    # Prüfung-Werte der potentiellen Ausreißer
    Gmax <- (a_max-a_mean)/a_sd*nalimov # Prüfwert für das Maximum
    Gmin<- (a_mean-a_min)/a_sd*nalimov # Prüfwert für das Maximum 
    
    
    if(Gmax>za | Gmin>za){ # Gibt es wenigstens einen Ausreißer?
      if (Gmax > Gmin) { # Welcher Wert weicht mehr ab? 
        a <- a[!a %in% a_max] # Entfernung des höheren Ausreißers
        if (!exists("loc_outliers")) {loc_outliers <- a_max_loc # nur beim ersten Schleifensurchlauf
        }else{loc_outliers <- c(loc_outliers,a_max_loc)} 
        # Erstellung eines Vektors mit fortlaufender Erweiterung um die Position des Ausreißers
      } else { # Gleichen Funktionen nur für das Minimum
        a <- a[!a %in% a_min] # Entfernung des höheren Ausreißers
        if (!exists("loc_outliers")) {loc_outliers <- a_min_loc # nur beim ersten Schleifensurchlauf
        }else{loc_outliers <- c(loc_outliers,a_min_loc)} 
      }
    } else{ # wenn kein Ausreißer (mehr) vorliegt
      
      Outlier <- logical(length(Data))
      
      if (exists("loc_outliers")) {Outlier[loc_outliers] <- TRUE}
      
      output <- data.frame("Residuen" = Data,"Outlier" = Outlier)
      return(output)
    }
  }
}

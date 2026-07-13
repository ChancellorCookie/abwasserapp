myOutlier <- function(Data,alpha = 0.01,Nalimov = TRUE){
  
  
  ## Dies ist ein Außreißer-Test nach Nalimov, der eine Modifikation des Grubbs Tests ist
  # alpha <- 0.05
  # Data <- vec
  # Nalimov <- F
  # 
  
  a <- Data # Zwischenspeichern der ursprünglichen Daten für die Ausgabe, zur Vermeidung von versehentlichem überschreiben)

  nal <- 1

  output <- list("Test.Method" = "Grubbs","Input" = Data,"Outlier" = logical(length(Data)))
  
  while(1){ # Schleife ohne Abbruchkriterium. Abbruch erfolgt in der Schleife selbst
    
    n <- sum(!is.na(a)) # länge des Vektors (Anzahl Werte)
    
    
    # Diese Formel für die Tabellenwerte kann man bei Wikipädia nachlesen
    # qt(alpha, Freiheitsgrad) gibt die Fläche der t-Verteilung zurück
    za <- (n-1)/sqrt(n)*sqrt(qt(alpha/2/n,n-2)^2/(n-2+qt(alpha/2/n,n-2)^2))
    # za ist der Prüfwert der Nullhypothese (kein Ausßreißer)
    
    # Potentielle Ausreißer können nur das Maximum oder Minimum des Vektors sein 
    a_max <- max(a,na.rm = T) # Maximal Wert
    a_min <- min(a,na.rm = T) # Minimal Wert
    a_max_loc <- which(a==a_max) # Position (Index) des potenziellen Maximums
    a_min_loc <- which(a==a_min) # oder Minimums im Vektor
    
    # Parameter für die Berechnung des Test-Werts
    a_mean <- mean(a,na.rm = T) # Mittelwert
    a_sd <- sd(a,na.rm = T) # Standardabweichung
    
    # Abbruchkriterium
    if (n <= 2 | a_sd == 0) { # n <= 2 kann keine Prüfung gemacht werden
      return(output)
    } 
    
    # Anwendung des Nalimov Korrekturfaktors?
    if (Nalimov) {
      nal <- sqrt(n/(n-1)) # Faktor in Abhängigkeit von der Anzahl der Werte
      output$Test.Method <- "Nalimov"
    }
    
    # Prüfung-Werte der potentiellen Ausreißer
   
     Gmax <- (a_max-a_mean)/a_sd*nal # Prüfwert für das Maximum
    Gmin<- (a_mean-a_min)/a_sd*nal # Prüfwert für das Maximum 
    
    
    if(Gmax>za | Gmin>za){ # Gibt es wenigstens einen Ausreißer?
      if (Gmax > Gmin) { # Welcher Wert weicht mehr ab? 
        a[a %in% a_max] <- NA  # Entfernung des höheren Ausreißers
        output$Outlier[a_max_loc] <- TRUE
      }else{
        a[a %in% a_min] <- NA  # Entfernung des höheren Ausreißers
        output$Outlier[a_min_loc] <- TRUE
      }
    }else{ # wenn kein Ausreißer (mehr) vorliegt
      return(output)
    }
  }
}

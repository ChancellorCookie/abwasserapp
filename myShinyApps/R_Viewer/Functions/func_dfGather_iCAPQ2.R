dfGather_iCAPQ2 <- function(df,massTrace){
  df <- myList$Data$Raw.Intensity
  massTrace <- massTraces
  require("dplyr")
  require("tidyr")
  
  # Daten Sortieren
  for(i in 1:length(massTrace)) {
    # Temporäre Datentabelle mit einer Massespur
    
    TraceReps <- sum(names(df) %in% massTrace[i])
    newTraceLabels <- paste0(rep(massTrace[i],TraceReps),".",seq(1,TraceReps))
    dataSingleTrace <- df[,names(df) %in% c("Index","Labels",massTrace[i])]
    # %>% `names<-`(.,c("Index","Labels",newTraceLabels))
    
    # Umsortierung der Massespuren in eine "wide to long" Tabelle.
    # %>% PIPE Zeichen, zum übertragen des DataFrames an die Funktion.
    # Vorteil ist, dass die Pipe sich in Reihe schalten lässt
    dataSortedSingleTrace <- dataSingleTrace %>% gather(key = "Wiederholung", # Bezeichnung der neuen Spalte
                                                        value = "bla", # Dummy-Bezeichnung, weil massTrace[i] nicht funktioniert
                                                        -Index, # belässt die Spalte "Index"
                                                        -Labels, # belässt die Spalte "Labels"
                                                        na.rm = TRUE # überspringt NA's (rm = remove)
                                                        ) ;
    
    # Umbenennen der Spalte "bla" durch die passende Massespur
    names(dataSortedSingleTrace)[4] <- massTrace[i] ;
    
    
    # Die Spalte Wiederholung enthält die gelabelten und fortlaufend nummerierten Mass-Spur Bezeichnungen,
    # die nur noch die Nummer der Wiederholung enthalten soll.
    # Dabei wird der Spalten inhalt mit sich selbst überschrieben...
    # Diese Syntax ist in den meisten anderen Programmiersprachen nicht möglich.
    
    #dataSortedSingleTrace$Wiederholung <- as.integer(gsub(paste0(massTrace[i],"."),"",  # entfernt die Bezeichung der MasseSpur
    #                                                      dataSortedSingleTrace$Wiederholung, # in der Spalte Wiederholungen
    #                                                      fixed=TRUE   # vermeidet interpretationen von Sonderzeichen
    #                                                      )
    #                                                 ); # Konvertiert den String in eine Integer
    
    
    dataSortedSingleTrace$Wiederholung <- as.integer(rep(seq(1,TraceReps),nrow(df)))
    
    # es folgt die Aneinanderreihung der sortierten Masse-Spuren 
    if(i == 1){ # Der erste gather Durchlauf generiert die Spalten "bla" (Name der Massespur) und "Wiederholungen"
      dataSorted <- dataSortedSingleTrace;
    }
    else{ # in den nachfolgenden Durchläufen sollen diese Spalten jedoch nicht mehr geändert werden.
      dataSorted <- merge(dataSorted,dataSortedSingleTrace,by=c("Index","Labels","Wiederholung"));
      # die Funktion merge() vergleicht die beiden beiden Tabellen, die zusammengeführt werden anhand der Spalten by=...
      # und fügt die verbleibende Spalte in der passenden Reihenfolge an.
    }
  }
  return(dataSorted)
}
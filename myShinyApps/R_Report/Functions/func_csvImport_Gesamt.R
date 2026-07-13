
readCSV_func_Gesamt <- function(SAA = "gesamt") {
  
  source('./func_dfGather_iCAPQ.R', encoding = 'UTF-8')
  require("magrittr")
  require("dplyr")
  require("tidyr")
  
    
  # Einlesen der Rohdaten im CSV Format
  Path <- file.choose(new = FALSE)
  #Path <- "testPt.csv"
  data_Raw <- read.csv(file = Path,  # OpenFileDialog
                       
                       header = FALSE, # kein Header
                       sep = ",",      # Zellen mit Kommata getrennt
                       fill = TRUE,
                       na.strings = "",
                       blank.lines.skip = TRUE,
                       stringsAsFactors = FALSE
  );
  
  
  if(SAA == "gesamt"){   # Ab hier werden die Geräte differenziert
    # Bereinigung der Raw-Tabelle
    data_Raw[1,1] <- "Index" # Umbenennung der ersten Zelle der Tabelle.
    data_Raw[1,2] <- "Labels" # Umbenennung der zweiten Zelle der Tabelle.
    data_Raw <- data_Raw[, sapply(data_Raw, function(i)!all(is.na(i)))]; # entfernt leere Spalten
    data_Raw[4,] <- gsub("Â","",data_Raw[4,]) # Entfernt das Sonderzeichen aus den Zellen (Y (Âµg/l))
    data_Raw[,2] <- gsub("Â","",data_Raw[,2]) # Entfernt das Sonderzeichen aus den Zellen ( Âµg/l )
    
    # Ermittlung der Einheiten
    data_Units <- unique(as.character(data_Raw[4,!data_Raw[4,] %in% c(NA)]))
    data_Units <- gsub("Y ","",data_Units) # Entfernt das Sonderzeichen aus den Zellen (Y (Âµg/l))
    data_Units <- gsub("[()]","",data_Units) # Entfernt das Sonderzeichen aus den Zellen (Y (Âµg/l))
    data_Units <- data_Units %>% data.frame(stringsAsFactors = FALSE, row.names = c("Raw_Intensity","ExtCal_StandardConcentration","IntCal_StandardConcentration"))
    names(data_Units) <- "Units"
 
  
    # Der Export muss zwingend die Spalten
    #   Raw.Intensity (Messdaten)
    #   ExtCal.StandardConcentration (Konzentrationen der Kalibrationsstandards)
    #   BrigidMS (Zuweisung der gemessenen Proben zu den Standards und Angabe der Verdünnungsfaktoren)
    # enthalten
  
    # Trennung Raw-DataFrames in Drei DataFrames
    Raw_Intensity <- data_Raw[as.character(data_Raw[1,]) %in% c("Index","Labels","Raw.Intensity")];
    ExtCal_StandardConcentration <- data_Raw[as.character(data_Raw[1,]) %in% c("Index","Labels","ExtCal.StandardConcentration")];
    BrigidMS <- data_Raw[as.character(data_Raw[1,]) %in% c("Index","Labels","BrigidMS")];
    StartTime <- data_Raw[as.character(data_Raw[1,]) %in% c("Index","Labels","StartTime")];
    # ACHTUNG!!!
    # Die Spalten Index und Labels sind nicht Bestandteil der neuen DataFrames und müssen nachträglich eingefügt werden
    
    
    
    
    # Betrifft nur die Bearbeitung der Raw_Intensity Daten
    
    dataLabel <- Raw_Intensity[3,];          # nur die gesamte 3te Zeile
    dataLabel <- as.character(dataLabel[1,]); # Erstellung eines Vektors aus dem Label DataFrame
    dataLabel <- dataLabel[!dataLabel %in% c(NA, "")]; # NA's werden aus dem Vektor entfernt
    
    
    massTrace <- unique(dataLabel); # Unterschiedliche Labels werden zusammengefasst
    massTrace <- gsub("[()]","",massTrace);  # entfernt Klammern aus den Labels
    massTrace <- gsub("\\s",".",massTrace);  # ersetzt die Leerzeichen durch einen "."
    
    t <- table(dataLabel) # Alternative zum ermittelen der Dimension
    n <- t[[1]]
    
    linseq <- rep(1:n, each=length(massTrace)); 
    # Wiederholende Sequenz:
    # Elemente 1,2,3...n
    # Wiederholungsanzahl each = x
    # x ist definiert durch die Anzahl an verschiedenen Massespuren
    dataLabel <- paste0(rep(massTrace,n),".",linseq);
    
    
    dataLabel <- c("Index","Labels",dataLabel);
    names(Raw_Intensity) <- dataLabel; #Benennt die Spalten neu
    names(ExtCal_StandardConcentration) <- c("Index","Labels",massTrace)
    names(BrigidMS) <- c("Index","Labels",c(BrigidMS[3,3:ncol(BrigidMS)]))
    names(StartTime) <- c("Index","Labels",c(StartTime[1,3:ncol(StartTime)]))
    
    
    data_Extracted <- Raw_Intensity[5:nrow(Raw_Intensity),] # Extraktion der eigentlichen Daten
    ExtCal_StandardConcentration <- ExtCal_StandardConcentration[5:nrow(ExtCal_StandardConcentration),]
    BrigidMS <- BrigidMS[5:nrow(BrigidMS),]
    StartTime <- StartTime[5:nrow(StartTime),]
    
    # Alle Ziffern sind zur Zeit noch als Character formatiert, d.h., Berechnungen sind nicht möglich.
    # Daher werden die Spalten mit den Messwerten in einer Schleife in das Numeric Format konvertiert.
    data_Extracted[[1]] <- as.integer(data_Extracted[[1]])
    for(i in 3:length(data_Extracted)) {
      data_Extracted[[i]] <- as.numeric(data_Extracted[[i]])
    }
    ExtCal_StandardConcentration[[1]] <- as.integer(ExtCal_StandardConcentration[[1]])
    for(i in 3:length(ExtCal_StandardConcentration)) {
      ExtCal_StandardConcentration[[i]] <- as.numeric(ExtCal_StandardConcentration[[i]])
    }
    
    BrigidMS$Index <- as.integer(BrigidMS$Index)
    BrigidMS$`Dilution Factor` <- as.integer(BrigidMS$`Dilution Factor`)
    
    StartTime$StartTime %<>% strptime(format = "%FT%T")
    
    
    #######################################################
    # Sortieren
    dataSorted <- dfGather_iCAPQ(data_Extracted,massTrace)
    #######################################################
  }
  
  duration <- StartTime$StartTime[length(StartTime$StartTime)] - StartTime$StartTime[1]
  
  fileName <- basename(Path)
  Path <- dirname(Path)
  Infos <- list("Filename" = fileName, 
                "Path" = Path, 
                "Date" = as.character.Date(StartTime[1,3],format ="%d %b %Y"), 
                "Duration" = duration)
  
  myOutput <- list(massTrace,dataSorted,BrigidMS,ExtCal_StandardConcentration,data_Units,Infos)
  return(myOutput)
}


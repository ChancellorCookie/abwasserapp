
readCSV_iCAPQ <- function(){
  
  # Einlesen der Rohdaten im CSV Format
  Path <- tk_choose.files(filters = matrix( c("CSV Dateien","*.csv","Alle Dateien","*.*"),
                                            nrow = 2,
                                            ncol = 2,
                                            byrow = T,
                                            dimnames = list(c("csv","All"),c("",""))))
  
  data_Raw <- read.csv(file = Path,  # OpenFileDialog
                       
                       header = FALSE, # kein Header
                       sep = ",",      # Zellen mit Kommata getrennt
                       fill = TRUE,
                       na.strings = "",
                       blank.lines.skip = TRUE,
                       stringsAsFactors = FALSE
  );
  
  
  
  # Bereinigung der Raw-Tabelle
  data_Raw[1,1] <- "Index" # Umbenennung der ersten Zelle der Tabelle.
  data_Raw[1,2] <- "Labels" # Umbenennung der zweiten Zelle der Tabelle.
  data_Raw <- data_Raw[, sapply(data_Raw, function(i)!all(is.na(i)))]; # entfernt leere Spalten
  data_Raw[4,] <- gsub("Â","",data_Raw[4,]) # Entfernt das Sonderzeichen aus den Zellen (Y (Âµg/l))
  data_Raw[,2] <- gsub("Â","",data_Raw[,2]) # Entfernt das Sonderzeichen aus den Zellen ( Âµg/l )
  
  # Ermittlung der Einheiten
  Units <- unique(as.character(data_Raw[4,!data_Raw[4,] %in% c(NA)]))
  Units <- gsub("Y ","",Units) # Entfernt das Sonderzeichen aus den Zellen (Y (Âµg/l))
  Units <- gsub("[()]","",Units) # Entfernt das Sonderzeichen aus den Zellen (Y (Âµg/l))
  Units <- Units %>% data.frame(stringsAsFactors = FALSE)#, row.names = c("Concentration","PeakPosition","PeakHight","PeakArea"))
  names(Units) <- "Units"
  
  chrCols <- unique(as.character(data_Raw[1,]))
  myOutput <- list()
  for (i in 3:length(chrCols)) {
    # # Trennung Raw-DataFrames in Drei DataFrames
    
    df <- data_Raw[as.character(data_Raw[1,]) %in% c(chrCols[1],chrCols[2],chrCols[i])]
    
    # Benennung der Spalten
    if(chrCols[i] == "StartTime"){
      names(df)<- c(chrCols[1],chrCols[2],c(df[1,3:ncol(df)]))
    } else { # StartTime hat einen anderen Index der Zeilen 
      names(df) <- c(chrCols[1],chrCols[2],c(df[3,3:ncol(df)]))
    }
    
    
    # Extraktion der eigentlichen Daten
    df   <- df[5:nrow(df),] 
    
    # Konvertierung des Formats
    if (chrCols[i] == "BrigidMS") { # andere Spaltenstruktur
      df[names(df) %in% c("Index","Dilution Factor")] %<>% sapply(function(i)as.numeric(i))
    } else if(chrCols[i] == "StartTime"){ # Datums Format
      df$StartTime %<>% strptime(format = "%FT%T")
    }else {
      df[!names(df) %in% "Labels"] %<>% sapply(function(i)as.numeric(i))
    }
    
    # Analyten
    if (i == 3) {
      Components <- unique(names(df)[3:ncol(df)])
    }
    
    # Benennung des Df's
    myOutput[i-2] <- list(assign(chrCols[i],df))
    
  }
  
  names(myOutput) = chrCols[3:length(chrCols)]
  duration <- StartTime$StartTime[nrow(StartTime)] - StartTime$StartTime[1]
  
  fileName <- basename(Path)
  Path <- normalizePath(Path)
  
  list("Data" = myOutput,
       "Analytes" = Components,
       "Units" = Units,
       "Filename" = fileName, 
       "Path" = Path, 
       "Date" = as.character.Date(StartTime[1,3],format ="%d %b %Y"), 
       "Duration" = duration)
  
}


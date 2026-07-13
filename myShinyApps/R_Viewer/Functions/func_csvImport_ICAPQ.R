
readCSV_iCAPQ <- function(inFile = choose.files(filters = matrix( c("Alle Dateien","*.*","CSV Dateien","*.csv"),
                                                                   nrow = 2,
                                                                   ncol = 2,
                                                                   byrow = T,
                                                                   dimnames = list(c("All", "csv"),c("",""))))){
  
  require("magrittr")
  require("dplyr")
  require("tidyr")
  
  
  
  
  data_Raw <- read.csv(file = inFile,  # OpenFileDialog
                       
                       header = FALSE, # kein Header
                       sep = ",",      # Zellen mit Kommata getrennt
                       fill = TRUE,
                       na.strings = "",
                       blank.lines.skip = TRUE,
                       stringsAsFactors = FALSE
                       );
  
  
  store.Raw <- data_Raw
  
  
  # Bereinigung der Raw-Tabelle
  data_Raw[1,1] <- "Index" # Umbenennung der ersten Zelle der Tabelle.
  data_Raw[1,2] <- "Labels" # Umbenennung der zweiten Zelle der Tabelle.
  data_Raw <- data_Raw[, sapply(data_Raw, function(i)!all(is.na(i)))]; # entfernt leere Spalten
  data_Raw[4,] <- gsub("Â","",data_Raw[4,]) # Entfernt das Sonderzeichen aus den Zellen (Y (Âµg/l))
  data_Raw[,2] <- gsub("Â","",data_Raw[,2]) # Entfernt das Sonderzeichen aus den Zellen ( Âµg/l )
  
  # Ermittlung der Einheiten
  Units.List <-  list()
  
  Units <- unique(as.character(data_Raw[4,!data_Raw[4,] %in% c(NA)]))
  Units <- gsub("Y ","",Units) # Entfernt das Sonderzeichen aus den Zellen (Y (Âµg/l))
  Units <- gsub("[()]","",Units) # Entfernt das Sonderzeichen aus den Zellen (Y (Âµg/l))
  Units <- Units %>% data.frame(stringsAsFactors = FALSE)#, row.names = c("Concentration","PeakPosition","PeakHight","PeakArea"))
  names(Units) <- "Units"
  
  chrCols <- unique(as.character(data_Raw[1,]))
  myOutput <- list()
  for (i in 3:length(chrCols)) {
    # # Trennung Raw-DataFrames in dreispaltige DataFrames
    
    df <- data_Raw[as.character(data_Raw[1,]) %in% c(chrCols[1],chrCols[2],chrCols[i])]
    
    ### Umbenennung der Spalten
    ###########################
    massTraces <- unname(unlist(c(df[3,3:ncol(df)])))
    ## ACHTUNG mit Pt-Spezies Testen!
    tempUnits <- unname(unlist(c(df[4,3:ncol(df)])))
    
     t<- table(massTraces)
    
    if (i == 3) {
      Components <- unique(massTraces)
    }
    
    # Liegt eine eQuant Messung vor?
    if (chrCols[i] == "Raw.Intensity") {
      
      n<- t[[1]]
      
      linseq <- rep(1:n, each=length(t)); 
      # Wiederholende Sequenz:
      # Elemente 1,2,3...n
      # Wiederholungsanzahl each = x
      # x ist definiert durch die Anzahl an verschiedenen Massespuren
      massTraces <- paste0(massTraces,".",linseq)
    }
    
    
    if(chrCols[i] == "StartTime"){ # StartTime hat einen anderen Index der Zeilen 
      massTraces <- df[1,3]
    }
    
    
    
    names(df) <- c(chrCols[1],chrCols[2],massTraces)
    
    # Extraktion der eigentlichen Daten
    df   <- df[5:nrow(df),] 
    
    # Index Spalte als Integer formatieren
    df$Index %<>% as.integer
    # Sortieren nach Index (sollte aber perse so sortiert sein)
    df <- df[order(df$Index),]
    # Index beginnt bei 1 (Falls beim Export einige Zeilen der Sequenz nicht Ausgewertet wurden)
    df$Index <- seq(1:nrow(df))
    
    # Konvertierung des Formats
    if (chrCols[i] == "BrigidMS") { # andere Spaltenstruktur
      df[names(df) %in% c("Dilution Factor")] %<>% sapply(function(i)as.numeric(i))
      
    } else if(chrCols[i] == "StartTime"){ # Datums Format
      df$StartTime %<>% strptime(format = "%FT%T")
    }else {
      df[!names(df) %in% "Labels"] %<>% sapply(function(i)as.numeric(i))
    }
    
    
    # Benennung des Df's
    myOutput[[chrCols[i]]] <- list("Data" = df,
                                    "Analytes" = massTraces,
                                    "Units" = tempUnits)
    
  }
  
  names(myOutput) = chrCols[3:length(chrCols)]
  
  fileName <- basename(inFile)
  Path <- normalizePath(inFile)
  
  list("Raw.Data" = store.Raw,
       "AllData" = myOutput,
       "Analytes" = Components,
       "Units" = Units,
       "Filename" = fileName, 
       "Path" = Path, 
       "Date" = as.character.Date(myOutput$StartTime$Data$StartTime[1],format ="%d %b %Y"))
  
}
 

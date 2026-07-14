
readCSV_iCAPQ_short <- function(inFile = tk_choose.files(filters = matrix( c("CSV Dateien","*.csv","Alle Dateien","*.*"),
                                                                           nrow = 2,
                                                                           ncol = 2,
                                                                           byrow = T,
                                                                           dimnames = list(c("csv","All"),c("","")))),
                                na.strings = c("")){
  
  require("magrittr")
  require("dplyr")
  require("tidyr")
  
  data_Raw <- tryCatch({
    read.csv(file = inFile,
             header = FALSE,
             sep = ",",
             fill = TRUE,
             na.strings = na.strings,
             blank.lines.skip = TRUE,
             stringsAsFactors = FALSE)
  }, error = function(e) {
    if (grepl("multibyte|invalid.*string|encoding", e$message, ignore.case = TRUE)) {
      tryCatch({
        read.csv(file = inFile,
                 header = FALSE,
                 sep = ",",
                 fill = TRUE,
                 na.strings = na.strings,
                 blank.lines.skip = TRUE,
                 stringsAsFactors = FALSE,
                 fileEncoding = "UTF-16LE")
      }, error = function(e2) {
        stop(paste("Cannot read file. File may not be a valid CSV or uses an unsupported encoding.\n",
                   "Original error: ", e$message, "\n",
                   "UTF-16 fallback error: ", e2$message))
      })
    } else {
      stop(e)
    }
  })
  
  
  store.Raw <- data_Raw
  
  
  # Bereinigung der Raw-Tabelle
  data_Raw[1,1] <- "Index" # Umbenennung der ersten Zelle der Tabelle.
  data_Raw[1,2] <- "Labels" # Umbenennung der zweiten Zelle der Tabelle.
  data_Raw[,2] <- gsub("Â","",data_Raw[,2])
  
  data_Raw <- data_Raw[, !na.all(data_Raw)] # na.all() identifiziert leere Spalten
  
  # Ermittlung der Einheiten
  
  chrCols <- unique(as.character(data_Raw[1,]))
  myOutput <- list()
  for (i in 3:length(chrCols)) {
    # # Trennung Raw-DataFrames in Drei DataFrames
    
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
    
    # Gibt es Wiederholungsmessungen?
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
    
    if(chrCols[i] == "iCapOES"){
      massTraces <- c("Label","Main.Runs","Full Frames","Comments","Evaluate","Sample Type","Internal Standard","Standard","Dilution Factor","Amount","Final Quantity","Special Blank")
    }
    
    if(chrCols[i] == "BrigidMS" && df[2,4] == "Chromatography"){
      massTraces <- c("Comments","Sample Type","Standard","Dilution Factor")
    }
      
    names(df) <- c(chrCols[1],chrCols[2],massTraces)
    
    # # Prüfen ob eine Comment Spalte vorhanden ist. Die Bezeichung befindet sich eine Zeile über den anderen 
    # if (chrCols[i] == "BrigidMS" && any(is.na(names(df)))) {
    #   if (df[2,which(is.na(names(df)))] == "Comment"){
    #     names(df)[which(is.na(names(df)))] <- "Comment"
    #   }
    # }
    # Extraktion der eigentlichen Daten
    df   <- df[5:nrow(df),] 
    
    # Index Spalte als Integer formatieren
    df$Index %<>% as.integer
    # Sortieren nach Index (sollte aber perse so sortiert sein)
    df <- df[order(df$Index),]
    # Index beginnt bei 1 (Falls beim Export einige Zeilen der Sequenz nicht Ausgewertet wurden)
    df$Index <- seq(1:nrow(df))
    
    # # Konvertierung des Formats
    if (chrCols[i] == "BrigidMS" || chrCols[i] == "iCapOES") { # andere Spaltenstruktur
      df[names(df) %in% c("Dilution Factor")] %<>% sapply(function(i)as.numeric(i))
    } else  if(chrCols[i] == "StartTime"){ # Datums Format
      df$StartTime %<>% strptime(format = "%FT%T")
    } else {
      df[!names(df) %in% "Labels"] %<>% sapply(function(i)as.numeric(i))
    }
    
    # Benennung des Df's
    myOutput[[chrCols[i]]] <- list("Data" = df,
                                    "Analytes" = massTraces,
                                    "Units" = tempUnits)
    
  }
  
  names(myOutput) = chrCols[3:length(chrCols)]
  #duration <- StartTime$StartTime[nrow(StartTime)] - StartTime$StartTime[1]
  
  fileName <- basename(inFile)
  Path <- normalizePath(inFile)
  
  list("Raw.Data" = store.Raw,
       "AllData" = myOutput)
  
}
 

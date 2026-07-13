
readCSV_iCAPQ <- function(inFile){
  
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
    
    # varOrder <- order(df[3,3:ncol(df)]) + 2
    # varOrder2 <- c(1,2,varOrder)
    # 
    # dfSorted <- df[,varOrder2]
   
    
    ### Umbenennung der Spalten
    ###########################
    massTraces <- unname(unlist(c(df[3,3:ncol(df)])))
    # massTraces <- gsub("[()]","",massTraces);  # entfernt Klammern aus den Labels
    # massTraces <- gsub("\\s",".",massTraces);  # ersetzt die Leerzeichen durch einen "."  
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
    
    # if(chrCols[i] == "ExtCal.StandardConcentration"){
    #   massTraces <- Components
    # }
    
    # # Liegt eine tQuant Messung vor?
    # if(chrCols[i] == "Chromatography.Concentration"){
    #   Components <- massTraces
    # }
    
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
    myOutput[i-2] <- list(assign(chrCols[i],df))
    
  }
  
  names(myOutput) = chrCols[3:length(chrCols)]
  duration <- StartTime$StartTime[nrow(StartTime)] - StartTime$StartTime[1]
  
  fileName <- basename(inFile)
  Path <- normalizePath(inFile)
  
  list("Data" = myOutput,
       "Analytes" = Components,
       "Units" = Units,
       "Filename" = fileName, 
       "Path" = Path, 
       "Date" = as.character.Date(StartTime[1,3],format ="%d %b %Y"), 
       "Duration" = duration)
  
}
 

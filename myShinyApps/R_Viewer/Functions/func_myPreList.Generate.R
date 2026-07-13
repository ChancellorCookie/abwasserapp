
myPreList.Generate <- function(inFile,Device){
  if (Device == "iTeva") {
    t <- read.table(myParams$inFile,blank.lines.skip = T,sep = "\n",dec = ",",skip = 1,skipNul = T,stringsAsFactors = F)
    
    myRawData <- list("Data" = t,
                      "Headers" = unique(t[grep("\\[(.*)\\]",t$V1,perl = T),]), # Extrahiert überschriften, die in eckigen Klammern stehen [...]
                      "FileName" = basename(myParams$inFile),
                      "FilePath" = myParams$inFile)
    
    
    HeaderRows <- c(which(myRawData$Data == myRawData$Headers[1]))
    ResultsRows<- c(which(myRawData$Data == myRawData$Headers[2]))
    InternalStdRows<- c(which(myRawData$Data == myRawData$Headers[3]))
    
    
    firstLoop <- T
    
    for (i in 1:length(HeaderRows)){
      
      ### HEADER ###
      k1 <- HeaderRows[i]+1
      k2 <- ResultsRows[i]-1
      header <- t[k1:k2,1]
      
      header2<-data.frame(strsplit(header, "="),stringsAsFactors = F) 
      header2 %<>% `names<-`(as.character(header2[1,])) %>% slice(-1) 
      # slice funktion: Pick Rows. Negativ Values -> Drop
      
      header2$Index <- i
      
      ### RESULTS ###
      r1 <- ResultsRows[i]+2
      r2 <- InternalStdRows[i]-1
      results <- t[r1:r2,1]
      labels.Results <- unlist(strsplit(t[r1-1,1], ";"))
      results2 <- data.frame(strsplit(results, ";"),stringsAsFactors = F, row.names = labels.Results)
      
      
      # Extraktion der Metadaten
      Elem.Results <- results2["Elem",]
      ISRef.Results <- results2["ISRef",]
      WL.Results <- data.frame(str_split(results2["WL",],"[{}]",3),stringsAsFactors = F) %>% slice(-2:-3)
      Ord.Results <- data.frame(str_split(results2["WL",],"[{}]",3),stringsAsFactors = F) %>% slice(-1) %>% slice(-2)
      
      # Umbenennung der DatenSpalten
      newNames <- paste(Elem.Results,Ord.Results,sep = "_")
      names(results2) <- newNames
      names(Elem.Results) <- newNames
      names(ISRef.Results) <- newNames
      names(WL.Results) <- newNames
      names(Ord.Results) <- newNames
      
      # Extraktion der Messwerte
      Einheiten.Results <- data.frame(results2["Einheiten",]) %>% `names<-`(newNames)
      Mittel.Results <- data.frame(results2["Mittel",]) %>% `names<-`(newNames)
      StdAbw.Results <- data.frame(results2["StdAbw",]) %>% `names<-`(newNames)
      RSD.Results <- data.frame(results2["RSD",]) %>% `names<-`(newNames)
      resultsUnit.Results <- unique(Einheiten.Results)
      
      results2 %<>% slice(-1:(as.numeric(header2$Messwiederh.)-nrow(results2)))
      
      results2$Index <- i
      Mittel.Results$Index <- i
      StdAbw.Results$Index <- i
      RSD.Results$Index <- i
      
      results2 <- myGSubStringDF(results2,"i ","")
      Mittel.Results <- myGSubStringDF(Mittel.Results,"i ","")
      StdAbw.Results <- myGSubStringDF(StdAbw.Results,"i ","")
      RSD.Results <- myGSubStringDF(RSD.Results,"i ","")
      
      
      ### INTERNAL STANDARD ###
      i1 <- InternalStdRows[i]+2
      i2 <- HeaderRows[i+1]-1
      
      if(is.na(i2) & myParams$UseIntCorr){
        i2 <- nrow(t)} # wenn letze Zeile
      if(is.na(i2) & !myParams$UseIntCorr){
        i2 <- i1-1}
      
      if(i2 < i1){ # wenn kein Interner Standard definiert wurde
        myParams$UseIntCorr <- F
      }else {
        
        IntStd <- t[i1:i2,1]
        labels.IntStd <- unlist(strsplit(t[i1-1,1], ";"))
        IntStd2 <- data.frame(strsplit(IntStd, ";"),stringsAsFactors = F,row.names = labels.IntStd)
        
        # Extraktion der Metadaten
        Elem.IntStd <- IntStd2["Elem",]
        WL.IntStd <- data.frame(str_split(IntStd2["WL",],"[{}]",3),stringsAsFactors = F) %>% slice(-2:-3)
        Ord.IntStd <- data.frame(str_split(IntStd2["WL",],"[{}]",3),stringsAsFactors = F)  %>% slice(-1) %>% slice(-2)
        Einheiten.IntStd <- data.frame(IntStd2["Einheiten",])
        
        # Umbenennung der DatenSpalten
        newNames <- paste(Elem.IntStd,Ord.IntStd,sep = "_")
        names(IntStd2) <- newNames
        names(Elem.IntStd) <- newNames
        names(WL.IntStd) <- newNames
        names(Ord.IntStd) <- newNames
        names(Einheiten.IntStd) <- newNames
        
        # Extraktion der Messwerte
        Mittel.IntStd <- data.frame(IntStd2["Mittel",]) %>% `names<-`(newNames)
        StdAbw.IntStd <- data.frame(IntStd2["StdAbw",]) %>% `names<-`(newNames)
        RSD.IntStd <- data.frame(IntStd2["RSD",]) %>% `names<-`(newNames)
        resultsUnit.IntStd <- unique(Einheiten.IntStd)
        
        IntStd2 %<>% slice(-1:(as.numeric(header2$Messwiederh.)-nrow(IntStd2)))
        IntStd2$Index <- i
        Mittel.IntStd$Index <- i
        StdAbw.IntStd$Index <- i
        RSD.IntStd$Index <- i
      }
      
      if(firstLoop){
        headerPreDF <- header2
        
        resultsPreDF<- results2
        Mittel.Results.PreDF <- Mittel.Results
        StdAbw.Results.PreDF <- StdAbw.Results
        RSD.Results.PreDF <- RSD.Results
        
        if(myParams$UseIntCorr){
          IntStdPreDF<- IntStd2
          Mittel.IntStd.PreDF <-  Mittel.IntStd
          StdAbw.IntStd.PreDF <-  StdAbw.IntStd
          RSD.IntStd.PreDF <-  RSD.IntStd}
      }else{
        equalColsHeader <- intersect(names(headerPreDF),names(header2))
        headerPreDF <- rbind(headerPreDF[equalColsHeader],header2[equalColsHeader], 
                             deparse.level = 0, 
                             make.row.names = F,
                             stringsAsFactors = F)
        
        
        
        equalColsResults <- intersect(names(resultsPreDF),names(results2))
        resultsPreDF <- rbind(resultsPreDF[equalColsResults],results2[equalColsResults], 
                              deparse.level = 0, 
                              make.row.names = F,
                              stringsAsFactors = F)
        
        Mittel.Results.PreDF <- rbind(Mittel.Results.PreDF[equalColsResults],Mittel.Results[equalColsResults], 
                                      deparse.level = 0, 
                                      make.row.names = F,
                                      stringsAsFactors = F)
        
        StdAbw.Results.PreDF <- rbind(StdAbw.Results.PreDF[equalColsResults],StdAbw.Results[equalColsResults], 
                                      deparse.level = 0, 
                                      make.row.names = F,
                                      stringsAsFactors = F)
        
        RSD.Results.PreDF <- rbind(RSD.Results.PreDF[equalColsResults],RSD.Results[equalColsResults], 
                                   deparse.level = 0, 
                                   make.row.names = F,
                                   stringsAsFactors = F)
        
        
        if(myParams$UseIntCorr){
          equalColsResults <- intersect(names(IntStdPreDF),names(IntStd2))
          IntStdPreDF <- rbind(IntStdPreDF,IntStd2, 
                               deparse.level = 0, 
                               make.row.names = F,
                               stringsAsFactors = F)
          
          Mittel.IntStd.PreDF <- rbind(Mittel.IntStd.PreDF[equalColsResults],Mittel.IntStd[equalColsResults], 
                                       deparse.level = 0, 
                                       make.row.names = F,
                                       stringsAsFactors = F)
          
          StdAbw.IntStd.PreDF <- rbind(StdAbw.IntStd.PreDF[equalColsResults],StdAbw.IntStd[equalColsResults], 
                                       deparse.level = 0, 
                                       make.row.names = F,
                                       stringsAsFactors = F)
          
          RSD.IntStd.PreDF <- rbind(RSD.IntStd.PreDF[equalColsResults],RSD.IntStd[equalColsResults], 
                                    deparse.level = 0, 
                                    make.row.names = F,
                                    stringsAsFactors = F)}
      }
      
      # Verknüpfung der Header, Results und IntStd Daten
      firstLoop <- F
    }
    
    headerDF <- headerPreDF
    
    resultsDF <- resultsPreDF
    Mittel.Results.DF <- Mittel.Results.PreDF
    StdAbw.Results.DF <- StdAbw.Results.PreDF
    RSD.Results.DF <- RSD.Results.PreDF
    
    if(myParams$UseIntCorr){
      IntStdDF <- IntStdPreDF
      Mittel.IntStd.DF <- Mittel.IntStd.PreDF
      StdAbw.IntStd.DF <- StdAbw.IntStd.PreDF
      RSD.IntStd.DF <- RSD.IntStd.PreDF}
    
    
    # Konvertierung der Datentypen
    headerDF$Messzeit <- as.POSIXct(headerPreDF$Messzeit,format = "%d.%m.%Y %H:%M:%S")
    headerDF$KorrFaktor <- as.numeric(gsub(",",".",headerPreDF$KorrFaktor))
    headerDF$Messwiederh. <- as.numeric(headerPreDF$Messwiederh.)
    
    # Results und Interne Standards müssen in einer for-Loop konvertiert werden, da "as.numeric()" nur spaltenweise funktioniert 
    for (i in 1:ncol(resultsPreDF)) {
      resultsDF[,i] <- as.numeric(gsub(",",".",resultsPreDF[,i]))
      Mittel.Results.DF[,i] <- as.numeric(gsub(",",".",Mittel.Results.DF[,i]))
      StdAbw.Results.DF[,i] <- as.numeric(gsub(",",".",StdAbw.Results.DF[,i]))
      RSD.Results.DF[,i] <- as.numeric(gsub(",",".",RSD.Results.DF[,i]))
    }
    
    if(myParams$UseIntCorr){
      for (i in 1:ncol(IntStdPreDF)) {
        IntStdDF[,i] <- as.numeric(gsub(",",".",IntStdPreDF[,i]))
        Mittel.IntStd.DF[,i] <- as.numeric(gsub(",",".",Mittel.IntStd.DF[,i]))
        StdAbw.IntStd.DF[,i] <- as.numeric(gsub(",",".",StdAbw.IntStd.DF[,i]))
        RSD.IntStd.DF[,i] <- as.numeric(gsub(",",".",RSD.IntStd.DF[,i]))
      }}
    
    # Die Fehlermeldung "NAs introduced by coercion" kann vorerst ignoriert werden
    
    
    # Definition der Kalibration
    LinesResults <- names(resultsDF %>% select(-Index))
    Results.ElementList <- unique(gsub("[[:digit:]]","",str_split_fixed(LinesResults,"_",2)[,1]))
    DefCal_DF <- myParams$Calibration.Definition %>% select(Index,Labels,Results.ElementList)
    
    
    
    if (myParams$UseIntCorr) {
      LinesIntStd <- names(IntStdDF %>% select(-Index))
      IntStd.ElementList <- unique(gsub("[[:digit:]]","",str_split_fixed(LinesIntStd,"_",2)[,1]))
    }
    
    
    
    if(!myParams$UseIntCorr){
      IntStdDF <-NA
      Mittel.IntStd.DF <- NA
      StdAbw.IntStd.DF <- NA
      RSD.IntStd.DF <- NA
      WL.IntStd <- NA
      Ord.IntStd <- NA
      LinesIntStd <- NA
      IntStd.ElementList <- NA
    }
    
    list("System" = data.frame("Device.Software" = "iTeva","Aquisition.Mode"="NA","Export.Version" = "NA",stringsAsFactors = F),
         "Input.Parameter" = myParams,
         "Header" = headerDF,
         "RawData" = myRawData,
         "Measured" = list("Values" = resultsDF,
                           "Means" = Mittel.Results.DF,
                           "SD" = StdAbw.Results.DF,
                           "RSD" = RSD.Results.DF,
                           "ISRef" = ISRef.Results,
                           "Analytes" = c(LinesResults),
                           "Analyte.Elements" = Results.ElementList,
                           "WL" = WL.Results,
                           "Ord" = Ord.Results,
                           "Units" = resultsUnit.Results),
         "IntStd" = list("Perform.IntStd" = myParams$UseIntCorr,
                         "Values" = IntStdDF,
                         "Means" = Mittel.IntStd.DF,
                         "SD" = StdAbw.IntStd.DF,
                         "RSD" = RSD.IntStd.DF,
                         "Standards" = c(LinesIntStd),
                         "IntCorr.Selection.WL" = "auto",
                         "Standard.Elements" = IntStd.ElementList,
                         "WL" = WL.IntStd,
                         "Ord" = Ord.IntStd),
         "Calibration" = list("Definition" = DefCal_DF,
                              "Bestimmungsgrenze.Methode" = myParams$BGMethod,
                              "Unit" = "mg/L"))
    
    
    
  }
  if (Devive == "eQuant"){
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
    #duration <- StartTime$StartTime[nrow(StartTime)] - StartTime$StartTime[1]
    
    fileName <- basename(inFile)
    Path <- normalizePath(inFile)
    
    list("Raw.Data" = store.Raw,
         "AllData" = myOutput)
    
  }
  if (Device == "tQuant"){
    
    
  }
  if (Device == "DMA80evo"){
    
  }
  if (Device == "FIMS400"){
    
  }
  
  return(myPreList.Generate())
  
  
  
}


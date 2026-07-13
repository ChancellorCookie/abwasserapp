
iCAP6000_process <- function(myParams){
  
  
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
    
    if (firstLoop) {
      # Extraktion der Metadaten
      Elem.Results <- results2["Elem",]
      ISRef.Results <- results2["ISRef",]
      WL.Results <- data.frame(str_split(results2["WL",],"[{}]",3),stringsAsFactors = F) %>% slice(-2:-3)
      Ord.Results <- data.frame(str_split(results2["WL",],"[{}]",3),stringsAsFactors = F) %>% slice(-1) %>% slice(-2)
      
      # Umbenennung der DatenSpalten
      newNames <- paste(Elem.Results,Ord.Results,sep = "_")
      newNames <- gsub(" ","",newNames)
      names(Elem.Results) <- newNames
      names(ISRef.Results) <- newNames
      names(WL.Results) <- newNames
      names(Ord.Results) <- newNames
    }
    names(results2) <- newNames
    
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
    # ersetzt das Sonderzeichen "i" bei den Ergebnissen
    results2 <- myGSubStringDF(results2,"i ","")
    Mittel.Results <- myGSubStringDF(Mittel.Results,"i ","")
    StdAbw.Results <- myGSubStringDF(StdAbw.Results,"i ","")
    RSD.Results <- myGSubStringDF(RSD.Results,"i ","")
    
    
    ### INTERNAL STANDARD ###
    i1 <- InternalStdRows[i]+2
    i2 <- HeaderRows[i+1]-1
    
    if(is.na(i2) & myParams$UseIntCorr){i2 <- nrow(t)} # wenn letze Zeile
    if(is.na(i2) & !myParams$UseIntCorr){i2 <- i1-1}
    
    if(i2 < i1){ # wenn kein Interner Standard definiert wurde
      myParams$UseIntCorr <- F
    }else {
      
      IntStd <- t[i1:i2,1]
      labels.IntStd <- unlist(strsplit(t[i1-1,1], ";"))
      IntStd2 <- data.frame(strsplit(IntStd, ";"),stringsAsFactors = F,row.names = labels.IntStd)
      
      # Extraktion der Metadaten
      Elem.IntStd <- IntStd2["Elem",]
      Elem.IntStd.old <- Elem.IntStd
      WL.IntStd <- data.frame(str_split(IntStd2["WL",],"[{}]",3),stringsAsFactors = F) %>% slice(-2:-3)
      Ord.IntStd <- data.frame(str_split(IntStd2["WL",],"[{}]",3),stringsAsFactors = F)  %>% slice(-1) %>% slice(-2)
      Einheiten.IntStd <- data.frame(IntStd2["Einheiten",])
      
      # Umbenennung der DatenSpalten
      newNames.IntStd <- paste(Elem.IntStd,Ord.IntStd,sep = "_")
      names(IntStd2) <- newNames.IntStd
      names(Elem.IntStd) <- newNames.IntStd
      names(WL.IntStd) <- newNames.IntStd
      names(Ord.IntStd) <- newNames.IntStd
      names(Einheiten.IntStd) <- newNames.IntStd
      
      # Extraktion der Messwerte
      Mittel.IntStd <- data.frame(IntStd2["Mittel",]) %>% `names<-`(newNames.IntStd)
      StdAbw.IntStd <- data.frame(IntStd2["StdAbw",]) %>% `names<-`(newNames.IntStd)
      RSD.IntStd <- data.frame(IntStd2["RSD",]) %>% `names<-`(newNames.IntStd)
      resultsUnit.IntStd <- unique(Einheiten.IntStd)
      
      IntStd2 %<>% slice(-1:(as.numeric(header2$Messwiederh.)-nrow(IntStd2)))
      IntStd2$Index <- i
      Mittel.IntStd$Index <- i
      StdAbw.IntStd$Index <- i
      RSD.IntStd$Index <- i
      
      # Ersetzen der ISRef (Automatisch eingelesen) durch neue Namen
      for (i in 1:length(Elem.IntStd)) {
        ISRef.Results[grepl(pattern = Elem.IntStd[[i]],x = ISRef.Results)] <- names(Elem.IntStd)[i]
      }
      
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
  # Warnmeldungen sind hier normal, wenn aus nicht numerischen Zeichenketten NAs erzeugt werden.
  # Alternative ist die Fuktion type.convert() allerdings müssen hier die Sonderzeichen definiert werden.
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
  Results.ElementList <- gsub("[[:digit:]]","",str_split_fixed(LinesResults,"_",2)[,1])
  
  
  # Erstellung einer Vollständigen Kalibrationsdefinition anhand Elementinformation
  DefCal_DF <- myParams$Calibration$definition %>% select(Index,Labels)
  
  for (v in names(myParams$Calibration$definition)) { # Schleife für Anzahl definierter Elemente
    ### Abfrage nach Wellenlänge gleichen Elements
    for (w in grep(pattern = v, x = LinesResults,value = T)) {
      DefCal_DF[[w]] <- myParams$Calibration$definition[[v]]
    }
     
  }
  
  
  
  
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
    Int.Corr.Factors <- NA
  }
  
  myParams[["Date"]] <- as.character.Date(headerDF$Messzeit[1],format = "%d %b %Y")
  myParams[["FileName"]] <- basename(myParams$inFile)
  myParams[["FilePath"]] <- gsub(myParams$FileName,"",myParams$inFile)
  
  list("System" = data.frame("Device.Software" = "iTeva","Aquisition.Mode"="iTeva","Export.Version" = "NA",stringsAsFactors = F),
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
                       #"Int.Corr.Factors" = Int.Corr.Factors,
                       "Standard.Elements" = IntStd.ElementList,
                       "WL" = WL.IntStd,
                       "Ord" = Ord.IntStd),
       "Calibration" = list("Definition" = DefCal_DF,
                            "Bestimmungsgrenze.Methode" = myParams$BGMethod,
                            "Unit" = "mg/L", # must be given by myParams!!!!!
                            "Info" = list("Analytes" = LinesResults,
                                          "Elements" = Results.ElementList,
                                          "Units" = rep("mg/L",length(LinesResults)))))
  
  
  
  
}


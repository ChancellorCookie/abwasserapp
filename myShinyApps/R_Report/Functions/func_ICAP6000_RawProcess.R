
iCAP6000_process <- function(DefCal = data.frame("V1" = c( 0.0 , 0.5 , 1 , 5 )),
                             CalUnit = "mg/L",
                             BG_Method = "DIN32645",
                             Operator = "Dronov",
                             chkIntStd = T,
                             Significant.Digits = 3,
                             IntCorr.Selection.WL){
  require("magrittr")
  require("dplyr")
  require("tidyr")
  
  source('func_ICAP6000_txtRead.R')
  
  myRawData <- iCAP6000_readtxt()
  
  t <- myRawData$Data
  
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
    Elem.Results <- results2["Elem",] %>% `names<-`(results2["Elem",])
    names(results2) <- Elem.Results
    ISRef.Results <- results2["ISRef",]
    WL.Results <- data.frame(str_split(results2["WL",],"[{}]",3),stringsAsFactors = F) %>% `names<-`(Elem.Results) %>% slice(-2:-3)
    Ord.Results <- data.frame(str_split(results2["WL",],"[{}]",3),stringsAsFactors = F) %>% `names<-`(Elem.Results) %>% slice(-1) %>% slice(-2)
    Einheiten.Results <- data.frame(results2["Einheiten",]) %>% `names<-`(Elem.Results)
    Mittel.Results <- data.frame(results2["Mittel",]) %>% `names<-`(Elem.Results)
    StdAbw.Results <- data.frame(results2["StdAbw",]) %>% `names<-`(Elem.Results)
    RSD.Results <- data.frame(results2["RSD",]) %>% `names<-`(Elem.Results)
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
    
    if(is.na(i2) & chkIntStd){
      i2 <- nrow(t)} # wenn letze Zeile
    if(is.na(i2) & !chkIntStd){
      i2 <- i1-1}
    
    if(i2 < i1){ # wenn kein Interner Standard definiert wurde
      chkIntStd <- F
    }else {
      
      IntStd <- t[i1:i2,1]
      labels.IntStd <- unlist(strsplit(t[i1-1,1], ";"))
      IntStd2 <- data.frame(strsplit(IntStd, ";"),stringsAsFactors = F,row.names = labels.IntStd)
      
      # Extraktion der Metadaten
      Elem.IntStd <- IntStd2["Elem",]
      names(IntStd2) <- Elem.IntStd
      WL.IntStd <- data.frame(str_split(IntStd2["WL",],"[{}]",3),stringsAsFactors = F) %>% `names<-`(Elem.IntStd) %>% slice(-2:-3)
      Ord.IntStd <- data.frame(str_split(IntStd2["WL",],"[{}]",3),stringsAsFactors = F) %>% `names<-`(Elem.IntStd) %>% slice(-1) %>% slice(-2)
      Einheiten.IntStd <- data.frame(IntStd2["Einheiten",]) %>% `names<-`(Elem.IntStd)
      Mittel.IntStd <- data.frame(IntStd2["Mittel",]) %>% `names<-`(Elem.IntStd)
      StdAbw.IntStd <- data.frame(IntStd2["StdAbw",]) %>% `names<-`(Elem.IntStd)
      RSD.IntStd <- data.frame(IntStd2["RSD",]) %>% `names<-`(Elem.IntStd)
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
      
      if(chkIntStd){
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
      
      
      if(chkIntStd){
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
  
  if(chkIntStd){
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
  
  if(chkIntStd){
    for (i in 1:ncol(IntStdPreDF)) {
      IntStdDF[,i] <- as.numeric(gsub(",",".",IntStdPreDF[,i]))
      Mittel.IntStd.DF[,i] <- as.numeric(gsub(",",".",Mittel.IntStd.DF[,i]))
      StdAbw.IntStd.DF[,i] <- as.numeric(gsub(",",".",StdAbw.IntStd.DF[,i]))
      RSD.IntStd.DF[,i] <- as.numeric(gsub(",",".",RSD.IntStd.DF[,i]))
    }}
  
  # Die Fehlermeldung "NAs introduced by coercion" kann vorerst ignoriert werden
  
  
  LinesResults <- names(resultsDF %>% select(-Index))
  Results.ElementList <- unique(gsub("[[:digit:]]","",str_split_fixed(LinesResults,"-",2)[,1]))
  DefCal_DF <- data.frame(DefCal,
                          stringsAsFactors = F)
  if (length(Results.ElementList) > 1) {
    for (i in 2:length(Results.ElementList)) {DefCal_DF <- cbind(DefCal_DF,DefCal) }
  }
     
  names(DefCal_DF) <- Results.ElementList
  
  if (chkIntStd) {
    LinesIntStd <- names(IntStdDF %>% select(-Index))
    IntStd.ElementList <- unique(gsub("[[:digit:]]","",str_split_fixed(LinesIntStd,"-",2)[,1]))
  }
  
  
  
  if(!chkIntStd){
    IntStdDF <-NA
    Mittel.IntStd.DF <- NA
    StdAbw.IntStd.DF <- NA
    RSD.IntStd.DF <- NA
    WL.IntStd <- NA
    Ord.IntStd <- NA
    LinesIntStd <- NA
    IntStd.ElementList <- NA
  }
  
  list("RawData" = myRawData,
       "Header" = headerDF,
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
       "IntStd" = list("Perform.IntStd" = chkIntStd,
                       "Values" = IntStdDF,
                       "Means" = Mittel.IntStd.DF,
                       "SD" = StdAbw.IntStd.DF,
                       "RSD" = RSD.IntStd.DF,
                       "Standards" = c(LinesIntStd),
                       "IntCorr.Selection.WL" = IntCorr.Selection.WL,
                       "Standard.Elements" = IntStd.ElementList,
                       "WL" = WL.IntStd,
                       "Ord" = Ord.IntStd),
       "Calibration" = list("Definition" = DefCal_DF,
                            "Bestimmungsgrenze.Methode" = BG_Method,
                            "Unit" = CalUnit),
       "Report" = list( "Operator" = Operator,
                        "Filename" = myRawData$FileName, 
                        "Path" = myRawData$FilePath, 
                        "Date" = as.character.Date(headerDF$Messzeit[1],format ="%d %b %Y"), 
                        "Duration" = headerDF$Messzeit[2] - headerDF$Messzeit[1],
                        "Significant.Digits" = Significant.Digits))
  
  
  
  
}


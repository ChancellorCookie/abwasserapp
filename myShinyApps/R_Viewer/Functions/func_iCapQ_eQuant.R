iCapQ_eQuant <- function(myPreList,myParams){
  
 
  
  
  ##########################################
  # Restrukturierung der eingelesenen Daten
  ##########################################
  
  ### RawData Sublist ###
  
  # Alle im Exportfile enthaltenen Gruppen
  myPreList.AllData.names <- names(myPreList$AllData) 
  
  
  #### SORTIERUNG DER SURVEY DATEN
  ##################################################
  # Daten, die "Survey" im Namen enthalten
  myPreList.AllData.Survey.names <- 
    myPreList.AllData.names[
      grepl(x = myPreList.AllData.names,
            pattern = ".+?Survey.+?")]
  
  # Wenn Survey nicht existiert werden NA ausgegeben
  myPreList.Survey <- myPreList$AllData[myPreList.AllData.Survey.names]
  
  
  #############
  #Liste wird weiter unterteilt in Raw
  myPreList.AllData.Survey.Raw.names <- 
    myPreList.AllData.Survey.names[
      grepl(x = myPreList.AllData.Survey.names,
            pattern = "Raw.+?")]
  
  myPreList.Survey.Raw <- myPreList$AllData[myPreList.AllData.Survey.Raw.names]
  if(length(myPreList.Survey.Raw) != 0){
    myPreList.Survey.Raw$Info <-  myRawDataExtraction_MS(myPreList.Survey.Raw)
  }else{
      myPreList.Survey.Raw$Info <- NA
  }
  
  #############
  #Liste wird weiter unterteilt in ExtCal
  myPreList.AllData.Survey.ExtCal.names <- 
    myPreList.AllData.Survey.names[
      grepl(x = myPreList.AllData.Survey.names,
            pattern = "ExtCal.+?")]
  
  myPreList.Survey.ExtCal <- myPreList$AllData[myPreList.AllData.Survey.ExtCal.names]
  if(length(myPreList.Survey.ExtCal) != 0){
    myPreList.Survey.ExtCal$Info <-  myRawDataExtraction_MS(myPreList.Survey.ExtCal)
  }else{
    myPreList.Survey.ExtCal$Info <- NA
  }
  
  ##################################################
  
  #### SORTIERUNG DER QUANTI DATEN
  ##################################################
  # Daten, die "Raw" ohne "Survey" im Namen enthalten
  myPreList.AllData.Raw.names <- 
    myPreList.AllData.names[
      grepl(x = myPreList.AllData.names,
            pattern = "Raw.+?") & 
        !grepl(x = myPreList.AllData.names,
               pattern = ".+?Survey.+?")]
  
  myPreList.Raw <- myPreList$AllData[myPreList.AllData.Raw.names]
  # Nur für Raw.Intensity anzuwenden
  # Die Liste wird ersetzt durch eine anders sortierte Liste
  if(length(myPreList.Raw$Raw.Intensity) != 0 ){
    
    # untereinander sortierte Intensitäten
    myPreList.Raw$Raw.Intensity$Data <- 
      dfGather_iCAPQ(myPreList.Raw$Raw.Intensity$Data,
                     myPreList.Raw$Raw.Average$Analytes)
    # zusätzliche Spalte "Wiederholungen"
    
    # Die gemessenen Analyten müssen vom Index befreit werden
    # Die Anzahl an Einheiten stimmt demnach auch nicht mehr überein
    myPreList.Raw$Raw.Intensity$Analytes <- myPreList.Raw$Raw.Average$Analytes
    myPreList.Raw$Raw.Intensity$Units <- myPreList.Raw$Raw.Average$Units
  }
  
  myPreList.Raw$Info <-  myRawDataExtraction_MS(myPreList.Raw)
  
  ##################################################
  
  ##################################################
  # Daten, die "ExtCal" ohne "Survey" im Namen enthalten
  myPreList.AllData.ExtCal.names <- 
    myPreList.AllData.names[
      grepl(x = myPreList.AllData.names,
            pattern = "ExtCal.+?") & 
        !grepl(x = myPreList.AllData.names,
               pattern = ".+?Survey.+?")]
  
  myPreList.ExtCal <- myPreList$AllData[myPreList.AllData.ExtCal.names]
  myPreList.ExtCal$Info <-  myRawDataExtraction_MS(myPreList.ExtCal)
  ##################################################
  
  ######################################################
  
  ### Header Sublist ### 
  
  # Funktion entfernt NA's aus den Spaltenbezeichnungen und ersetzt diese durch Strings
  myPreList$AllData$BrigidMS$Data %<>% 
    add.name.if.na(.,c("Survey.Runs","Main.Runs","Comments"))
  
  
  header <- 
    myPreList$AllData$BrigidMS$Data %>%
    merge(.,myPreList$AllData$StartTime$Data,by = c("Index","Labels"))
  
  header$StartTime %<>% strptime(format ="%Y-%m-%d %H:%M:%S")%<>% as.POSIXct()
  header$`Dilution Factor` %<>% as.numeric()
  header$Index %<>% as.numeric()
  header$Amount %<>% as.numeric()
  header$`Final Quantity` %<>% as.numeric()
  ######################################################
  
  ######################################################
  
  ### Calibration Sublist ###
  
  ### External Standard Sublist ###
  
  ExtCal.which.Analytes <- myPreList.ExtCal$Info %>% filter(!(Units %in% "%"))
  
  Standard.Concentration <- header %>% select(Index,Labels,`Sample Type`) %>%
    merge(myPreList.ExtCal$ExtCal.StandardConcentration$Data,by = c("Index","Labels")) %>% 
    filter(`Sample Type` %in% c("BLK","STD")) %>% select(Index,Labels,ExtCal.which.Analytes$Analytes)
  
  Standard.Raw.Average <- header %>% select(Index,Labels,`Sample Type`) %>%
    merge(myPreList.Raw$Raw.Average$Data,by = c("Index","Labels")) %>% 
    filter(`Sample Type` %in% c("BLK","STD")) %>% select(Index,Labels,ExtCal.which.Analytes$Analytes)
  
  Standard.Raw.Intensity <- header %>% select(Index,Labels,`Sample Type`) %>%
    merge(myPreList.Raw$Raw.Intensity$Data,by = c("Index","Labels")) %>% 
    filter(`Sample Type` %in% c("BLK","STD")) %>% select(Index,Labels,ExtCal.which.Analytes$Analytes)
  
  
  
  ### Internal Standard Sublist ###
  IntStd.which.Standards <- myPreList.ExtCal$Info %>% filter(Units %in% "%")
  
  IntStd.Raw.Average <- myPreList.Raw$Raw.Average$Data %>% select(Index,Labels,IntStd.which.Standards$Analytes)
  IntStd.Raw.Intensity <- myPreList.Raw$Raw.Intensity$Data %>% select(Index,Labels,IntStd.which.Standards$Analytes)
  
  IntStd.Factors.Average <- IntStd.Raw.Average
  IntStd.Factors.Average[IntStd.which.Standards$Analytes] <- 
    sapply(IntStd.Raw.Average[IntStd.which.Standards$Analytes], function(i)myInternalStandardFactors(i))
  
  IntStd.Factors.Intensity <- IntStd.Raw.Intensity
  IntStd.Factors.Intensity[IntStd.which.Standards$Analytes] <- 
    sapply(IntStd.Raw.Intensity[IntStd.which.Standards$Analytes], function(i)myInternalStandardFactors(i))
  
  
  ### Zuordnung der Internen Standards zu den Analyten
  IntStd.Assignment <- myIntStdAssignment(ExtCal.which.Analytes,IntStd.which.Standards)
  
  
  myPreList.ExtCal[["Which.Analytes"]] <- ExtCal.which.Analytes 
  
  myPreList.IntStd <- list("Raw.Average"= IntStd.Raw.Average, 
                        "Average.Factors" = IntStd.Factors.Average, 
                        "Raw.Intensity"= IntStd.Raw.Intensity, 
                        "Intensity.Factors" = IntStd.Factors.Intensity, 
                        "Which.Standard" = IntStd.which.Standards, 
                        "Assignment" = IntStd.Assignment) 
  
  
  myParams[["Date"]] <- as.character.Date(header$StartTime[1],format ="%d %b %Y")
  myParams[["FileName"]] <- basename(myParams$inFile)
  myParams[["FilePath"]] <- gsub(myParams$FileName,"",myParams$inFile)
  
  ###### Output #####
  outList <- list("System" = data.frame("Device.Software" = "QTegra","Aquisition.Mode"="eQuant","Export.Version" = "Export_Dro04_Survey",stringsAsFactors = F),
                  "Input.Parameter" = myParams) 
  
  outList[["Raw.Data"]] <- list("ImportFile" = myPreList$Raw.Data,
                                "PreSorted" = myPreList$AllData,
                                "eQuant" = list("Raw" = myPreList.Raw, 
                                                "ExtCal" = myPreList.ExtCal, 
                                                "IntStd" = myPreList.IntStd,
                                                "Std.Concentration" = Standard.Concentration, 
                                                "Std.Raw.Average" = Standard.Raw.Average, 
                                                "Std.Raw.Intensity" = Standard.Raw.Intensity),
                                "Survey" = list("Raw" = myPreList.Survey.Raw, 
                                                "ExtCal" = myPreList.Survey.ExtCal)) 
  
  outList[["Header"]] <- header[order(header$Index),]  %>% select(-Amount,-`Final Quantity`,-`Special Blank`)
  
  outList[["Calibration"]] <- list("Std.Concentration" = Standard.Concentration, 
                                   "Std.Raw.Average" = Standard.Raw.Average, 
                                   "Std.Raw.Intensity" = Standard.Raw.Intensity)
  
  outList[["Concentration"]] <- list()
  
  outList[["Report"]] <- list()
  
  
  return(outList)
   }

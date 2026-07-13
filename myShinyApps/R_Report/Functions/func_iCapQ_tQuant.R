iCapQ_tQuant <- function(myPreList,myParams){
  
  
  
  ##########################################
  # Restrukturierung der eingelesenen Daten
  ##########################################
  
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
  
  
  ######################################################
  
  ######################################################
  
  
  ### Calibration Sublist ###
  
  ### External Standard Sublist ###
  
  
  
  
  ExtCal.which.Analytes <- names(myParams$Calibration.Definition[-1])
  
  Standard.Concentration <- header %>% select(Index,Labels,`Sample Type`) %>%
    merge(myParams$Calibration.Definition,by = c("Index")) %>% 
    filter(`Sample Type` %in% c("BLK","STD")) %>% select(-`Sample Type`)
  
  Standard.PeakHeight <- header %>% select(Index,Labels,`Sample Type`) %>%
    merge(myPreList$AllData$Chromatography.PeakHeight$Data,by = c("Index","Labels")) %>% 
    filter(`Sample Type` %in% c("BLK","STD")) %>% select(-`Sample Type`)
  
  Standard.PeakArea <- header %>% select(Index,Labels,`Sample Type`) %>%
    merge(myPreList$AllData$Chromatography.PeakArea$Data,by = c("Index","Labels")) %>% 
    filter(`Sample Type` %in% c("BLK","STD")) %>% select(-`Sample Type`)
  
  ######################################################
  
  ######################################################
  
   
  myParams[["Date"]] <- as.character.Date(header$StartTime[1],format ="%d %b %Y")
  myParams[["FileName"]] <- basename(myParams$inFile)
  myParams[["FilePath"]] <- gsub(myParams$FileName,"",myParams$inFile)
  
  ###### Output #####
  outList <- list("Input.Parameter" = myParams) 
  
  outList[["Raw.Data"]] <- myPreList$Raw.Data
  
  outList[["Prepared"]] <- myPreList$AllData
  
  outList[["Header"]] <- header[order(header$Index),] 
  
  outList[["tQuant"]] <- list("Chromatography" = list("Concentration" = myPreList$AllData$Chromatography.Concentration,
                                                      "PeakStart" = myPreList$AllData$Chromatography.PeakStart,
                                                      "PeakEnd" = myPreList$AllData$Chromatography.PeakEnd,
                                                      "Retention" = myPreList$AllData$Chromatography.Retention,
                                                      "PeakHeight" = myPreList$AllData$Chromatography.PeakHeight,
                                                      "PeakArea" = myPreList$AllData$Chromatography.PeakArea,
                                                      "Chromatogramm" = myPreList$AllData$MainRuns),
                              "Calibration" = list("Std.Concentration" = Standard.Concentration,
                                                   "Std.PeakHeight" = Standard.PeakHeight,
                                                   "Std.PeakArea" = Standard.PeakArea))
  
  
  return(outList)
}

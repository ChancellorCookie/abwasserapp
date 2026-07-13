myDMA80 <- function(myParams){
  
  myDF_Raw <- suppressWarnings(readCSV_DMA80evo(inFile = myParams$inFile))
  myDF <- myDF_Raw
  names(myDF)[names(myDF) == "ProcessTime" ] <- "StartTime"
  myDF$CreationDate %<>% as.POSIXct(format = "%Y-%m-%d-%H-%M-%S")
  
  myDF$StartTime  %<>% as.POSIXct(format = "%Y-%m-%d-%H-%M-%S")
  
  myDF <- myDF[complete.cases(myDF$Concentration),]
  
  # Sonderfall:
  if(sum(myDF$Concentration == "Out of range") > 0){
    myDF[myDF$Concentration == "Out of range",c("Concentration","Hg..ng.")] <- NA
    myDF$Hg..ng. %<>% str_replace_all(.,",",".")
    myDF$Concentration %<>% str_replace_all(.,",",".")
    myDF$Hg..ng. %<>% as.numeric()
    myDF$Concentration %<>% as.numeric()
  }
  
  header <- myDF %>% select(Nr,Pos,SampleName,StartTime,Amount, Unit, MethodFile, Cell, CalFactor, Remark)
  
  output <- list("System" = list("Device.Software" = "DMA80evo Package",
                                 "Acquisition.Mode" = "DMA80evo",
                                 "Export.Verion" = "NA"),
                 "Input.Parameter" = myParams,
                 "RawData" = list("Raw" = myDF_Raw,
                                  "RawClean" = myDF),
                 "Header" = header)
  output[["Input.Parameter"]][["Date"]] <- as.character.Date(myDF$StartTime[1],format ="%d %b %Y")
  output[["Input.Parameter"]][["FileName"]] <- basename(myParams$inFile)
  output[["Input.Parameter"]][["FilePath"]] <- dirname(myParams$inFile)
  return(output)
}
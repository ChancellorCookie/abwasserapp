myDMA80.Cali <- function(CaliFile){
  
  myDF_Raw <- suppressWarnings(readCSV_DMA80evo(CaliFile))
  
  myDF <- myDF_Raw
  myDF$CreationDate %<>% as.POSIXct(format = "%Y-%m-%d-%H-%M-%S")
  myDF$ProcessTime  %<>% as.POSIXct(format = "%Y-%m-%d-%H-%M-%S")
  
  myDF <- myDF[complete.cases(myDF$Concentration),]
  
  myDF
}
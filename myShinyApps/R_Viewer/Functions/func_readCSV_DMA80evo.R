readCSV_DMA80evo <- function(inFile = tk_choose.files(filters = matrix( c("CSV Dateien","*.csv","Alle Dateien","*.*"),
                                                                        nrow = 2,
                                                                        ncol = 2,
                                                                        byrow = T,
                                                                        dimnames = list(c("csv","All"),c("",""))))){

  library(readr)


  RawData <- read.delim2(file = inFile,
                         header = T,
                         sep = ";",
                         skip = 2,
                         fileEncoding = "ISO-8859-1",
                         stringsAsFactors = F)
  
  
  return(RawData)
}
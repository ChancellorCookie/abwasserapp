readCSV_DMA80evo <- function(inFile = tk_choose.files(filters = matrix( c("CSV Dateien","*.csv","Alle Dateien","*.*"),
                                                                        nrow = 2,
                                                                        ncol = 2,
                                                                        byrow = T,
                                                                        dimnames = list(c("csv","All"),c("",""))))){

library(readr)

RawData <- read.csv2(inFile,
                     skip = 2,
                     encoding = "ISO-8859-1",
                     stringsAsFactors = F,
                     fill = T)

RawData
}
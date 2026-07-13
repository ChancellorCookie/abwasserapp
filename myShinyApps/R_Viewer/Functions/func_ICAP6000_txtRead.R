iCAP6000_readtxt <- function(){
  
  # Einlesen der Rohdaten im CSV Format
  myFilters <- matrix(c("txt Dateien",
                        "*.txt",
                        "Alle Dateien",
                        "*.*"),
                      nrow = 2,
                      ncol = 2,
                      byrow = T,
                      dimnames = list(c("txt","All"),c("",""))
  )
 
   myfile <- tk_choose.files(filters = myFilters)
  
  t <- read.table(myfile,blank.lines.skip = T,sep = "\n",dec = ",",skip = 1,skipNul = T,stringsAsFactors = F)

  
  list("Data" = t,
       "Headers" = unique(t[grep("\\[(.*)\\]",t$V1,perl = T),]), # Extrahiert überschriften, die in eckigen Klammern stehen [...]
       "FileName" = basename(myfile),
       "FilePath" = myfile
  )
  
}

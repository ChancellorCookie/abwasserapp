myTempFileRemove <- function(path=getwd(),FilePattern = ".cpt"){
  
  # Deleting specific files, e.g. temporary files created by "rmarkdown::render" function while generating PDF report
  rmFiles <- file.remove(normalizePath(dir(path,FilePattern,full.names = T)))
}
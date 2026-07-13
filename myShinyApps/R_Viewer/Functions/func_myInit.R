myInit <- function(){
  require(dplyr)
  require(tidyr)
  require(magrittr)
  require(stringr)
  require(ggplot2)
  require(outliers)
  require(RColorBrewer)
  require(kableExtra)
  require(xlsx)
  require(DT)

  #Unterfunktionen werden geladen
  
  # Directory Structure
  # Ordner Struktur
  path.root <- normalizePath(getwd())
  path.QC <- normalizePath(file.path(path.root,"QC"))
  path.TargetValues <- normalizePath(file.path(path.QC,"Akzeptanzkriterien"))
  path.QCtoWrite <- normalizePath(file.path(path.QC,"Regelkarten"))
  path.Functions <- normalizePath(file.path(path.root,"Functions"))
  path.Calibration <- normalizePath(file.path(path.root,"Calibration"))
  
  
  # Regular Expression "^func.*.R$" defines that only R-Files beginning by "func" and ending with ".R" are loaded
  # Sonderzeichen, die variable Zeichenketten definieren. Siehe CheatSheet
  funcs <- normalizePath(dir(path.Functions,"^func.*.R$",full.names = T))
  for (i in funcs) {
    source(i,encoding = 'UTF-8')
  }
}
check.userid <- function(Filename,userid){
  # Funktion zum Abgleich der LogIn Daten
  
  # Öffnen der User Datei und Filter nach username
  id.stored <- read.csv(file = Login.File.Names(Filename)$UserFile,stringsAsFactors = F) %>% filter(id %in% userid)
  # Wenn Kennung nicht in Datei gespeichert ist
  if (nrow(id.stored) == 0){
    return(FALSE)
  }
  # Sonst
  return(TRUE)
  
}

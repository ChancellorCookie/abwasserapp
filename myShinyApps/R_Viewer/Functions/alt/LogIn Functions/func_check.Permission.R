check.Permission <- function(UserName){
  # Funktion zum Abgleich der LogIn Daten
  
  # Öffnen der User Datei und Filter nach UserName
  id.stored <- read.csv(file = "R_Viewer.usr",stringsAsFactors = F) %>% filter(username %in% UserName)
  # Wenn Kennung nicht in Datei gespeichert ist
  if (nrow(id.stored) == 0){ 
    return(FALSE)
  }
  # Sonst
  return(id.stored$group)
}
  
check.login <- function(FileName,UserName,pwHash){
  # Funktion zum Abgleich der LogIn Daten
  
  # Prüfung auf Vorhandensein einer Login Datei mit zugehöriger Passwort-Datei
  if (!file.exists(Login.File.Names(FileName)$UserFile) | !file.exists(Login.File.Names(FileName)$PWFile)) {
    # Die Login.File.Create() Funktion verlangt einen Namen ohne Format Endung (*.usr)
    # Diese Funktion erstellt die *.usr und *.pw Datei mit einem initialen Benutzer
    # Benutzerkennung "admin"
    # Passwort "admin"
    Login.File.Create(FileName)
  }
  
  # Öffnen der User Datei und Filter nach username
  id.stored <- read.csv(file = Login.File.Names(FileName)$UserFile,stringsAsFactors = F) %>% filter(username %in% UserName)
  pwHash.stored <- read.csv(file = Login.File.Names(FileName)$PWFile,stringsAsFactors = F) %>% filter(id %in% id.stored$id)
  
  # Wenn Kennung nicht in Datei gespeichert ist
  if (nrow(id.stored) == 0){ 
    return(FALSE)
  }
  
  # Wenn die Kennung in der Datei vermerkt ist
  password.stored <- pwHash.stored$password
  
  # Wenn Kennung vorhanden ist, erfolgt ein Abgleich des gespeicherten Hash-Werts
  if (password.stored == pwHash) { # Wenn Übereinstimmung vorliegt, wird der Nachname ausgegeben
    return(TRUE)
  }
  
  # Sonst
  return(FALSE)
}
  
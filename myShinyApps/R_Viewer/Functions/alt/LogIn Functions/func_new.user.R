new.user <- function(FileName,Vorname,Nachname,group,password,check.doubles = TRUE){
  
  # Nur Admins dürfen User einfügen, UserIDs oder PW ändern
  #
  
  # Laden des Kryprographie Package für R
  if (!"openssl" %in% installed.packages()){
    install.packages("openssl", dependencies = TRUE)
    require("openssl")
  }
  

  
  # Prüfung auf existenz der User-Datei (initialer Start)
  if (!file.exists(paste0(FileName,".usr")) | !file.exists(paste0(FileName,".pw"))) {
    # Die Login.File.Create() Funktion verlangt einen Namen ohne Format Endung (*.usr)
    # Diese Funktion erstellt die *.usr und *.pw Datei mit einem initialen Benutzer
    # Benutzerkennung "admin"
    # Passwort "admin"
    Login.File.Create(FileName)
  }
  
  
  # Prüfung auf Dubletten
  Laufindex <- 1 # Indizierung der Kennung
  

  while(TRUE){ 
    # Erstellen erstellen der
    
    userid <- paste0(calc.userid(Vorname,Nachname), # Generierung der Kennung
                     formatC(Laufindex, width=2, flag="0"))
    
    # Prüfung, ob die Kennung bereits registriert ist
    if (!isFALSE(check.userid(userid))) {
      
      # Soll ein Dubletten Check drchgeführt werden?
      if (check.doubles){
        # Wenn Kennung bereits registriert ist, wird zusätzlich der Vorname und Nachname geprüft
        if (check.Name(userid)[["Vorname"]] == Vorname & check.Name(userid)[["Nachname"]] == Nachname) {
          return(FALSE) # Beendigung der Funktion durch Ausgabe der FALSE Variable
        }
      }      
      # Falls kein Abbruch eintritt, also die Kombination aus Vorname
      # und Nachname bei gleicher UserID noch nicht registriert sind,
      # wird der Index für die Indizierung der Kennung um 1 erhöht und 
      # die Loop zur Prüfung auf Doppelte Einträge wiederholt
      Laufindex <- Laufindex + 1
      
    } else {
      break 
      # Wenn die check.userid() = FALSE ist, d.h., die Kennung noch nicht vergeben ist,
      # wird die loop beendet und der Code fortgesetzt
    }
  }
  
  
  # Überschreiben der alten User-Datei mit neuen Daten
  write.csv(x = rbind(read.csv(file = "R_Viewer.usr",stringsAsFactors = F),
                      data.frame("id"=userid, # Anhängen des Laufindex als zweistellige Zahl und vorangestellter Null
                                 "Vorname"= Vorname,
                                 "Nachname"=Nachname,
                                 "username"=userid, # Anhängen des Laufindex als zweistellige Zahl und vorangestellter Null
                                 "group" = group,
                                 stringsAsFactors = F)),
            file = "R_Viewer.usr",
            quote = FALSE,
            row.names = FALSE)
  write.csv(x = rbind(read.csv(file = "R_Viewer.pw",stringsAsFactors = F),
                      data.frame("id"=userid, # Anhängen des Laufindex als zweistellige Zahl und vorangestellter Null
                                 "password"= saltedHash(password),
                                 stringsAsFactors = F)),
            file = "R_Viewer.pw",
            quote = FALSE,
            row.names = FALSE)
  return(TRUE)
}

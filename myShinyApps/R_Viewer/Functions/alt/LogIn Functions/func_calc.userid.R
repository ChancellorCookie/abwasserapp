calc.userid <- function(Vorname,Nachname){
  # Erstellung der Benutzerkennung nach Anja Tenberges Prinzip für die IUTA-Plattform
  # z.B. Michail Dronov <=> m_dron
  
  # Entfernen von Sonderzeichen
  Vorname <- gsub(" ","",Vorname)
  Vorname <- gsub("-","",Vorname)
  Nachname <- gsub(" ","",Nachname)
  Nachname <- gsub("-","",Nachname)
  
  # umwandeln in kleine Buchstaben
  a <- tolower(substr(Vorname,1,1))
  b <- tolower(substr(Nachname,1,4))
  
  return(paste(a,b,sep = "_"))
}
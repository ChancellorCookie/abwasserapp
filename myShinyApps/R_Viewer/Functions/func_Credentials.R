#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# CREDENTIALS
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# INFORMATION ----
#__________________
#
# Sammlung aller Funktionen, die für die Benutzung von Credentials (Login Daten) benötigt werden
# Diese Funktionen umfassen:
#   - das Erstellen der benötigten Dateien mit Initalem Benutzer "admin",
#   - das Anlegen und Entfernen eines Users,
#   - das Ändern von Benutzernamen, Password und Freigabe-Gruppe,
#   - den Abgleichen der Credentials mit Eingaben,
#   - Verschlüsserungsfunktionen zum sicheren speichern 
#     der Passwörter auf basis des "openssl" Packages,
#
#
#::::::::::::::::::::::::  
# Voreinstellung:
# Benutzername: "admin"
# Password: "admin"
#::::::::::::::::::::::::
#______________________________________________________________________________________________

# BASIC Functions ----

Login.Package.Load <- function(){
  # Laden des Kryprographie Package für R
  if (!"openssl" %in% installed.packages()){
    install.packages("openssl", dependencies = TRUE)
    require("openssl")
  }
}

saltedHash <- function(pw){
  # Funktion für die Verschlüsselung eines Passworts
  
  # Laden benötigter Packages
  Login.Package.Load()
  
  # Definition des Salt-Wertes
  salt <- ".water123"
  
  return(unclass(as.character(sha256(paste0(pw,salt)))))
}

calc.userid <- function(Vorname,Nachname){
  # Funktion zur Erstellung der Benutzerkennung 
  # nach Anja Tenberge's Vorbild für die IUTA-Webservices
  # z.B. Michail Dronov <=> m_dron
  # im Nachgang wird beim Ablegen eines neuen Users mittels "new.user()" Funktion
  # ein automatisch generierter Laufindex angehängt
  
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

Login.File.Names <- function(Filename){
  # Diese Funktion erstellt die Dateinamen mit entsprechender Endung und gibt diese heraus
  end.usr <- ".usr"
  end.pw <- ".pw"
  
  return(c("UserFile" = paste0(Filename,end.usr), "PWFile" = paste0(Filename,end.pw)))  
}

Login.Data.Create <- function(id,Vorname,Nachname,username,group){
  # Funktion zur Erzeugung eines einzeiligen Dataframes mit definierten Spaltenüberschriften
  return(data.frame("id"=id,
                    "Vorname"= Vorname,
                    "Nachname"=Nachname,
                    "username"=username,
                    "group" = group,
                    stringsAsFactors = F))
}

Login.File.Init <- function(Filename){
  # Die Login.File.Init() Funktion verlangt einen Namen ohne Format Endung (*.usr)
  # Diese Funktion erstellt die *.usr und *.pw Datei mit einem initialen Benutzer
  # Benutzerkennung "admin"
  # Passwort "admin"
  
  # Filename darf keine Format Endung (*.usr) enthalten, da diese Endung hier erstellt wird
  
  # Anlegen einer neuen User-Datei mit einem Benutzer "admin" und 
  write.csv(x = Login.Data.Create("a_admi01","","","admin","admin"),
            file = Login.File.Names(Filename)[["UserFile"]],
            quote = FALSE,
            row.names = FALSE)
  
  # Anlegen einer neuen PW-Datei mit einem Benutzer "admin" und 
  # dem Salted-Hash Wert für "admin", das als Initial-Password definiert ist
  write.csv(x = data.frame("id"="a_admi01",
                           "password"=saltedHash("admin"),
                           stringsAsFactors = F),
            file = Login.File.Names(Filename)[["UserFile"]],
            quote = FALSE,
            row.names = FALSE)
  
  UserFile <- dir(path = ".",pattern = paste0(Filename,".usr"),full.names = TRUE)
  PWFile <- dir(path = ".",pattern = paste0(Filename,".pw"),full.names = TRUE)
  return(c("UserFile.Path" = UserFile,"PWFile.Path" = PWFile))
  
}


# NEW Functions ----
new.user <- function(Filename,Vorname,Nachname,group,password,check.doubles = TRUE){
  
  # Nur Admins dürfen User einfügen, UserIDs oder PW ändern
  #
  
  # Laden benötigter Packages
  Login.Package.Load()
  
  # Prüfung auf Vorhandensein einer Login Datei mit zugehöriger Passwort-Datei
  # Nur relevant wenn aus der Konsole aufgerufen wird
  if (all(!file.exists(Login.File.Names(Filename)))) {
    return(FALSE)
  }
  
  
  # Prüfung auf Dubletten
  Laufindex <- 1 # Indizierung der Kennung
  
  
  while(TRUE){ 
    # Erstellen erstellen der
    
    userid <- paste0(calc.userid(Vorname,Nachname), # Generierung der Kennung
                     formatC(Laufindex, width=2, flag="0")) 
    # Anhängen des Laufindex als zweistellige Zahl und vorangestellter Null
    
    # Prüfung, ob die Kennung bereits registriert ist
    if (!isFALSE(check.userid(Filename,userid))) {
      
      # Soll ein Dubletten Check drchgeführt werden?
      if (check.doubles){
        # Wenn Kennung bereits registriert ist, wird zusätzlich der Vorname und Nachname geprüft
        if (check.Name(Filename,userid)[["Vorname"]] == Vorname & check.Name(Filename,userid)[["Nachname"]] == Nachname) {
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
  write.csv(x = rbind(read.csv(file = Login.File.Names(Filename)[["UserFile"]],stringsAsFactors = F),
                      Login.Data.Create(userid,Vorname,Nachname,userid,group)),
            file = Login.File.Names(Filename)[["UserFile"]],
            quote = FALSE,
            row.names = FALSE)
  write.csv(x = rbind(read.csv(file = Login.File.Names(Filename)[["PWFile"]],stringsAsFactors = F),
                      data.frame("id"=userid, 
                                 "password"= saltedHash(password),
                                 stringsAsFactors = F)),
            file = Login.File.Names(Filename)[["PWFile"]],
            quote = FALSE,
            row.names = FALSE)
  return(TRUE)
}

new.group <- function(Filename,UserName,new.Group){
  # Funtion zur Änderung der Freigabe-Gruppe
  
  if (UserName == "admin") { # Diese ID darf NIE geändert werden
    return(FALSE)
  }
  
  # Open User-File
  UserData <- read.csv(file = Login.File.Names(Filename)[["UserFile"]],stringsAsFactors = F)
  
  if (check.username(Filename,UserName)) {
    UserData[grepl(UserName,UserData$username),"group"] <- new.Group
    write.csv(x = UserData,
              file = Login.File.Names(Filename)[["UserFile"]],
              quote = FALSE,
              row.names = FALSE)
    return(TRUE)
  }
  return(FALSE)
}

new.pw <- function(Filename,UserName,newPW){
  # Funktion zum ändern eines Passworts
  #
  # Nur Admins dürfen User einfügen, UserIDs oder PW ändern
  #
  # Wenn das Password des Admins vergessen wurde, hilft nur 
  # noch eine Manuelle Berechnung des SaltedHash-Werts und 
  # Manuelles eintragen in die R_Viewer.R Datei
  
  # Laden benötigter Packages
  Login.Package.Load()
  
  # Open User-File
  UserData <- read.csv(file = Login.File.Names(Filename)[["UserFile"]],stringsAsFactors = F)  
  UserPW <- read.csv(file = Login.File.Names(Filename)[["PWFile"]],stringsAsFactors = F)  
  
  if (check.username(Filename,UserName)) {
    # get userid
    userid <- UserData[grepl(UserName,UserData$username),"id"]
    # Store new password
    UserPW[grepl(userid,UserPW$id),"password"] <- saltedHash(newPW)
    # overwrite User-File  
    write.csv(x = UserPW,
              file = Login.File.Names(Filename)[["PWFile"]],
              quote = FALSE,
              row.names = FALSE)
    
    return(TRUE)
  }
  return(FALSE)
}

new.username <- function(Filename,old.username,new.Username){
  # Funktion zum ändern eines Username
  
  if (old.username == "admin") { # Dieser User darf NIE geändert werden
    return(FALSE)
  }
  
  # Überprüfung auf Admin-Rechte. Wenn nicht, wird die Funktion abgebrochen
  
  UserData <- read.csv(file = Login.File.Names(Filename)[["UserFile"]],stringsAsFactors = F)
  
  if (check.username(Filename,old.username)) {
    UserData[grepl(old.username,UserData$username),"username"] <- new.Username
    write.csv(x = UserData,
              file = Login.File.Names(Filename)[["UserFile"]],
              quote = FALSE,
              row.names = FALSE)
    return(TRUE)
  }
  return(FALSE)
  
}

# REMOVE Functions ----
remove.user <- function(Filename,userid){
  # Funktion zum entfernen eines Users aus den Login-Dateien
  
  # Einlesen der Dateien
  usr <- read.csv(Login.File.Names(Filename)[["UserFile"]],stringsAsFactors = F)
  pw <- read.csv(Login.File.Names(Filename)[["PWFile"]],stringsAsFactors = F)
  
  # Überschreiben der Dateien mit gefilterten Usern
  write.csv(x = usr[!grepl(userid,usr$id),], # Filter für Userkennung
            file = Login.File.Names(Filename)[["UserFile"]],
            quote = FALSE,
            row.names = FALSE)
  write.csv(x = pw[!grepl(userid,pw$id),],
            file = Login.File.Names(Filename)[["PWFile"]],
            quote = FALSE,
            row.names = FALSE)
  return(TRUE)
}

# CHECKS Functions ----
check.Name <- function(Filename,UserName){
  # Funktion zur Ausgabe des vollen Namens
  
  # Öffnen der User Datei und Filter nach UserName
  id.stored <- read.csv(file = Login.File.Names(Filename)[["UserFile"]],stringsAsFactors = F) %>% filter(username %in% UserName)
  # Wenn Kennung nicht in Datei gespeichert ist
  if (nrow(id.stored) == 0){ 
    return(FALSE)
  }
  # Sonst
  return(c("Vorname"= id.stored$Vorname,"Nachname"= id.stored$Nachname))
}

check.userid <- function(Filename,userid){
  # Funktion zum Abgleich des Benutzernamen
  
  # Öffnen der User Datei und Filter nach username
  id.stored <- read.csv(file = Login.File.Names(Filename)[["UserFile"]],stringsAsFactors = F) %>% filter(id %in% userid)
  # Wenn Kennung nicht in Datei gespeichert ist
  if (nrow(id.stored) == 0){
    return(FALSE)
  }
  # Sonst
  return(TRUE)
  
}

check.login <- function(Filename,UserName,pwHash){
  # Funktion zum Abgleich der LogIn Daten
  # Die create.new Variable 
  # Prüfung auf Vorhandensein einer Login Datei mit zugehöriger Passwort-Datei
  # Nur relevant wenn aus der Konsole aufgerufen wird
  if (all(!file.exists(Login.File.Names(Filename)))) {
    return(FALSE)
  }
  
  # Öffnen der User Datei und Filter nach username
  id.stored <- read.csv(file = Login.File.Names(Filename)[["UserFile"]],stringsAsFactors = F) %>% filter(username %in% UserName)
  pwHash.stored <- read.csv(file = Login.File.Names(Filename)[["PWFile"]],stringsAsFactors = F) %>% filter(id %in% id.stored$id)
  
  # Wenn Kennung nicht in Datei gespeichert ist
  if (nrow(id.stored) == 0){ 
    return(FALSE)
  }
  
  # Wenn Kennung zwar in der User Datei vorhanden ist, aber kein entsprechender Eintrag in der PW Datei vorhanden ist
  if (nrow(pwHash.stored) == 0){
    return(FALSE)
  }
  
  # Wenn Kennung vorhanden ist, erfolgt ein Abgleich des gespeicherten Hash-Werts
  if (pwHash.stored$password == pwHash) { # Wenn Übereinstimmung vorliegt, wird der Nachname ausgegeben
    return(TRUE)
  }
  
  # Sonst
  return(FALSE)
}

check.Permission <- function(Filename,UserName){
  # Funktion zur Ausgabe der Freigabe-Gruppe
  
  # Öffnen der User Datei und Filter nach UserName
  id.stored <- read.csv(file = Login.File.Names(Filename)[["UserFile"]],stringsAsFactors = F) %>% filter(username %in% UserName)
  # Wenn Kennung nicht in Datei gespeichert ist
  if (nrow(id.stored) == 0){ 
    return(FALSE)
  }
  # Sonst
  return(id.stored$group)
}

check.username <- function(Filename,UserName){
  # Funktion zum Abgleich der LogIn Daten
  
  # Öffnen der User Datei und Filter nach username
  id.stored <- read.csv(file = Login.File.Names(Filename)[["UserFile"]],stringsAsFactors = F) %>% filter(username %in% UserName)
  # Wenn Kennung nicht in Datei gespeichert ist
  if (nrow(id.stored) == 0){ 
    return(FALSE)
  }
  # Sonst
  return(TRUE)
  
}

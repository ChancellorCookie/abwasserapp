new.pw <- function(UserName,newPW){
  # Funktion zum ändern eines Passworts
  #
  # Nur Admins dürfen User einfügen, UserIDs oder PW ändern
  #
  # Wenn das Password des Admins vergessen wurde, hilft nur 
  # noch eine Manuelle Berechnung des SaltedHash-Werts und 
  # Manuelles eintragen in die R_Viewer.R Datei
  
  # Laden des Kryprographie Package für R
  if (!"openssl" %in% installed.packages()){
    install.packages("openssl", dependencies = TRUE)
    require("openssl")
  }
  
  # Open User-File
  UserData <- read.csv(file = "R_Viewer.usr",stringsAsFactors = F)  
  UserPW <- read.csv(file = "R_Viewer.pw",stringsAsFactors = F)  
  
  if (check.username(UserName)) {
    # get userid
    userid <- UserData[grepl(UserName,UserData$username),"id"]
    # Store new password
    UserPW[grepl(userid,UserPW$id),"password"] <- saltedHash(newPW)
    # overwrite User-File  
    write.csv(x = UserPW,
              file = "R_Viewer.pw",
              quote = FALSE,
              row.names = FALSE)
    
    return(TRUE)
  }
  return(FALSE)
}
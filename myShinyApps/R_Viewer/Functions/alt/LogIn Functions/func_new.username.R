new.username <- function(old.username,new.Username){
  # Funktion zum ändern eines Username
  #
  if (old.username == "admin") { # Diese ID darf NIE geändert werden
    return(FALSE)
  }
  
  # Überprüfung auf Admin-Rechte. Wenn nicht, wird die Funktion abgebrochen
  
  UserData <- read.csv(file = "R_Viewer.usr",stringsAsFactors = F)
  
  if (check.username(old.username)) {
    UserData[grepl(old.username,UserData$username),"username"] <- new.Username
    write.csv(x = UserData,
              file = "R_Viewer.usr",
              quote = FALSE,
              row.names = FALSE)
    return(TRUE)
  }
  return(FALSE)
  
}
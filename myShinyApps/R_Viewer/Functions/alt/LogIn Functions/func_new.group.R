new.group <- function(UserName,new.Group){
  
  if (UserName == "admin") { # Diese ID darf NIE geändert werden
    return(FALSE)
  }
  
  # Open User-File
  UserData <- read.csv(file = "R_Viewer.usr",stringsAsFactors = F)
  
  if (check.username(UserName)) {
    UserData[grepl(UserName,UserData$username),"group"] <- new.Group
    write.csv(x = UserData,
              file = "R_Viewer.usr",
              quote = FALSE,
              row.names = FALSE)
    return(TRUE)
  }
  return(FALSE)
}
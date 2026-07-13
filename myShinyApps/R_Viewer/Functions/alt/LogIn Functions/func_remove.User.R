remove.user <- function(userid){
  
  usr <- read.csv("R_Viewer.usr",stringsAsFactors = F)
  pw <- read.csv("R_Viewer.pw",stringsAsFactors = F)
  
  write.csv(x = usr[!grepl(userid,usr$id),],
            file = "R_Viewer.usr",
            quote = FALSE,
            row.names = FALSE)
  write.csv(x = pw[!grepl(userid,pw$id),],
            file = "R_Viewer.pw",
            quote = FALSE,
            row.names = FALSE)
  return(TRUE)
}
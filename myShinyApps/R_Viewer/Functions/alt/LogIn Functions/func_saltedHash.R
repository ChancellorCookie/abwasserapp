saltedHash <- function(pw){
  
  
  # Definition des Salt-Wertes
  salt <- ".water123"
  
 return(unclass(as.character(sha256(paste0(pw,salt)))))
}
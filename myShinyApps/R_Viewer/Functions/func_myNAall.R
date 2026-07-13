na.all <- function(vec){
  sapply(vec,
         function(i)all(is.na(i)))
}
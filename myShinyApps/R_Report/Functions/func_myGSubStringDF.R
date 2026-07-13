myGSubStringDF <- function(df,pattern,replacement){
  
  df.out <- df
  
  for (i in 1:length(df)) {
    if(is.character(df[,i])){
      df.out[,i] <- gsub("i ","",df[,i])
    }
  }
  df.out
}


myDFround <- function(df,digits=0){
  
  # Filter Column names Index
  dfout <- df[!names(df) == "Index"]
  
  for(col in names(dfout)){
    isNum <- lapply(dfout[col],is.numeric)
    if(isNum[[1]]){
      df[col] <- round(dfout[col],digits)
    }
  }
  return(df)
}

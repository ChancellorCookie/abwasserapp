myDFsignif <- function(df,digits=2){
  
  # Filter Column names Index
  dfout <- df[!names(df) == "Index"]
  
  for(col in names(dfout)){
    isNum <- lapply(dfout[col],is.numeric)
    if(isNum[[1]]){
      #df[col] <- signif(dfout[col],digits)
      df[col] <- format(x = dfout[col],scientific = F,trim = T,digits = digits,nsmall = 0,zero.print = F)
    }
  }
  return(df)
}

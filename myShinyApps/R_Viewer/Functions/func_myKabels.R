myKabel <- function(df,maxCols,repeatCols = NULL,caption  = "Tabelle",row.names = F){
  
  # Names of Cols to Split except the repeatCols
  if (is.null(repeatCols)) {
    colNames <- colnames(df)
  }else{
    colNames <- colnames(df)[colnames(df) != repeatCols]
  }
  
  # count cols to separate 
  nCols <- length(colNames)
  maxCols <- maxCols - length(repeatCols)
  
  if(maxCols <= 0){
    stop("Ungültige Eingabe in myKabel() => maxCols - length(repeatCols) = 0 !!!")
  }
  
  # count loops
  nGanz <- trunc(nCols/maxCols)
  nRest <- nCols - nGanz*maxCols
  if (nRest == 0) {
    nRest <- NULL
  }
  
  a <- 1 # Loop Counter
  for (i in c(rep(maxCols,nGanz),nRest)) {# variable i gets the number of columns to extract until 
    
    extCols <- colNames[1:i] # extracts the maximum number of columns
    
    # Generate the looped Tex table and outputs it
    print(kable(df %>% select(repeatCols,extCols), "latex",longtable = T, booktabs = T, row.names = row.names,caption = paste(caption,"Teil",a)) %>% 
      kable_styling(latex_options = c("repeat_header")))
    
    
    # Drops the processed columns
    colNames <- colNames[is.na(match(colNames,table = extCols))]
    
    # add the counter
    a <- a +1
    }
  
  
}

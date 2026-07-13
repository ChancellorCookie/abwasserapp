add.name.if.na <- function(df,stringtoadd){
  
  temp <- df
  
  na.logic <- names(temp) %>% is.na()
  countNA <- sum(na.logic)
  
  if(is.null(stringtoadd)){
    stringtoadd <- rep("NA",countNA) %>% paste0(.,".",as.character(seq(1,countNA)))
  }
  
  
  if(countNA != length(stringtoadd)){
    # warning("Ein Fehler in der function add.name.if.na(). Die Anzahl an NA's in Spaltennamen stimmt mit der Anzahl an StringsToAdd nicht überein",
    #         immediate. = T,
    #         call. = T)
    return(df) 
  }
  
  dfnames <- names(df)
  dfnames[na.logic] <- stringtoadd
  
  names(df) <- dfnames
  return(df)
  
}
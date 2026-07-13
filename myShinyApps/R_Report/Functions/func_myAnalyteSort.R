myAnalyteSort <- function(df,Kriterium = "Alphanumeric"){
  
  
  df_labels <- names(df)
  Analytes <- df_labels[3:length(df_labels)]
  
  if(Kriterium == "Alphanumeric"){
    
  } else
  if(Kriterium == "Retention"){
    
  } else
  if(Kriterium == "Massnumber"){
    
  } else
    
  if(Kriterium == "Element"){
      
  }else {
    df_order <- seq(3:length(df_labels))
  }
  
  df[c(1,2,df_order)]
}
myRawDataExtraction <- function(mySubList){
  
  out <- NA
  temp <- mySubList[[1]]
  if(length(names(mySubList)) != 0){
    
    Analytes <- names(temp$Data %>% select(-Index,-Labels))
    
    out <- data.frame("Units" = myUnitsFormat(temp$Units),
                      "Analytes" = Analytes,
                      "Elements" = gsub("[[:digit:]]","",str_split_fixed(Analytes,"[()]",3)[,1]),
                      "Masses" = gsub("[[:alpha:]]","",str_split_fixed(Analytes,"[()]",3)[,1]),
                      "Methods" = gsub(" [[:digit:]]","",
                                       as.character(str_split_fixed(Analytes,"[()]",3)[,2])),
                      stringsAsFactors = F) # Bugfix (2018-07-20_Photovoltaik_01.csv)
  }
  return(out)
}
myElementMean <- function(conc,str_Element,Analytes){
  
  conc.Elem <- conc[c(T,T,str_detect(Analytes,str_Element))]
  cElem <- conc.Elem[,1:3]
  
  for (i in 1:nrow(conc)) {
    cElem[i,3] <- conc.Elem[i,] %>% select(-Index,-Labels) %>% unlist() %>% mean()
  }
  cElem
}
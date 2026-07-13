myConc.multiply.VF <- function(conc,VF){
  Analytes <- conc %>% select(-Index,-Labels) %>% names()
  
  concDF <- conc %>% merge(VF,by = "Index")
  f <- concDF$`Dilution Factor`
  for (i in Analytes) {
    conc[,i] <- f*concDF[,i]
  }

  conc
}
  
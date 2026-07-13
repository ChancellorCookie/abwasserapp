myConcFinal <-  function(conc,Analytes,SignifDigits,BGs){
  
  BGs <- signif(BGs,2)
  conc_final <- conc
  conc_final[,Analytes] <- signif(conc[,Analytes],SignifDigits)
  
  for (i in 1:nrow(conc_final)) {
    for(w in Analytes){
      if (conc[i,w] == 0) {
        conc_final[i,w] <- paste0("< BG")
      }
    }
  }
  conc_final
}
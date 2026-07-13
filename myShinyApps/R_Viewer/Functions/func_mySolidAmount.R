mySolidAmount <- function(myList){
  
  # Bezug auf Einwaagen 
  #Einwaage_DF <- myList$Data$BrigidMS %>% select(Index,Labels,Amount,`Final Quantity`) %>% filter(!is.na(Amount))
  
  
  
  
  conc_Mass <- conc_VF %>% filter(Labels %in% Einwaage_DF$Labels)
  
  Amounts <- as.numeric(str_split_fixed(Einwaage_DF$Amount," ",2)[,1])
  FinalAmount <- as.numeric(str_split_fixed(Einwaage_DF$`Final Quantity`," ",2)[,1])
  Einwaage_DF$`Gesamt Konzentration` <- Amounts/FinalAmount
  
  
  Einwaage_DF$`Einheit` <- paste0(mySIUnits(str_split_fixed(Einwaage_DF$Amount," ",2)[,2])$SI_Numerator,
                                  "/",
                                  mySIUnits(str_split_fixed(Einwaage_DF$`Final Quantity`," ",2)[,2])$SI_Numerator)
  
  for (i in 3:ncol(conc_Mass)) {
    conc_Mass[,i] <- conc_Mass[,i]/Einwaage_DF$`Gesamt Konzentration`
  }
  
  
  
  # Ermittlung der resultierenden Einheiten
  conc_Mass_Unit <- paste0(mySIUnits(myUnits[2])$call_Numerator,
                           "/",
                           mySIUnits(unique(Einwaage_DF$Einheit))$call_Numerator)
  
  conc_Mass_Unit_Ratio <- mySIUnits(conc_Mass_Unit)$not_SI_Unit
  
  if (conc_Mass_Unit_Ratio =="ppm") {
    conc_Mass_Unit <- "mg/kg"
  } else if (conc_Mass_Unit_Ratio =="ppb") {
    conc_Mass_Unit <- "µg/kg"
  }
  
  
  
  
  # Dieses Data Frame muss noch nach BG == 0 gefiltert werden und auf signifikante Stellen gerundet werden.
  
  conc_sig_Mass <- conc_Mass
  
  conc_sig_Mass[,3:ncol(conc_sig_Mass)] <- signif(conc_Mass[,3:ncol(conc_Mass)],n) # n wurde oben bereits definiert
  
  conc_final_Mass <- conc_sig_Mass
  
  for (i in 1:nrow(conc_final_Mass)) {
    for(j in 3:ncol(conc_final_Mass)){
      if (conc_sig_Mass[i,j] == 0) {
        conc_final_Mass[i,j] <- "< BG"
      } else{
        conc_final_Mass[i,j] <- formatC(conc_sig_Mass[i,j],digits = n,format = "fg",flag = "#")
        conc_final_Mass[i,j] <- gsub("\\.$","",conc_final_Mass[i,j])
      }
    }
  }
  
}


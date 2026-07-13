myConcFinal <-  function(conc,Analytes,SignifDigits,BGs){
  # Function for reformating the BG cleaned data (less BG == 0)
  # Round on significant values, output "< BG" instead of 0 and 
  # make nice view
  
  
  # conc: dataframe of the form Index, Label, Analyte#1, Analyte#2, ... 
  if (!is.null(BGs)) {
    BGs <- signif(x = BGs,digits = SignifDigits) # round to significant digits
  }
  
  conc_final <- conc # make a copy of original to change only values and keep the structure
  conc_final[,Analytes] <- signif(conc[,Analytes],SignifDigits) # round only values (as.numeric) to significant digits
  
  # nested loops to access every single value
  for (i in 1:nrow(conc_final)) { # 1st loop row-wise (numeric index)
    for(w in Analytes){ # 2nd loop column-wise (access columnnames)
      if(is.na(conc[i,w]) || is.null(conc[i,w])){ # if not a number (NA or NULL)
        conc_final[i,w] <- NaN # set Value as infinite
      }else if (conc[i,w] == 0) { # if x == 0 
        conc_final[i,w] <- paste0("< BG") # set less BG
      }else{
        conc_final[i,w] <- formatC(x = conc_final[i,w],
                                   digits = SignifDigits,
                                   format = "fg",
                                   flag = "#")
        conc_final[i,w] <- gsub("\\.$","",conc_final[i,w])
      }
    }
  }
  conc_final
}


mySIUnits <- function(str_Unit="µg/L"){
  #str_Unit <- "mg"
  require("dplyr")
  require("tidyr")
  require("magrittr")
  require("stringr")
  require("ggplot2")
  require("outliers")
  require("RColorBrewer")
  require("kableExtra")
  require("xlsx")
  
  output <- data.frame("SI_Numerator" = as.character(),
                       "SI_Denominator" = as.character(),
                       "Factor" = as.numeric(),
                       "not_SI_Unit" = as.character(),
                       "call_Numerator" = as.character(),
                       "call_Denominator" = as.character(),
                       stringsAsFactors = F)
  
  # Begin: Definition Einheiten ####################################
  
  ## Definition der möglichen Präfixe von Einheiten und deren Faktoren
  df_SI_factors <- data.frame(
    Prefix = c("f","p","n","µ","m","","k"),
    Factor = c(1e-15,1e-12,1e-9,1e-6,1e-3,1e0,1e3),
    stringsAsFactors = F)
  
  df_SI_Units <- data.frame(
    Prefix = c("g","L","l","mol"),
    Factor = c(1e0,1e3,1e3,1e0), # Liter hat den Faktor 1000 weil 1L <=> 1000g
    stringsAsFactors = F)
  
  ## Definition der möglichen nicht SI Einheiten und deren Faktoren
  df_NOT_SI <- data.frame(
    Unit = c("ppq","ppt","ppb","ppm","‰","%"),
    Factor = c(1e-15,1e-12,1e-9,1e-6,1e-3,1e-2),
    stringsAsFactors = F)
  
  # Ende: Definition Einheiten #####################################
  
  for (i in 1:length(str_Unit)) {
    
    
    if(is.na(match(str_Unit[i],df_NOT_SI$Unit))){ # Prüfung ob eine NOT_SI Einheit vorliegt
      # Es liegen SI Einheiten vor
      nummerator <- unlist(strsplit(str_Unit[i], "/"))[1]
      
      prefix_numerator <- substr(nummerator,1,1) # immer das erste Zeichen
      sufix_numerator <- substr(nummerator,2,nchar(nummerator)) # immer ab dem zweite Zeichen
      
      if (sufix_numerator == "") {
        sufix_numerator <- prefix_numerator
        prefix_numerator <- ""
        
        # Prüfung, ob eine der Einheiten "Liter" ist 
        # 1L <=> 1000g
        
        if (toupper(sufix_numerator) == "L") {
          num_Factor_n <- 1e3 
        } else {
          num_Factor_n <- 1e0
        }
        
      } else{
        num_Factor_n <- df_SI_factors[which(df_SI_factors == prefix_numerator),2]
        
        if (length(num_Factor_n) == 0) { # which gibt eine 0 als index zurück, wenn nicht vorhanden
          print("ERROR: Prefix of Numerator is not valid")
        }
      }
      
      if(grepl("/",str_Unit[i])){ # Prüfung auf vorhanden sein eines Nenners
        
        denominator <- unlist(strsplit(str_Unit[i], "/"))[2]
        
        prefix_denominator <- substr(denominator,1,1) # immer das erste Zeichen
        sufix_denominator <- substr(denominator,2,nchar(denominator)) # immer ab dem zweite Zeichen
        
        if (sufix_denominator == "") {
          sufix_denominator <- prefix_denominator
          prefix_denominator <- ""
          
          # Prüfung, ob eine der Einheiten "Liter" ist 
          # 1L <=> 1000g
          
          if (toupper(sufix_denominator) == "L") {
            num_Factor_d <- 1e3 
          } else  {
            num_Factor_d <- 1e0 
          }
          
          
        } else{
          num_Factor_d <- df_SI_factors[which(df_SI_factors == prefix_denominator),2]
          
          if (length(num_Factor_n) == 0) {
            print("ERROR: Prefix of Denominator is not valid")
          }
        }
        
        
        num_Factor <- num_Factor_n/num_Factor_d
        num_NOT_SI <- df_NOT_SI[which(df_NOT_SI$Factor == num_Factor),1]
        
        
        
      } else {
        sufix_denominator <- ""
        num_Factor <- num_Factor_n
        num_NOT_SI <- df_NOT_SI[which(df_NOT_SI$Factor == num_Factor),1]
        
        
      }
    } else { # Wenn eine NOT_SI Einheit vorliegt
      
      num_Factor <- df_NOT_SI$Factor[match(str_Unit,df_NOT_SI$Unit)]
      num_NOT_SI <- str_Unit
      
    }
    
    num_NOT_SI <- df_NOT_SI %>% filter(floor(log10(Factor))==floor(log10(num_Factor))) %>% select(Unit) %>% pull()
    if(length(num_NOT_SI) == 0){num_NOT_SI <- NA} # Replace an Character
    
    
    
    output[i,"SI_Numerator"] <- sufix_numerator
    output[i,"SI_Denominator"] <- sufix_denominator
    output[i,"Factor"] <- num_Factor
    output[i,"not_SI_Unit"] <- num_NOT_SI
    
  }
  output
}

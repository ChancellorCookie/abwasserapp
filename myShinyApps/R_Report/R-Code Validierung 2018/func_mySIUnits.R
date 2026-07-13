mySIUnits <- function(chr_Unit="mg"){
   output <- data.frame("SI_Numerator" = as.character(),
                       "SI_Denominator" = as.character(),
                       "Factor" = as.numeric(),
                       "not_SI_Unit" = as.character(),
                       "call_Unit" = as.character(),
                       "call_Numerator" = as.character(),
                       "call_Denominator" = as.character(),
                       stringsAsFactors = F)
 
   # Begin: Definition Einheiten ####################################
  
  ## Definition der möglichen Präfixe von Einheiten und deren Faktoren
  df_SI_prefix <- data.frame(
    Prefix = c("f","p","n","µ","m",NA,"k"),
    Factor = c(1e-15,1e-12,1e-9,1e-6,1e-3,1e0,1e3),
    stringsAsFactors = F)
  
  ## Definition der möglichen nicht SI Einheiten und deren Faktoren
  df_NOT_SI <- data.frame(
    Unit = c("ppq","ppt","ppb","ppm","‰","%"),
    Factor = c(1e-15,1e-12,1e-9,1e-6,1e-3,1e-2),
    stringsAsFactors = F)
  
  # Ende: Definition Einheiten #####################################
  
  
  ### µg/L oder mg/kg
  for (i in 1:length(chr_Unit)) {
    
    str_Unit <- tolower(chr_Unit[i])
    
    df_Units_Strings <- myUnitSplit(str_Unit) # Zerlegung der Einheiten in Zähler, Nenner und deren Pre- und Sufixe
    
    # Ermittlung der Faktoren durch vergleich mit definierten DF's
    df_Units_Values  <- myUnitFactors(df_SI_prefix,df_Units_Strings)
  
  
  num_NOT_SI <- df_NOT_SI %>% filter(floor(log10(Factor))==floor(log10(df_Units_Values$Call))) %>% select(Unit) %>% pull()
  if(length(num_NOT_SI) == 0){num_NOT_SI <- NA} # Replace a Character
  
  output[i,"SI_Numerator"]     <- df_Units_Strings$sufix_Numerator
  output[i,"SI_Denominator"]   <- df_Units_Strings$sufix_Denominator
  output[i,"Factor"]           <- df_Units_Values$Call
  output[i,"not_SI_Unit"]      <- num_NOT_SI
  output[i,"call_Unit"]        <- str_Unit
  output[i,"call_Numerator"]   <- df_Units_Strings$Numerator
  output[i,"call_Denominator"] <- df_Units_Strings$Denominator
  }
  output
}






#####################################################
# Funktionen zum splitten des Einheit strings
#####################################################
myUnitSplit <- function(str_Unit = "µg/mL"){
  #str_Unit = NA
  Unit_df <- data.frame("Call" = as.character(),
                        "Numerator" = as.character(),
                        "prefix_Numerator" = as.character(),
                        "sufix_Numerator" = as.character(),
                        "Denominator" = as.character(),
                        "prefix_Denominator" = as.character(),
                        "sufix_Denominator" = as.character(),
                        stringsAsFactors = F)
  
  Unit_df[1,1] <-str_Unit # Original
  
  Unit_df[1,2] <-unlist(strsplit(str_Unit, "/"))[1] # Numerator
  Unit_df[1,3] <- mySubUnitSplit(Unit_df$Numerator)$Prefix
  Unit_df[1,4] <- mySubUnitSplit(Unit_df$Numerator)$Sufix
  
  Unit_df[1,5] <-unlist(strsplit(str_Unit, "/"))[2] # Denominator
  Unit_df[1,6] <- mySubUnitSplit(Unit_df$Denominator)$Prefix 
  Unit_df[1,7] <- mySubUnitSplit(Unit_df$Denominator)$Sufix
  
  Unit_df
}

#######################

mySubUnitSplit <- function(subUnit_Str = "µg"){
  #subUnit_Str <- "mol"
  if(is.na(subUnit_Str)){ # wenn z.B. absolute Einheiten verwendet werden
    sufix <- NA
    prefix <- NA
   } else {
    
    if (nchar(subUnit_Str) > 2) {
      sufix <- mySubUnitSplit_specials(subUnit_Str) # Identifikation der eigentlichen SI Einheit
      prefix <- gsub(sufix,"",subUnit_Str)
      if(prefix == ""){prefix <- NA}  # Falls kein prefix vorhanden ist, gibt gsub() einen "" zurück
    } else {
      prefix <- substr(subUnit_Str,1,1) # immer das erste Zeichen
      sufix <- substr(subUnit_Str,2,nchar(subUnit_Str)) # immer ab dem zweite Zeichen
      if(sufix == ""){ # wenn zum nur g oder l angegeben ist
        sufix <- prefix
        prefix <- NA
      }
    }
  }
  data.frame("Prefix" = prefix,"Sufix" = sufix,stringsAsFactors = F)
}

#####################################################

mySubUnitSplit_specials <- function(subUnit_Str = "mol"){
  #subUnit_Str <- "Min"
  call <- c("Call"=subUnit_Str)
  
  #########################
  SpecialUnits <- c("mol",
                    "bar",
                    "Pa",
                    "min")
  ########################
  
  SpecialUnits[sapply(SpecialUnits, function(i)grepl(i,call,ignore.case = T))]
}
#####################################################


#####################################################
# Funktion zum berechnen des Konzentrationsverhältnisses
#####################################################

myUnitFactors <- function(df_SI_prefix,df_Units_Strings){
  
  Factor_df <- data.frame("Call" = as.numeric(),
                          "Numerator" = as.numeric(),
                          "prefix_Numerator" = as.numeric(),
                          "sufix_Numerator" = as.numeric(),
                          "Denominator" = as.numeric(),
                          "prefix_Denominator" = as.numeric(),
                          "sufix_Denominator" = as.numeric(),
                          stringsAsFactors = F)
  
  Factor_df[1,"prefix_Numerator"]   <- df_SI_prefix %>% filter(Prefix %in% df_Units_Strings$prefix_Numerator) %>% select(Factor)
  Factor_df[1,"prefix_Denominator"] <- df_SI_prefix %>% filter(Prefix %in% df_Units_Strings$prefix_Denominator) %>% select(Factor)
  
  Factor_df[1,"sufix_Numerator"]   <- 1 
  Factor_df[1,"sufix_Denominator"] <- 1
  
  # Prüfung ob Liter als Sufix definiert ist
  if (!is.na(df_Units_Strings$sufix_Numerator)) {
    if(tolower(df_Units_Strings$sufix_Numerator) == "l")  {Factor_df[1,"sufix_Numerator"]   <- 1000 }
  }
  if (!is.na(df_Units_Strings$sufix_Denominator)) {
    if(tolower(df_Units_Strings$sufix_Denominator) == "l"){Factor_df[1,"sufix_Denominator"] <- 1000 }
  }
  Factor_df[1,"Numerator"]   <- Factor_df[1,"prefix_Numerator"]   * Factor_df[1,"sufix_Numerator"]
  Factor_df[1,"Denominator"] <- Factor_df[1,"prefix_Denominator"] * Factor_df[1,"sufix_Denominator"]
  
  Factor_df[1,"Call"] <- Factor_df[1,"Numerator"] / Factor_df[1,"Denominator"]
  
  Factor_df
}


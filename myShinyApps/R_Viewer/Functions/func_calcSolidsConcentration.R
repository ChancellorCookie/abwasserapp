
calc.Concentration.Solids <- function( # Head function
  df.concVF, # df.concVF is the data.frame with cross-charged dilution faktors. 
  str.concVF.Unit, # a character with SI unit of measured concentrations.
  df.solids){ # df.solids must be a dataframe with identical Labels-Column like df.concVF.
  # This dataframe is generated in myList functions of the different measurement system an is rendered by UI
  
  # df.c_rel = Solid.Total.Relative( # Sub-function to get unitless concentrations
  # df.c_Total = Solid.Total.Concentration(
  #   df.solids))){ # df.solids must be a dataframe with identical Labels-Column like df.concVF.
  #  # This dataframe is generated in myList functions of the different measurement system an is rendered by UI
  # 
  
  # make a copy of the structure of input dataframe
  df.solids.Conc <- df.concVF
  # Calculate the total solid concentrations
  df.c_Total = Solid.Total.Concentration(df.solids)
  
  # Get the total solid concentration value and calculate the unitless relativ value
  t.rel <- df.c_Total$conc.Total * mySIUnits(df.c_Total$Unit.Total)$Factor
  
  # Get the measured value and calculate the unitless relativ value
  c.rel <- cbind(df.concVF["Index"],df.concVF["Labels"],
                      {df.solids.Conc %>% select(-Index,-Labels)}*mySIUnits(str.concVF.Unit)$Factor )
  
  # loop to access single analyte columns
  for (j in names(df.concVF %>% select(-Index,-Labels))) { # column-wise by Columnname
    # get the quotient between measured liquid concentration and total solid concentration
    df.solids.Conc[j] <- c.rel[[j]]/t.rel
  }
  return(df.solids.Conc)
}

#________________________________________________________________________________________________________________________

Solid.Total.Concentration <- function(df.solids){
  # function to calculate the total solids concentration 
  
  df.c_Total <- data.frame("Labels" = df.solids$Labels,
                           "conc.Total" = as.numeric(df.solids$Amount)/as.numeric(df.solids$Volume),
                           "Unit.Total" = paste0(df.solids$Unit.Amount,rep("/",length(df.solids$Labels)),df.solids$Unit.Volume))
  df.c_Total$Unit.Total <- gsub("NA/NA",NA,df.c_Total$Unit.Total)
  
  return(df.c_Total)
}

#________________________________________________________________________________________________________________________

Solid.Total.Relative <- function(df.c_Total = Solid.Total.Concentration(df.solids)){
  # Function to return the pure relative concentration, unitless!
  Fac <- na.omit(unique(mySIUnits(df.c_Total$Unit.Total)))$Factor
  
  df.c_rel <- data.frame("Labels" = df.c_Total$Labels,
                         "conc.Rel" = df.c_Total$conc.Total * Fac)
  return(df.c_rel)  
}

#________________________________________________________________________________________________________________________

conc.rel <- function(conc,unit){
  return(conc*mySIUnits(Unit)$Factor)
}

#________________________________________________________________________________________________________________________

Unit.CrossCharge <- function(Unit.Numerator = "µg/L",Unit.Denominator = "mg/mL"){
  # Function to get the resulted unit of quotient
  
  # Get SI information
  SI.Num <- mySIUnits(Unit.Numerator)
  # SI_Numerator SI_Denominator Factor not_SI_Unit call_Unit call_Numerator call_Denominator
  #            g              l  1e-09         ppb      µg/l             µg                l  
  
  
  SI.Den <- mySIUnits(Unit.Denominator)
  # SI_Numerator SI_Denominator Factor not_SI_Unit call_Unit call_Numerator call_Denominator
  #           g              l  0.001           ‰     mg/ml             mg               ml
  
  
  
  # Check if call_Denominator is from same SI?
  if (SI.Num$SI_Denominator == SI.Den$SI_Denominator) {
    # e.g.    µg/L : mg/mL <=> µg/L : g/L -> µg/g <=> mg/kg <=> ppm | Factor 1e-6
    fac.res <- SI.Num$Factor/SI.Den$Factor
  } else { # If not, a Unit can not be given
    return(NA)
  }
  
  return(fac.res)
}


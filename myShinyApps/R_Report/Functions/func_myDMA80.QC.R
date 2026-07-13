myDMA80.QC <- function(df, QC.filter = "^NIST.+?[[:digit:]].+?[[:lower:]]",TargetValue){
  
  # erfordert eine DataFrame
  
  # Nur Proben
  df <- df %>% filter(str_detect(SampleName,QC.filter))
  
  # Extraktion der Standards
  df$SampleNames.Simple <- as.character(
    str_extract_all(
      df$SampleName,QC.filter,
      simplify= TRUE))
  
  # Berechnung der WFR
  df$WFR <- df$Concentration / TargetValue * 100
  
  QC <- df[which.min(abs(df$Concentration-as.numeric(TargetValue))),]
  if (QC.filter == "BW") {
    QC <- df[which.min(abs(df$Concentration)),]
  }
  
    
  list("QC.Data" = df,
       "QC.Selected" = QC)
}
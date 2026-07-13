myDMA80.Report <- function(df.Smpl,SigDigits,BG){


  df.Smpl$Means %<>% signif(.,SigDigits)
  df.Smpl$RSD %<>% signif(.,SigDigits)
  # # AlphaNumerische Sortierung!
  df.Smpl <- df.Smpl[order(df.Smpl$SampleName),] 
  
  lessBg <- as.numeric(df.Smpl$Means) < BG
  
  df.Smpl[lessBg,"Means"] <- paste("<BG")
  df.Smpl[lessBg,"RSD"] <- NA
  
  names(df.Smpl) <- c("Labels", "Sample Name", "Einheiten","Hg"," rel. Stabw (%)")
  df.Smpl
  
}
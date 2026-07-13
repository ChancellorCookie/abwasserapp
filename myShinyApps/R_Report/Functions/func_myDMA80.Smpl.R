myDMA80.Smpl <- function(df, Smpl.filter = "^M.+?[[:digit:]]/[[:digit:]].+?"){
  
  # erfordert eine DataFrame
  
  # Nur Proben
  df <- df %>% filter(str_detect(SampleName,Smpl.filter))
  
  # Extraktion der M-Nummern
  df$SampleNames.Simple <- as.character(
    str_extract_all(
      df$SampleName,Smpl.filter,
      simplify= TRUE))
  
  # Nur zeilen in denen ein Kommentar steht als Maske für Ergebnisse
  Maske <- df %>% 
    select(SampleNames.Simple,Remark,Concentration,Unit.2) %>% 
    filter(str_detect(Remark,"[[:alnum:]]"))
  
  # Berechnung der Mittelwerte
  temp.mean <- Maske %>% 
    select(SampleNames.Simple,Concentration,Remark,Unit.2) %>% 
    group_by(SampleNames.Simple,Remark,Unit.2) %>% 
    summarise_all(funs(mean))
  
  # Berechnung der Stabw
  temp.Stabw <- Maske %>% 
    select(SampleNames.Simple,Concentration,Remark,Unit.2) %>% 
    group_by(SampleNames.Simple,Remark,Unit.2) %>% 
    summarise_all(funs(sd))
  
  Summarized.Data <- merge(temp.mean,temp.Stabw,by = c("SampleNames.Simple","Remark","Unit.2")) %>% `names<-`(.,c("SampleNames.Simple","Remark","Unit","Means","SD"))
  
  Summarized.Data$RSD <-  Summarized.Data$SD / Summarized.Data$Means *100

  
  list("Samples.Data" = df,
       "Samples.Average" = Summarized.Data)
}
myChromGraph <- function(df,SampleName){
  # Das Data Frame muss die Spalten "x" und "y" enthalten
  
  require("ggplot2")
  
  
  
  g<-ggplot(df,aes(Time, Counts)) +  # mit "+" können mehrere asthetics hinzugefügt werden
    geom_line()+
    
    theme_minimal() + # weißer Hintergrund mit Gittellinien
    labs(y = "Intensity (CPS)", # Achsenbeschriftungen, 
         x = "Time (s)",
         title = "Chromatogramm",
         subtitle = SampleName)
  
  return(g)
}
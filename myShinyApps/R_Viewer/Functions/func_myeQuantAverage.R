myeQuantAverage <- function(myList.Intensity,outlier.test = T, alpha = 0.01, Nalimov = F){
  
  #Analyten
  Analytes <- names(myList.Intensity %>% select(-Index,-Labels,-Wiederholung))
  
  IntMeans <- myList.Intensity %>% 
    select(-Wiederholung) %>% 
    group_by(Index,Labels) %>% 
    summarise_all(funs(mean)) %>% 
    as.data.frame()
  
  
  wdh <- myList.Intensity %>% 
    select(Index,Labels,Wiederholung) %>% 
    group_by(Index,Labels) %>% 
    summarise_all(funs(max)) %>%
    pull()
  
  n1 <- cumsum(wdh)-wdh+1
  n2 <- cumsum(wdh)
  
  
  # Dimensionierung der zu schreibenden Data.Frames
  
  Average.df <- IntMeans
  SD.df <- IntMeans
  RSD.df <- IntMeans
  myOutlier.Test <- list("Perform.Outlier.Test" = outlier.test,
                       "Alpha" = alpha,
                       "Nalimov" = Nalimov)
  
  for (i in 1:length(wdh)) { # Schleife für Anzahl an Proben
    current.sample <- paste(i,IntMeans$Labels[i],sep = ".")
    myCurrentSampleOutlierList <- list()
    
    for (v in Analytes){# Schleife für Anzahl an Analyt
      vec <- myList.Intensity[n1[i]:n2[i],v]
      
      myTempResults <- suppressWarnings(myMean(vec = vec,
                                               outlier.test,
                                               alpha,
                                               Nalimov))
      
      Average.df[i,v]<- round(myTempResults$mean,1)
      SD.df[i,v] <- myTempResults$sd
      RSD.df[i,v] <- myTempResults$rsd
      myCurrentSampleOutlierList[[v]] <- myTempResults$Outlier.Test
    }
    
    myOutlier.Test[[current.sample]] <- myCurrentSampleOutlierList
    
  }
  
  
  list("myAverage" = Average.df,
       "mySD" = SD.df,
       "myRSD" = RSD.df,
       "Outlier.Test" = myOutlier.Test)
}
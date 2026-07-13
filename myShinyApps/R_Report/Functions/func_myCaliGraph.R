myCaliGraph <- function(df,sel_massTrace,myUnits = "µg/L"){
  # Das Data Frame muss die Spalten "x" und "y" enthalten
  
#  source("func_myLinReg.R")
#  require("ggplot2")
#  require("RColorBrewer")
  
  # Die Lineare Regression soll stufenweise vonstatten gehen. 
  # Z.B. soll bei einer Kalibration mit 6 Konzentrationen zunächst eine Regression der ersten 3 Punkte erfolgen.
  # Danach der ersten 4, der ersten 5 und abschließend aller 6 Punkte.
  # Die jeweiligen Regression Parameter werden dann zusammengefasst an mit dem Plot in einer Liste ausgegeben
  
  myColours <- c(brewer.pal(9,"Set1"),brewer.pal(8,"Accent")) # Farbskala für den Plot
  df$Colour <- "#000000"  # (= "black") ## Definition von Farbwerten für den Plot
  
  yUnit <- "CPS"
  xUnit <- myUnits[!match(myUnits,table = c("cps","%","s","cts"),nomatch = 0)]
  
  ### Erstellung des Plots
  
  g<-ggplot(df,aes(xVal, yVal)) +  # mit "+" können mehrere asthetics hinzugefügt werden
  # geom_line(aes(y=mdl$fitted.values), color = "red", linetype = "dashed")
  #sel_massTrace <- names(CalX[i])
    theme_minimal() + # weißer Hintergrund mit Gittellinien
    labs(y = paste0("Intensität (",yUnit,")"),                   # Achsenbeschriftungen, 
                x = paste0("Konzentration (",xUnit,")"),         
                title = "Kalibrierung",                                 # Titel und
                subtitle = sel_massTrace)                              # Subtitel (Name der Massespur)


  lm_eq <- "Points"
  
  df <- df[order(df$xVal),] # Sortieren der Datenpunkte nach aufsteigender Konzentration der Standards # Sortieren der Datenpunkte nach aufsteigender Konzentration der Standards
  valSTD <- unique(df["xVal"]) # Ermittlung der einzelnen Konzentrationen
  for (i in 3:nrow(valSTD)) {
 
    df$Colour[df$xVal %in% valSTD$xVal[i]] <- myColours[i-2] # Spalte für Farbcodierung der Punkte nach Konzentration
    
    
    ### Lineare Regression (partiell)
    sum_LinReg <- myLinReg(df,i)
    Intercept <- sum_LinReg$b0
    Slope <- sum_LinReg$b1
    rSquare <- sum_LinReg$rSquare
   
    
    
    
    g <- g + 
      geom_abline(slope = Slope,
                  intercept = Intercept,
                  colour = myColours[i-2])
    
    
    
    ###### Anzeige der Regressions-Parameter

    
      lm_eq <- c(lm_eq,paste("n =", as.character(i),"\ny =",as.character(round(Intercept,2)),
                             "+",
                             as.character(round(Slope,2)),"x",
                             "\nR =",as.character(round(rSquare,5))))
    
  }
  
  
         
    g <- g + geom_point(aes(colour = factor(df$Colour,levels = unique(df$Colour)))) + 
      scale_colour_manual(values = setNames(unique(df$Colour),unique(df$Colour)),
                          labels = lm_eq) + # setNames erstellt einen Named Vector
      labs(colour = "") +  # Titel der colour Legende
      theme(legend.text = element_text(lineheight = 1.2),
            legend.key.size = unit(3, 'lines')) +
      guides(color = guide_legend(override.aes = list(size=3)))
    
    
    
      
  
  
  return(g)
}
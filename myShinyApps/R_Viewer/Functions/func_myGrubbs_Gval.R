myGrubbs_Gval <- function(alpha,n){
  
# Diese Formel für die Tabellenwerte kann man bei Wikipädia nachlesen.
# qt(alpha, Freiheitsgrad) gibt die Fläche der t-Verteilung zurück.
# za ist der Prüfwert der Nullhypothese (kein Ausßreißer)
return((n-1)/sqrt(n)*sqrt(qt(alpha/2/n,n-2)^2/(n-2+qt(alpha/2/n,n-2)^2)))
}
na.drop <- function(df){ # entfernt leere Spalten
  df <- df[, sapply(df, function(i)!all(is.na(i)))]
}
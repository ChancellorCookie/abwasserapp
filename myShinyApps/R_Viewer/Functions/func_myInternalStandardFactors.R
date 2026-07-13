myInternalStandardFactors <- function(df){ # Erfordert einen einspaltigen DataFrame
  Denominator <- df[1]
  dfFact <- df/Denominator
  dfFact
}
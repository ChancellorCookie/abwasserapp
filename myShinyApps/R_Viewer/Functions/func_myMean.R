myMean <- function(vec,outlier.test=T,alpha =  0.01,TestName = "Grubbs",Nalimov = F){
  # Nalimov Parameter is only vailed for Grubbs-Test in myOutlier() function
  # Check for Consistence and difference in vec
  # No test needed, when less then 3 values are available
  if (sum(!is.na(vec)) < 3) {outlier.test <- F}
  # Store the input vector
  vec2 <- vec
  if (outlier.test){
    # Which Test-Method should be performed
    if (TestName == "Dixon") {
      listOutliers <- myDixon.test(vec)
      listOutliers[["Test.Method"]] <- "Dixon"
      # Generate the outlier-clean vector
      vec2 <- vec[!listOutliers$Outlier.Check]
    } else if (TestName == "Grubbs") {
      # if explicitly Grubbs-Test is chosen by TestName parameter
      listOutliers <- myOutlier(vec,alpha,Nalimov)
      # Generate the outlier-clean vector
      vec2 <- vec[!listOutliers$Outlier.Check$out]
    } else {
      # If not supported TestName was choosen, no test will be performed
      listOutliers <- list("Input" = vec2,
                           "Outlier" = logical(length(vec)),
                           "Test.Method" = paste(TestName,"is not supported"))
    }
  } else {
    # If outlier.test = FALSE
    listOutliers <- list("Input" = vec2,
                         "Outlier" = logical(length(vec)),
                         "Test.Method" = "Parameter outlier.test was set to FALSE")
  } 
  list("mean" = mean(vec2),
       "sd" = sd(vec2),
       "rsd" = sd(vec2)/mean(vec2)*100,
       "Outlier.Test" = listOutliers)
}
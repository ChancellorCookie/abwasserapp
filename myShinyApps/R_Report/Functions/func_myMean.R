myMean <- function(vec,outlier.test=T,alpha =  0.01,Nalimov = T){
  source('func_myOutlier.R')
  source('func_myDixonOutlier.R')
  
  vec2 <- vec
  
  # Check for Consistence and difference in vec
  if(sum(!is.na(vec)) == 0){outlier.test <- F}
  if(max(vec,na.rm = T)-min(vec,na.rm = T) == 0){outlier.test <- F}
  
  if(outlier.test){
    if (length(vec)>30) {
      listOutliers <- myOutlier(vec,alpha,Nalimov)
    }else{
      listOutliers <- myDixon.test(vec)
      listOutliers[["Test.Method"]] <- "Dixon"
    }
    vec2 <- vec[!listOutliers$Outlier]
  }else{
    listOutliers <- list("Input" = vec2,
                         "Outlier" = logical(length(vec)),
                         "Test.Method" = "None")
  } 
  list("mean" = mean(vec2),
       "sd" = sd(vec2),
       "rsd" = sd(vec2)/mean(vec2),
       "Outlier.Test" = listOutliers)
    
}
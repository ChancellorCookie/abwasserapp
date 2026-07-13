save.myList <- function(myList){
  
  myRDATA.path <- gsub(basename(myList$Input.Parameter$inFile),"",myList$Input.Parameter$inFile)
  myRDATA.file <- paste0(str_split_fixed(string = basename(myList$Input.Parameter$inFile),pattern = ".csv",n=2)[1],"_rEval.RData")
  dirsave.myList.RData <- paste0(myRDATA.path,myRDATA.file)
  
  save(myList,file = dirsave.myList.RData)
}
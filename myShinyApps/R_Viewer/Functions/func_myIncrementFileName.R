myIncrementFileName <- function(fileName,ending = ".xlsx"){
  ### Test for not overwriting existing rEval.xlsx files ###
  
  SearchEnding <- paste0(ending,"$")
  FileName.WithoutEnding <- str_split_fixed(fileName,SearchEnding,2)[1]
  
  i<-1
  OverwriteCondition <- T
  while(OverwriteCondition){
    
    if(!file.exists(fileName)){
      OverwriteCondition <- F
    }else{
      fileName <- paste0(FileName.WithoutEnding,
                         "_",
                         as.character(i),
                         ending)
      OverwriteCondition <- T
      i <- i+1
    }
  }
  fileName
}
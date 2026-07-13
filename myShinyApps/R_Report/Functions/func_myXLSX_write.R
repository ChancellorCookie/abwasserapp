myXLSX.write <- function(ItemList,toPath,Override = F){
  
  Items <- names(ItemList)
  
  wb <- createWorkbook()
  for (i in Items) {
    addDataFrame(ItemList[[i]],createSheet(wb,sheetName=i))
  }
  
  if(!Override){toPath <- myIncrementFileName(toPath,".xlsx")}
  
  saveWorkbook(wb,toPath)
}
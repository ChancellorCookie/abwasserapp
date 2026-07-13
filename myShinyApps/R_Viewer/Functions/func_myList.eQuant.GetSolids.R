myList.eQuant.GetSolids <- function(BrigidMS.Solids = myList$Prepared$BrigidMS$Data %>% select(Labels,Amount,`Final Quantity`)){
  # This function converts the stored amount and final quantity values in eQuant export file and creates a data.frame for output in the UI
   
  # generate an empty data.frame for output with pre-defined structure
  output <- data.frame("Labels" = BrigidMS.Solids$Labels,
                       "Amount" = BrigidMS.Solids$Amount,
                       "Unit.Amount" = rep(NA,length(BrigidMS.Solids$Labels)),
                       "Volume" = BrigidMS.Solids$`Final Quantity`,
                       "Unit.Volume" = rep(NA,length(BrigidMS.Solids$Labels)),
                       stringsAsFactors = FALSE)
  
  # loop for splitting the concatenated string in seperate values for amount and unit and store in output data.frame 
  for (i in 1:length(BrigidMS.Solids$Labels)) {
    output$Amount[i] <- str_split_fixed(string = BrigidMS.Solids$Amount[i],pattern = " ",n = 2)[1]
    output$Unit.Amount[i] <- str_split_fixed(string = BrigidMS.Solids$Amount[i],pattern = " ",n = 2)[2]
    output$Volume[i] <- str_split_fixed(string = BrigidMS.Solids$`Final Quantity`[i],pattern = " ",n = 2)[1]
    output$Unit.Volume[i] <- str_split_fixed(string = BrigidMS.Solids$`Final Quantity`[i],pattern = " ",n = 2)[2]
    
  }
  
  # convertion in SI
  output$Unit.Volume <- gsub(pattern = "l",replacement = "L",x = output$Unit.Volume)
  
  # Add a factor dimension to Unit columns for enabling the popup function in UI (rhandsontable package)
  output$Unit.Amount <- factor(output$Unit.Amount,levels = c("kg","g","mg","µg","ng"))
  output$Unit.Volume <- factor(output$Unit.Volume,levels = c("L","mL","µL"))
  
  return(output)
}
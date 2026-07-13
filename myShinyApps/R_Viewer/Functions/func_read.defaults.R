# _____________________________________________________________________________________________________________
# [READ DEFAULT VALUES] ----
# For now a CSV file is read. In future a SQLite query can be defined and cenverted to data.frame
# _____________________________________________________________________________________________________________

define.default.files <- function(){
  file <- file.path(getwd(), "Config", "defaults.csv")
  backup.path <- file.path(getwd(), "Config", "Backups")
  vec <- c("file" = file,"backup.path" = backup.path)
  return(vec)
}

read.defaults <- function(){
  read.csv(file = define.default.files()[["file"]],header = T,na.strings = "",stringsAsFactors = F)  
}

load.defaults <- function(defaults.backup.filename){
  read.csv(file = file.path(define.default.files()[["backup.path"]], defaults.backup.filename), header = T, na.strings = "", stringsAsFactors = F)
}

delete.backup.defaults <- function(defaults.backup.filename){
  filename <- file.path(define.default.files()[["backup.path"]], defaults.backup.filename)
  file.remove(filename)
}

write.defaults <- function(defaults.df){
  default.filename <- define.default.files()[["file"]]
  default.backupname <- file.path(define.default.files()[["backup.path"]], paste0("defaults_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
  
  file.copy(from = default.filename, to = default.backupname, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  write.table(x =  defaults.df, file = default.filename, row.names=F, col.names=T, sep=",",na = "")
  backup <- basename(default.backupname)
  return(backup)
}



get.defaults.Backups <- function(){
  backup.files <- dir(define.default.files()[["backup.path"]])
  if (length(backup.files) == 0) {
    return(NULL)
  }
  return(backup.files)
}

define.defaults.Columns <- function(){
  df <- data.frame("Parameter" = names(read.defaults()),
                   "Attribute" = 
                     c("Single","Logical","Logical","Logical","Single","Single",
                       "Single","Single","Logical","Single","Multiple","Multiple",
                       "Multiple","Single","Single","Multiple","Multiple","Multiple",
                       "Multiple","Multiple","Multiple","Multiple","Multiple","Multiple",
                       "Multiple","Multiple","Multiple","Multiple","Multiple","Single",
                       "Single","Single","Multiple","Logical","Logical","Logical",
                       "Logical","Logical"),stringsAsFactors = F)
  levels(df$Attribute) <- unique(df$Attribute)
  return(df)
 
}

get.defaults.Parameter <- function(Argument = NULL){
  if (is.null(Argument)) {
    return(define.defaults.Columns()[["Parameter"]])
  }
  
  df <- define.defaults.Columns()
  df <- df  %>% filter(Attribute %in% Argument)
  vec <- df[["Parameter"]]
  return(vec)
}

get.defaults.Attribute <- function(Param = NULL){
  if (is.null(Param)) {
    return(define.defaults.Columns()[["Attribute"]])
  }
  
  df <- define.defaults.Columns()
  df <- df  %>% filter(Parameter %in% Param)
  vec <- df[["Attribute"]]
  return(vec)
}


# _____________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________
#
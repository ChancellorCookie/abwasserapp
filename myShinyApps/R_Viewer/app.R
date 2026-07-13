##### TODO-Liste
# QC ändern - Die CSV Zielwert enthält die Zielwerte aller Analyten, die durch eine Funktion getrennt an die QC - Handling Function übergeben werden.
# QC Pfade werden anhand der SAA Nummer automatisch ausgelesen
#
# Startup (Pfade, Packages, Functions, Globals) jetzt in global.R
# NOTE: global.R is not reliably loaded by Shiny Server, so everything is inlined here

# Crash-Logging: capture all output to a dedicated log file
crash_log <- file.path(getwd(), "crash.log")
sink(file = crash_log, split = FALSE, append = TRUE)
# redirect stderr using connection
msg_con <- file(crash_log, open = "a")
sink(msg_con, type = "message")

# Directory Structure ----
path.root <- normalizePath(getwd())
path.QC <- normalizePath(file.path(path.root, "QC"))
path.TargetValues <- normalizePath(file.path(path.QC, "Akzeptanzkriterien"))
path.QCtoWrite <- normalizePath(file.path(path.QC, "Regelkarten"))
path.Functions <- normalizePath(file.path(path.root, "Functions"))
path.Calibration <- normalizePath(file.path(path.root, "Calibration"))

# Load User-Defined Functions ----
funcs <- normalizePath(dir(path.Functions, "^func.*.R$", full.names = TRUE))
for (i in funcs) {
  source(i, encoding = "UTF-8")
}

# Load Packages ----
library(shiny)
library(shinyFiles)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(dplyr)
library(stringr)
library(DT)
library(rhandsontable)
library(tools)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(openssl)
library(magrittr)

# Global Variables ----
df.PipettenListe <- data.frame()
df.ini.PipettenList <- data.frame("Pipetten_ID" = c("1231", "1240", "1241", "1246", "771"),
                                  stringsAsFactors = FALSE)

str.LoginFile <- "R_Viewer"
str.UserNames <- Login.File.Names(str.LoginFile)[["UserFile"]]
str.UserPWs <- Login.File.Names(str.LoginFile)[["PWFile"]]

try({
  invisible(read.defaults())
}, silent = TRUE)



## Header ----
header <- dashboardHeader(title = textOutput('HeaderTitle'))

### SideBar ----
sidebar <- dashboardSidebar(
  uiOutput("LinkToQC"),
  sidebarMenuOutput("menuOut")
)


### Body ----

body <- dashboardBody(
  useShinyjs(),
  # Dynamic FileName as Section in Body
  uiOutput("FileName"),
  # Initial Input Elements on Frontpage for all Users
  uiOutput("initTabs")
)

### ui ----
ui <- fluidPage(
  titlePanel(title = HTML(paste("ZE1 Report Generator", tags$sub("Version 1.1"))),
             windowTitle = "ZE1 Report Generator"),
  #_____________________
  ## BETA Label in Top!
  #_____________________
  # titlePanel(title = HTML(paste("ZE1 Report Generator", tags$b(tags$sub("beta",tags$sup(tags$b("2"))), style = "color: red;"))),
  #            windowTitle = "ZE1 Report Generator"),
  dashboardPage(header,sidebar,body)
)


### server ----

server = function(input, output,session) {
  # Source server logic from modules/
  source("modules/server.R", local = TRUE)
}


### Call ----
shinyApp(ui,server)
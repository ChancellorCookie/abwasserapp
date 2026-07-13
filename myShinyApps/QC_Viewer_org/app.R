#____________________________________________________#
#____________________ QC VIEWER _____________________#
#____________________________________________________#


# Load User-Defined Functions ----
# ______________________________________________
## Directory Structure
# working directory has to be the R_Viewer folder to get access to all functions and config-files
# In Docker: both apps are at same level under /srv/shiny-server/myShinyApps/
R_Viewer_Path <- file.path(dirname(getwd()), "R_Viewer")
if (!dir.exists(R_Viewer_Path)) R_Viewer_Path <- getwd()

path.root <- normalizePath(R_Viewer_Path)
path.QC <- normalizePath(file.path(path.root,"QC"))
path.TargetValues <- normalizePath(file.path(path.QC,"Akzeptanzkriterien"))
path.QCtoWrite <- normalizePath(file.path(path.QC,"Regelkarten"))
path.QCArchiv <- normalizePath(file.path(path.QCtoWrite,"QC_Archiv"))
path.Functions <- normalizePath(file.path(path.root,"Functions"))
path.Calibration <- normalizePath(file.path(path.root,"Calibration"))


# Regular Expression "^func.*.R$" defines that only R-Files beginning by "func" and ending with ".R" are loaded
funcs <- normalizePath(dir(path.Functions,"^func.*.R$",full.names = T))
for (i in funcs) {
  source(i,encoding = 'UTF-8')
}

### Initialise Packages ----
# !!!!!!!!!!!!!!!!!!!!!!!!!!
# ______________________________________________
# Die Installation eines neuen Package MUSS zwingend über das Terminal passieren, 
# da das Package ansonsten nicht in der Shiny-Server Version verfügbar ist
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Beispiel zur Installation des "shiny" Package über das Terminal
# sudo su - -c "R -q -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
#__________________________________________
myPackageList <- c("shiny",
                   "shinyFiles",
                   "shinydashboard",
                   "shinyjs",
                   "shinyalert",
                   "dplyr",
                   "stringr",
                   "DT",
                   "rhandsontable",
                   "tools",
                   "data.table",
                   "ggplot2",
                   "RColorBrewer",
                   "openssl",
                   "magrittr",
                   "xlsx")

MultiPackageInstall(myPackageList)
#__________________________________________


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Beim Hinzufügen neuer Dateien ins R_Viewer Verzeichnis von Shiny-Server, 
# müssen zwingend die Rechte neu vergeben werden, da der Ubuntu Benutzer "shiny"
# sonst keinen Zugriff bei der Ausführung erhält und somit eine Fehlermeldung 
# im Browser erscheint
#
# Terminal Befehl zur Änderung der Zugriffsrechte:
# sudo chmod -R 777 /srv/shiny-server/myShinyApps/R_Viewer/
# ______________________________________________
# ______________________________________________
# ______________________________________________






# Global Variables ----
# ______________________________________________
# ______________________________________________

# NOT REACTIVE!!!
str.LoginFile <- "R_Viewer"
# Functions to determine the filepath of LogIn-files
str.UserNames <- Login.File.Names(str.LoginFile)[["UserFile"]]
str.UserPWs <-  Login.File.Names(str.LoginFile)[["PWFile"]]
# ______________________________________________
# ______________________________________________
# ______________________________________________


## Header ----
#____________
header <- dashboardHeader(title = textOutput('HeaderTitle'))



### SideBar ----
#____________
sidebar <- dashboardSidebar(
  # Dynamically created Items in the sidebar of the Page, depending on permission of credentials.
  # View QC (all)
  # New Method (Admin)
  # Revalidation (Admin)
  sidebarMenuOutput("sidebarMenuOut")
)


### Body ----
#____________

body <- dashboardBody(
  useShinyjs(),
  # Dynamically created Items in the body of the Page, like, Boxes with plots, tables or controls
  uiOutput("initTabs")
)

### ui ----
#____________
ui <- fluidPage(
  # Title of the page ----
  titlePanel(title = HTML(paste("ZE1 QC Report", tags$sub("Version 1.0"))),
             windowTitle = "ZE1 QC Report"),
  
  #_____________ example with sub scripted text ___________________
  
  # titlePanel(title = HTML(paste("ZE1 Report Generator", tags$b(tags$sub("beta",tags$sup(tags$b("2"))), style = "color: red;"))),
  #            windowTitle = "ZE1 Report Generator"),
  #________________________________________________________________
  
  
  # call the Dashboard style
  dashboardPage(header,sidebar,body)
)


### server ----
server = function(input, output,session) {
  
  
  
  # Definition einer Source, deren Änderung einen Observer auslösen kann, wie zum Beispiel die Input$... bei einem Button-Klick
  # In diesem Fall soll die Änderung der R_Viewer.usr und ...pw Datei eine Aktualisierung der Tabelle auslösen.
  
  # Reactive Values, lists and dataframes ----
  vals <- reactiveValues(Operator = NULL,
                         AdminRights = NULL,
                         perform.QC = 0,
                         defaults.backup.files =get.defaults.Backups(),
                         QC.Filename = NULL,
                         QCpath = NULL)
  
  lists <- reactiveValues(QC = NULL)
  
  
  dfs <- reactiveValues(df.defaults.QC = NULL,
                        defaults = NULL,
                        QCAllData = NULL,
                        QCPlot = NULL,
                        QCData = NULL,
                        QCComment = NULL)
  
  
  
  #___________________________________________________________________________________________________________________
  # [AUTHENTIFICATION] Start ----
  
  # Alternative zur selbstgestrikten Lösung
  # https://github.com/PaulC91/shinyauthr/blob/master/inst/shiny-examples/shinyauthr_example/app.R
  # Eleganter, da mit LogOut Funktion
  #___________________________________________________________________________________________________________________
  
  # Schaltet einen Observer ohne zu beobachtende Änderung ein, d.h., wird bei Start des Server aktiviert
  # Dieser Observer wird durch den suspend() Befehl vorläufig deaktiviert, der bei erfolgreicher Authentifizierung aufgerufen wird.
  # Der Observer kann mit dem resume() Befehl wieder reaktiviert werden, sodass eine erneute Abfrage der Credentials gemacht werden kann.
  # Der priority Wert wird auf 10000 gesetzt und besagt, das dieser Observer vor allen anderen aufgerufen wird. Das beschleunigt das Laden der Seite.

  
  
  #------- DEV-MODE -------
  # Zum Beenden des Entwickler-Modus, obs1 auskommentieren (Einschalten des Log-In) und vals$AdminRights kommentieren (werden im Log-In definiert)
  #____________________________________________
  # obs1 <- observe(priority = 10000,x = {
  #   showModal(dataModal.init())
  # })
  vals$AdminRights <- TRUE
  vals$Operator <- "DEV"
  #____________________________________________
  
  
  
  
  # Eine Funktion, die das Dialog-Fenster generiert. Der Parameter failed ist per Default auf FALSE
  # (Funktion kann ohne Eingabe dieses Parameters ausgerufen werden)
  # Wird die Funktion/das Dialog-Fenster mir failed = TRUE aufgerufen, wird eine Fehlermeldung in roter Schrift angezeigt.
  dataModal.init <- function(failed = FALSE) {
    # Das Dialog Fenster für die Eingabe und Prüfung der Credentials wird generiert
    modalDialog(
      title = "Authentification",
      # Zusätzliche Fehlermeldung wenn falsche Credentials eingegeben wurden
      if (failed)
        div(tags$b("Invailed Username or Password", style = "color: red;")),
      # Textfeld für Benutzernamen in Klartext
      textInput("username", "Username:"),
      # Textfeld für Password als Zeichenpunkte
      passwordInput("password", "Password:"),
      # Fußzeile mit Buttons
      footer = tagList(
        #modalButton("Cancel"),
        actionButton("btn.creds.ok", "OK")
      )
    )
  }
  
  
  # Observer, der auf den Klick des Ok Buttons des Credentials Dialog-Fensters reagiert und die Funktion zur Prüfung der Authorität beinhaltet
  observeEvent(input$btn.creds.ok,{
    # Abgreifen der "reactive values" Username und Password aus dem Modal
    isolate({
      Username <- input$username
      Password <- saltedHash(input$password)
    })
    
    # Prüfung ob Benutzername in der R_Viewer.usr Datei hinterlegt ist und mit dem Passwort in der R_Viewer.pw Datei und übereinstimmt
    if (check.login(str.LoginFile,Username,Password)) {
      # # In der Maske wird der Operator mit dem zugehörigen hinterlegten Nachnamen vorausgefüllt
      # output$Initial.Operator <- renderUI({
      #   textInput(inputId = "txt.input.Operator",label = "Operator",value = check.Name(str.LoginFile,Username)[["Nachname"]])
      # })
      vals$Operator <- check.Name(str.LoginFile,Username)[["Nachname"]]
      # Initiale Beschränkung der Freigaben auf das niedrigste Level
      vals$AdminRights <- FALSE
      # Prüfung auf erweiterte Freigabe wird durchgeführt
      if (any(check.Permission(str.LoginFile,Username) == "admin")) {
        # Änderung der Freigabe Variable
        vals$AdminRights <- TRUE
      }
      # Deaktiviert den observer
      obs1$suspend()
      # entfernt das Modal für die Credentials und gibt die eigentlichen Steuerelemente frei
      removeModal()
      # Dokumentation der LogIn's in einer externen log.usr Datei
      write(x = paste(Username,date(),sep = ","),file = "log.usr",append = TRUE)
    } else {
      # Wenn falsche Credentials eingegeben wurden, wird das Dialog-Fenster mit der optionalen Fehlermeldung aufgerufen
      showModal(dataModal.init(failed = TRUE))
    }     
  })
  
  #___________________________________________________________________________________________________________________
  # [AUTHENTIFICATION] End
  
  #_______________________________________________________________________________________________________
  #_______________________________________________________________________________________________________
  #_______________________________________________________________________________________________________
  
  
  
  
  # Read-Out of defaults.csv triggert by vailed credentials ----
  observeEvent(vals$AdminRights,{
    dfs$defaults <- read.defaults()
  })
  
  
  
  # Generate the dynamic sidebar menue tabs (only Names and IDs) ----
  output$sidebarMenuOut <- renderMenu({
    req(!is.null(vals$AdminRights))
    sidebarMenu(id = "sidebar_tabs",
                menuItem("View QC",tabName = "sidebartab_ViewQC", icon = icon("archive")),
                if(vals$AdminRights){menuItem("New Method",tabName = "sidebartab_New_Method", icon = icon("archive"))},
                if(vals$AdminRights){menuItem("Revalidation",tabName = "sidebartab_Revalidation", icon = icon("archive"))}
    )
  })
  
  
  # Generate the dynamic tab content ----
  output$initTabs <- renderUI({
    req(!is.null(vals$AdminRights))
    tabItems(
      # [VIEW QC] tabItems----
     tabItem(tabName = "sidebartab_ViewQC",
              h2("View QC"),
              column(width = 2,
                     fluidRow(
                       box(title = "Controls",width = "200%",
                           selectInput(inputId = "ViewQC_Controls_selectInput_SAA",
                                       label = "SAA Method",
                                       choices = unique(dfs$defaults %>% pull(SAA.Menue))),
                           # dynamic selectInput (Dropdown Item)
                           uiOutput("ViewQC_uiOutput_Controls"))),
                     fluidRow(
                       box(title = "Statistische Bewertung",width = "200%",
                           verbatimTextOutput("ViewQC_statistics"),
                           verbatimTextOutput("collection_txt")
                           ))),
             
             box(title = "Plot",width = 4,
                        plotOutput("ViewQC_Plot"),
                        verbatimTextOutput("ViewQC_Comments")),
                  
             box(title = "Data",width = 6,
                        actionButton(inputId = "btn.comment.qc",label = "Comment the Point"),
                        downloadButton(outputId = "dl.xlsx",label = "Excel"),
                        downloadButton(outputId = "dl.pdf",label = "Report"),
                        if(vals$AdminRights) actionButton(inputId = "btn.delete.qc",label = "Delete Point"),
                        hr(),
                        DTOutput("ViewQC_Data"))
      ),
     # [New_Method] tabItems----
      if(vals$AdminRights) {tabItem(tabName = "sidebartab_New_Method",
                                    h2("Noch in Arbeit")
      )},
     # [Revalidation] tabItems----
      if(vals$AdminRights) {tabItem(tabName = "sidebartab_Revalidation",
                                    fluidPage(
                                      fluidRow(width = 24,
                                               selectInput(inputId = "dropDown.Defaults.SAA.Menu",label = "SAA Method",choices = unique(dfs$defaults %>% pull(SAA.Menue))),
                                               actionButton(inputId = "btn.Defaults.Save",label = "Save"),
                                               actionButton(inputId = "btn.Defaults.Load",label = "Load"),
                                               verbatimTextOutput(outputId = "verb.Defaults.Info")
                                      ),
                                      hr(),
                                      fluidRow(
                                        box(title = "Single Parameters",width = 2,
                                            uiOutput("Single.Defaults")
                                        ),
                                        box(title = "Multiple Parameters",width = 10,
                                            rHandsontableOutput("tbl.Defaults")
                                        )
                                      )
                                    )
      )}
    )
  })
  
  
  # [View QC] Select dynamic sub control ----
  output$ViewQC_uiOutput_Controls <- renderUI({
    selectInput(inputId = "sidebartab_ViewQC_Controls_selectInput_QC",
                label = "QC",choices = unique(dfs$defaults %>% filter(SAA.Menue %in% input$ViewQC_Controls_selectInput_SAA) %>% select(QC.Name)) %>% pull())
  })
  
  
  
  #---------------------------------------------#
  # [View QC] Observer Load and display data ----
  #---------------------------------------------#
  
  # Help-Reactive-Value to enable multiple triggers
  observeEvent(input$sidebartab_ViewQC_Controls_selectInput_QC,{
    
    # Extract selected QC Data from whole defaults file
    dfs$df.defaults.QC <- dfs$defaults %>% 
      filter(SAA.Menue %in% input$ViewQC_Controls_selectInput_SAA) %>% 
      filter(QC.Name %in% input$sidebartab_ViewQC_Controls_selectInput_QC)
    
    # Get the Filename
    vals$QC.Filename <- dfs$df.defaults.QC %>% pull(Filename)
    
    # Generate full path
    vals$QCpath <- normalizePath(file.path(path.QCtoWrite,vals$QC.Filename))
    
    # Help reactiive Value to trigger a re-render of QC Viewer Outputs
    vals$perform.QC <- vals$perform.QC + 1
    
  })
  
  
  # Generate the reactive Objects to be displayed ----
  #____________#
  
  # Observer is triggered by a Help-Reactive-Value to enable multiple triggers
  # new selection:  input$sidebartab_ViewQC_Controls_selectInput_QC 
  # add comment:    input$btn.cmt.qc
  # delete comment: input$ btn.del.qc
  
  observeEvent(vals$perform.QC,{
    # Avoid error while initialization
    req(input$sidebartab_ViewQC_Controls_selectInput_QC)
    
    # Load QC Information
    # Generate the ControlChart
    #-----------------------#
    
    # Read the table from file
    # Convert the columns
    # perfomed by read.QC()
    
    # Generate plot and table
    #-----------------------#
    ## Differenz between VBW and QC
    ## VBW plots Concentration
    ## QC plots WFR
    #__________________________
    #__________________________
    
    #browser()
    if (!file.exists(vals$QCpath)){ 
      # if no QC file exists in QC folder, associated with the filename in defaults.csv, an empty file must be created
      if (dfs$df.defaults.QC$QC.Kind == "VBW") {
        # Define structure of an empty VBW File
        QCtowrite <- data.frame("Labels" = as.character(),
                                "StartTime" = as.character(),
                                "Intensity" = as.character(),
                                "Concentration" = as.character(),
                                "Comment" = as.character(),
                                "TimeStamp" = as.character(),
                                "Operator" = as.character(),
                                "Outlier" = as.character(),
                                stringsAsFactors = F)
        
        
        
      } else if (dfs$df.defaults.QC$QC.Kind == "QC") {
        # Define structure of an empty QC File
        QCtowrite <- data.frame("Labels" = as.character(),
                                "StartTime" = as.character(),
                                "Concentration" = as.character(),
                                "WFR" = as.character(),
                                "Defined" = as.character(),
                                "Comment" = as.character(),
                                "TimeStamp" = as.character(),
                                "Operator" = as.character(),
                                "Outlier" = as.character(),
                                stringsAsFactors = F)
      }
      # Write the empty file
      write.csv(QCtowrite, file = vals$QCpath,row.names=FALSE,na = "")
    } 
    
    
    if (dfs$df.defaults.QC$QC.Kind == "VBW") {
      
     
      # Make the Plot
      plot.QC <- myBlindwertkartenPlot(plotData = read.QC(vals$QCpath),
                                       BWKrit = dfs$df.defaults.QC$LimitBG,
                                       title = paste(dfs$df.defaults.QC$QC.Name,"Regelkarte"),
                                       yLabel = paste("Konzentration",dfs$df.defaults.QC$Unit.Accept),
                                       xLabel = "Messzeitpunkt")
    }else if (dfs$df.defaults.QC$QC.Kind == "QC") {
      
      
      # Make the Plot
      plot.QC <- myZielwertkartenPlot(plotData = read.QC(vals$QCpath),
                                      Zielwert = dfs$df.defaults.QC$Zielwert,
                                      Unsicherheit = dfs$df.defaults.QC$Unsicherheit,
                                      title = paste(dfs$df.defaults.QC$QC.Name,"Regelkarte"),
                                      yLabel = paste("Wiederfindungsrate",dfs$df.defaults.QC$Unit.Accept),
                                      xLabel = "Messzeitpunkt")
    }
    
     
    dfs$QCAllData <- read.QC(vals$QCpath)
    dfs$QCPlot <- plot.QC
    dfs$QCData <- read.QC(vals$QCpath) %>% select(-Comment,-TimeStamp,-Operator)
    dfs$QCComment <- read.QC(vals$QCpath) %>% filter(!Comment %in% "")
  })
  
  
  # [View QC] Outputs----
  
  output$ViewQC_Plot <- renderPlot({
    req(input$sidebartab_ViewQC_Controls_selectInput_QC)
    dfs$QCPlot
  })
  
  
  output$ViewQC_Data <- renderDataTable({
    req(input$sidebartab_ViewQC_Controls_selectInput_QC)

    # Change the number of significant values
    df <- dfs$QCData
    if (!is.null(df$Concentration)) {
      df$Concentration <- as.numeric(df$Concentration)
      df$Concentration <- signif(df$Concentration,4)
      df$Concentration <- as.character(df$Concentration)
    }
    if (!is.null(df$Intensity)) {
      df$Intensity <- as.numeric(df$Intensity)
      df$Intensity <- signif(df$Intensity,4)
      df$Intensity <- as.character(df$Intensity)
    }
    if (!is.null(df$WFR)) {
      df$WFR <- as.numeric(df$WFR)
      df$WFR <- signif(df$WFR,4)
      df$WFR <- as.character(df$WFR)
    }
    
    # Sub-Function to translate JavaScript Code for shiny Input elements in character Vectors
    ## This Function is useful to Implement an Input-Element, e.g. checkboxes, in a DataTable by JavaScript Callback option 
    shinyInput <- function(FUN,id,num,all_values,...) {
      if(is.null(num)) return()
      inputs <- character(num)
      for (i in seq_len(num)) {
        inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,value = all_values[i],...))
      }
      inputs
    }
    # Generate Checkboxes in DataTable
    
    chkBoxes <- df$Outlier %>% as.numeric() %>% as.logical()
    
    df$Outlier <- shinyInput(checkboxInput,"srows_",nrow(df),all_values=chkBoxes,width=NULL)
    
    datatable(df,
              rownames = FALSE,
              selection='single',
              escape=F,
              options = list(orderClasses = TRUE,
                             pageLength = 10,
                             drawCallback= JS(
                               'function(settings) {
                                     Shiny.bindAll(this.api().table().node());}')))
    })
  
  # Reactive output to read out the checkbox inputs of DataTable
  CheckedRows <- reactive({
    rows=names(input)[grepl(pattern = "srows_",names(input))]
    paste(unlist(lapply(rows,function(i){
      if(input[[i]]==T){
        return(substr(i,gregexpr(pattern = "_",i)[[1]]+1,nchar(i)))
      }
    })))
  })
  
  # # Output to conroll the checkbox callback in DataTable ----
  # output$collection_txt <- renderText({
  #   req(dfs$QCAllData)
  #   rows <- CheckedRows()
  #   txt.Outlier <- character()
  #   
  #   # Generate a FALSE vector of length of DataTable
  #   Outlier <- logical(nrow(dfs$QCAllData))
  #   # Set checked rows as TRUE
  #   if (length(rows)!=0) {
  #     Outlier[as.numeric(CheckedRows())] <- TRUE
  #   }
  #   
  #   #browser()
  #   # Add a comment to the selected point from selected QC file
  #   DONE <- isolate(setOutliers(vals$QCpath,Outlier))
  #   txt.Outlier <- paste0(dfs$QCAllData[Outlier,"StartTime"],"\n")
  #   
  #   c("Outliers:\n",txt.Outlier)
  # })
  # 

  
  output$ViewQC_Comments <- renderText({
    req(dfs$QCComment)
    # Proof if no rows with comments are available
    if (nrow(dfs$QCComment) == 0) {
      Text <- ""
    }else{
      Text <- c("Kommentare:\n")
      Text <- c(Text,paste0(dfs$QCComment$Labels,
                            " gemessen am ",
                            dfs$StartTime,":  ",
                            dfs$QCComment$Comment,
                            "(",dfs$QCComment$TimeStamp," ",dfs$QCComment$Operator,")\n")
                )
    }
    
    Text
  })
  
  
  # Output for statistical evaluation, based on the selection of Outliers in the DataTable
  output$ViewQC_statistics <- renderText({
    req(dfs$QCAllData)
    
    df <- dfs$QCAllData
    rows <- CheckedRows()
    txt.Outlier <- character()
    
    # Generate a FALSE vector of length of DataTable
    Outlier <- logical(nrow(dfs$QCAllData))
    # Set checked rows as TRUE
    if (length(rows)!=0) {
      Outlier[as.numeric(CheckedRows())] <- TRUE
    }
    
    #browser()
    # Add a comment to the selected point from selected QC file
    DONE <- isolate(setOutliers(vals$QCpath,Outlier))
    txt.Outlier <- paste0(dfs$QCAllData[Outlier,"StartTime"],"\n")
    
    
    if (!is.null(df$Intensity)) {
      data <- as.numeric(df$Concentration)
      clean <- df %>% filter(Outlier %in% "0") %>% pull(Concentration) %>% as.numeric()
      Text <- c("Concentration:\n")
    } else if (!is.null(df$WFR)){
      data <- as.numeric(df$WFR)
      clean <- df %>% filter(Outlier %in% "0") %>% pull(WFR) %>% as.numeric()
      Text <- c("WFR:\n")
    }
    
    df.n <- length(data)
    df.mean <- mean(data)
      
    df.sd <- sd(data)
    df.rsd <- df.sd/df.mean
    
    clean.n <- length(clean)
    out.n <- length(data) - length(clean)
    clean.mean <- mean(clean)
    clean.sd <- sd(clean)
    clean.rsd <- clean.sd/clean.mean
    
    Text <- c(Text,paste0("n:",as.character(df.n),"\n",
                          " Mean:",as.character(signif(df.mean,3))," ",dfs$df.defaults.QC$Unit.Accept,"\n",
                          " SD:",as.character(signif(df.sd,3))," ",dfs$df.defaults.QC$Unit.Accept,"\n",
                          " RSD:",as.character(signif(df.rsd,3))," %\n",
                          "\nAusreißer frei:\n",
                          " n:",as.character(clean.n),"\n",
                          " Outliers:",as.character(out.n),"\n",
                          " Mean:",as.character(signif(clean.mean,3))," ",dfs$df.defaults.QC$Unit.Accept,"\n",
                          " SD:",as.character(signif(clean.sd,3))," ",dfs$df.defaults.QC$Unit.Accept,"\n",
                          " RSD:",as.character(signif(clean.rsd,3))," %\n"),
              "Outliers:\n",txt.Outlier)
    
    
  })
  #----------------------------------------------------------------------------------#
  #----------------------------------------------------------------------------------#
  
  # [View QC] Comment functions ----
  
  
  # [View QC] Observer triggert by Comment Button ----
  observeEvent(input$btn.comment.qc,{
    # Funtion to display the Modal with further input items
    showModal(CommentQCModal())
  })
  
  
  # [View QC] Comment Modal ----
  CommentQCModal <- function() {
    # Check if a row is selected
    if (is.null(input[["ViewQC_Data_rows_selected"]])) { # If no selection was made
      modalDialog(title = "Selection failed",
                  helpText("You have to select a QC point prior to select an action"),
                  footer = modalButton("Ok")
      )
    }else{ # If selection was made
      modalDialog(
        title = "Remove QC Point",
        p("Please enter your comment. Date and name will be entered automatically!"),
        footer = tagList(
          textInput(inputId = "txt.comment.qc",label = "Comment:",value = "",placeholder = "Enter comment here"),
          actionButton(inputId = "btn.cmt.qc", "Apply"),
          modalButton("Cancel")
        )
      )
    }
  }
  
  
  
  # Observer triggered by the Apply-Button in CommentQCModal()
  observeEvent(input$btn.cmt.qc, {
    # Extract selected QC Data from whole defaults file
    df.defaults.QC <- dfs$defaults %>% 
      filter(SAA.Menue %in% input$ViewQC_Controls_selectInput_SAA) %>% 
      filter(QC.Name %in% input$sidebartab_ViewQC_Controls_selectInput_QC)
    
    # Get the Filename
    QC.Filename <- df.defaults.QC$Filename
    
    # Generate full path
    QCpath <- normalizePath(file.path(path.QCtoWrite,QC.Filename))
    
    # get StartTime (unique identifier) of selected point
    StartTime <- dfs$QCData[input[["ViewQC_Data_rows_selected"]],"StartTime"]
    Label <- dfs$QCData[input[["ViewQC_Data_rows_selected"]],"Labels"]
    
    if (input$txt.comment.qc == "") {
      # If an empty string is set as input, an empty string has to be written.
      # This is a kind of "delete comment" function.
      str.comment <- ""
      TimeStamp <- ""
      Operator <- ""
    }else{
      # Define the comment and add Stamps like system time and Operators name
      str.comment <- input$txt.comment.qc
      TimeStamp <- format(Sys.time(),"%Y-%m-%d")
      Operator <- vals$Operator
    }
    
    # Add a comment to the selected point from selected QC file
    comment.QC(QCpath,StartTime,str.comment,TimeStamp,Operator)
    
    # Update the View QC outputs
    vals$perform.QC <- vals$perform.QC + 1
    
    # remove the modal
    removeModal()
  })
  
  
  
  #----------------------------------------------------------------------------------#
  #----------------------------------------------------------------------------------#
  
  # [View QC] Download functions ----
  
  # Download Buttons ----
  observe({
    # [Excel] ----
    output$dl.xlsx <- downloadHandler(
      filename = "QC-Report.xlsx",
      content = function(file){
        writeData <- list("QCData"= dfs$QCAllData)
        suppressWarnings(myXLSX.write(writeData,file,Override = F))
      })
    
    # [PDF] ----
    output$dl.pdf <- downloadHandler(
      filename = function() {"QC-Report.pdf"},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        
        # Select Report file from defaults.csv
        #______________________________________
        
        rmdFile <- file.path(tempdir(),"QC-Report.rmd")
        file.copy("QC-Report.rmd",rmdFile,overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list()
        params[["QCInfo"]] <- dfs$defaults %>% filter(SAA.Menue %in% input$ViewQC_Controls_selectInput_SAA) %>% filter(QC.Name %in% input$sidebartab_ViewQC_Controls_selectInput_QC)
        params[["Operator"]] <- vals$Operator
        params[["QCData"]] <- dfs$QCData
        params[["QCPlot"]] <- dfs$QCPlot
        params[["QCComment"]] <- dfs$QCComment
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        ### Function to generate compile a PDF with rmarkdown and LaTex
        showModal(modalDialog("Generating Report...Please wait!", footer=NULL))
        
        out <- rmarkdown::render(rmdFile, output_file = file,
                                 params = params,
                                 encoding="UTF-8",
                                 envir = new.env(parent = globalenv()))
        removeModal()
        
        file.rename(out, file)
        
        
      }
    )
  })
  
  #----------------------------------------------------------------------------------#
  #----------------------------------------------------------------------------------#
  
  # [View QC] Delete functions ----
  
  # [View QC] Observer triggert by Delete Button ----
  observeEvent(input$btn.delete.qc,{
    # Funtion to display the Modal with further input items
    showModal(RemoveQCModal())
  })
  
  
  # [View QC] Delete  Modal ----
  RemoveQCModal <- function() {
    if (is.null(input[["ViewQC_Data_rows_selected"]])) { # If no selection was made
      modalDialog(title = "Selection failed",
                  helpText("You have to select a QC point prior to select an action"),
                  footer = modalButton("Ok")
      )
    }else{ # If selection was made
      modalDialog(
        title = "Remove QC Point",
        p("Are you sure?"),
        footer = tagList(
          actionButton(inputId = "btn.rm.qc", "Apply"),
          modalButton("Cancel")
        )
      )
    }
  }
  
  
  
  # Observer triggered by "Apply" button in modal
  observeEvent(input$btn.rm.qc, {
    # Extract selected QC Data from whole defaults file
    df.defaults.QC <- dfs$defaults %>% 
      filter(SAA.Menue %in% input$ViewQC_Controls_selectInput_SAA) %>% 
      filter(QC.Name %in% input$sidebartab_ViewQC_Controls_selectInput_QC)
    
    # Get the Filename
    QC.Filename <- df.defaults.QC$Filename
    
    # Generate full path
    QCpath <- normalizePath(file.path(path.QCtoWrite,QC.Filename))
    
    # get StartTime (unique identifier) of selected point
    StartTime <- dfs$QCData[input[["ViewQC_Data_rows_selected"]],"StartTime"]
    
    #browser()
    # delete the selected point from selected QC file
    delete.QC(QCpath,StartTime)
    
    # Update the View QC outputs
    vals$perform.QC <- vals$perform.QC + 1
    
    # remove the modal
    removeModal()
  })
  
  #----------------------------------------------------------------------------------#
  #----------------------------------------------------------------------------------#
  
  
  
  #---------------------------------------------------------#
  #----------- Revalidation (Change defaults) --------------
  #---------------------------------------------------------#
  
  # Extract the Single Defaults Parameter for render as Textbox or Checkbox
  output$Single.Defaults <- renderUI({
    req(!is.null(vals$AdminRights))
    # Generate dynamic elements with values of defaults.csv 
    
    # loop for dynamically generation of input items
    lapply(1:length(get.defaults.Parameter(c("Single","Logical"))), function(i){
      # Check the Attribute of default parameter, defined by define.defaults.Columns() function
      if (get.defaults.Attribute(get.defaults.Parameter(c("Single","Logical"))[[i]]) == "Single") {
        textInput(inputId = paste0("txt.defaults.",get.defaults.Parameter(c("Single","Logical"))[[i]]),
                  label = get.defaults.Parameter(c("Single","Logical"))[[i]],
                  value = unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.Defaults.SAA.Menu) %>% select(get.defaults.Parameter(c("Single","Logical"))[[i]])))
      }else if (get.defaults.Attribute(get.defaults.Parameter(c("Single","Logical"))[[i]]) == "Logical"){
        checkboxInput(inputId = paste0("chk.defaults.",get.defaults.Parameter(c("Single","Logical"))[[i]]),
                      label = get.defaults.Parameter(c("Single","Logical"))[[i]],
                      value = as.logical(unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.Defaults.SAA.Menu) %>% select(get.defaults.Parameter(c("Single","Logical"))[[i]]))))
      }
    })
  })
  
  # Extract the Multiple Defaults Parameter for render as Table
  observeEvent(input$dropDown.Defaults.SAA.Menu,{
    # delete text information
    output$verb.Defaults.Info <- NULL
    # load and convert table for displaying Multiples
    #dfs$Multiple.defaults <-get.multiple.defaults(df = dfs$defaults,filter = input$dropDown.Defaults.SAA.Menu)
  })
  
  # load and convert table for displaying Multiples
  get.multiple.defaults <- function(df,input.filter){
    df.multiple <- df %>% 
      filter(SAA.Menue %in% input.filter) %>% 
      select(get.defaults.Parameter("Multiple"))
  }
  
  # Render the Table with Multiple Defaults Parameter
  output$tbl.Defaults <- renderRHandsontable({
    req(!is.null(vals$AdminRights))
    rhandsontable(get.multiple.defaults(df = dfs$defaults,
                                        input.filter = input$dropDown.Defaults.SAA.Menu),
                  rowHeaders = NULL,
                  useTypes = FALSE)
  })
  # Register and store changes in table with Multiple Defaults Parameter
  observeEvent(input$tbl.Defaults,{
    dfs$Multiple.defaults <- hot_to_r(input$tbl.Defaults)
  })
  
  #__________________
  # Function to store new Config
  
  Defaults.ChangeValues <- function(df,whichSAA){
    # get a copy of selected SAA.Menue of defaults.csv 
    df.new <- df
    for(i in get.defaults.Parameter(c("Single","Logical"))) {
      # Check the Attribute of default parameter, defined by define.defaults.Columns() function
      # for choosing the right dynamically generated InputID of the Element
      if (get.defaults.Attribute(i) == "Single") {
        # store the new value in the copy of defaults.csv
        df.new[df[["SAA.Menue"]] == whichSAA,i] <- input[[paste0("txt.defaults.",i)]]
      }else if (get.defaults.Attribute(i) == "Logical"){
        # store the new value in the copy of defaults.csv
        df.new[df[["SAA.Menue"]] == whichSAA,i] <- as.numeric(input[[paste0("chk.defaults.",i)]])
      }
    }
    df.new[df[["SAA.Menue"]] == whichSAA,get.defaults.Parameter("Multiple")] <- dfs$Multiple.defaults
    return(df.new)
  }
  
  observeEvent(input$btn.Defaults.Save,{
    # get a copy of selected SAA.Menue of defaults.csv 
    df.new <- Defaults.ChangeValues(df = dfs$defaults,whichSAA = input$dropDown.Defaults.SAA.Menu)
    write.defaults(df.new)
    write(x = paste(vals$Operator,date(),"Save",sep = ","),file = "defaults.log",append = TRUE)
    
    # Trigger for reload of input elements
    dfs$defaults <- read.defaults()
    output$verb.Defaults.Info <- renderText({"Successfully stored!"})
    vals$defaults.backup.files <- get.defaults.Backups()
  })
  
  # Action when Restore Button is clicked
  observeEvent(input$btn.Defaults.Load,{
    # Show Modals
    showModal(RestoreDefaultsModal())
  })
  
  # Action, wenn Modals Button is clicked
  observeEvent(input$btn.load.defaults,ignoreInit = TRUE, {
    req(input$dropdown.defaults.backups)
    dfs$defaults <- load.defaults(input$dropdown.defaults.backups)
    removeModal()
  })
  
  
  # Load Defaults Backup ----
  RestoreDefaultsModal <- function() {
    modalDialog(title = "Restore defaults",
                selectInput(inputId = "dropdown.defaults.backups",label =  "Backup-File:",choices = vals$defaults.backup.files),
                helpText("To store the loaded defaults as actual, the \"Save\" Button must be clicked. "),
                footer = tagList(
                  actionButton(inputId = "btn.load.defaults", "Apply"),
                  modalButton("Cancel")
                )
    )
  }
  #______________________________________________________________________________________________________________________________________
  #______________________________________________________________________________________________________________________________________
  
 
}


### Call ----
shinyApp(ui,server)

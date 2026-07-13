  



# Reactive value definitions

  vals <- reactiveValues(reload_adminDF = 0,





                         AdminRights = NULL,

                         FileToLoad = "",

                         loaded.Cali.File.Path = NULL,

                         perform.QC = 0,

                         NonDefaultAnalyt.vec = NULL,

                         ERROR = NULL,

                         Operator = NULL,

                         prependTabDone = FALSE,

                         performRenderSidebar = 0,

                         numSigDigits = 4,

                         btn.click.del.qc = 0,

                         btn.click.comment.qc = 0,

                         perform.del.qc = 0,

                         perform.comment.qc = 0,

                         defaults.backup.files =get.defaults.Backups())

  

  lists <- reactiveValues(myParams = NULL,

                          myList = NULL)

  

  

  

  # Definition einer Source als Datentabelle. Noch leer, aber die User-Datei wird hineingeschrieben

  dfs <- reactiveValues(defaults = read.defaults())



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

  obs1 <- observe(priority = 10000,x = {

    showModal(dataModal.init())

  })

  

  # Eine Funktion, die das Dialog-Fenster generiert. Der Parameter failed ist per Default auf FALSE

  # (Funktion kann ohne Eingabe dieses Parameters ausgerufen werden)

  # Wird die Funktion/das Dialog-Fenster mir failed = TRUE aufgerufen, wird eine Fehlermeldung in roter Schrift angezeigt.

  dataModal.init <- function(failed = FALSE) {

    # Das Dialog Fenster für die Eingabe und Prüfung der Credentials wird generiert

    modalDialog(

      title = "Authentification",

      # Zusätzliche Fehlermeldung wenn falsche Credentials eingegeben wurden

      if (failed)

        div(tags$b("Invalid Username or Password", style = "color: red;")),

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



# File selection and data loading



  # Initial Options for render of Data Tables ----

  #___________________________________________________________________________________________________________________

 



  # # For now a CSV file is read. In future a SQLite query will be defined


  # read.defaults <- function(){

  #   read.csv(file = paste0(path.root,"/","defaults.csv"),header = T,na.strings = "",stringsAsFactors = F)  

  # }




  # #

  

   

  # Globale Einstellungen für die Tabellen, die mit dem DT-Package erstellt werden



  

  # Initial parameters for FileSelection Dialog ----

  # Globale Einstellungen für die OpenFile-Dialogfenster

  # Create a new folder to link to a server folder


  roots = c(wd = ".")

  shinyFileChoose(input,'FileInput',roots = roots,filetypes = c('txt','csv'))



# Main data loading handler



  # Load Data when click FileInput-Button ----

  #___________________________________________________________________________________________________

  #___________________________________________________________________________________________________

  #___________________________________________________________________________________________________

  

  observeEvent(input$FileInput,{

    #browser()

    ### Function to generate RData from csv

    inFile <- parseFilePaths(roots,input$FileInput)$datapath

    

    # inFile <- input$FileInput$datapath

    if(length(inFile) == 0){return(0)}

    ## Process Uploaded File

    vals$FileToLoad <- normalizePath(inFile,mustWork = TRUE)

    

    # Block Inputs and show a message while waiting for processing

    showModal(modalDialog("Loading and evaluating...Please wait!", footer=NULL))

    

    

    if(file_ext(vals$FileToLoad) == "csv" || file_ext(vals$FileToLoad) == "txt" ){

      

      

      

      # [myParams] Generate the myParams list ----

      ## Create myParams for Input.Analyt

      

      # browser() # only for debugging

      lists$myParams <- list()

      

      #---- myParams Default Definition

      # Globals loaded by first Tab "Initial Inputs"

      lists$myParams$Operator <- vals$Operator

      lists$myParams$SAA.Selected <- input$dropDown.SAA.Menu

      lists$myParams$System <- input$dropDown.System

      lists$myParams$PipettenList <- df.ini.PipettenList

      lists$myParams$inFile <- vals$FileToLoad

      

      

      # Change defaults by input elements

      if(is.null(input$dropDown.BGMethod)){

        # Load Defaults for "User" usage

        lists$myParams$BGMethod <- unique(dfs$defaults %>% filter(System %in% input$dropDown.System) %>% select(BGMethod)) %>% pull()

      }else if(input$dropDown.BGMethod == "Kalibriergraden-Methode"){

        lists$myParams$BGMethod <- "DIN32645"

      }else if(input$dropDown.BGMethod == "Leerwertnethode"){

        lists$myParams$BGMethod <- "Leer"

      }else if(input$dropDown.BGMethod == "Nach Kaiser"){

        lists$myParams$BGMethod <- "Kaiser"

      }else if(input$dropDown.BGMethod == "Signal/Rausch (vor Okt 2020)"){

        lists$myParams$BGMethod <- "SN_2020_06"

      }else if(input$dropDown.BGMethod == "Signal/Rausch"){

        lists$myParams$BGMethod <- "SN"

      }

      

      if(is.null(input$num.SigDigit)){

        # Load Defaults for "User" usage

        lists$myParams$Significant.Digits <- unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(Significant.Digits))  %>% pull()

      }else{

        lists$myParams$Significant.Digits <- input$num.SigDigit

      }



      if(is.null(input$chk.BG.SAA)){

        # Load Defaults for "User" usage

        lists$myParams$BG.SAA <- as.logical(unique(dfs$defaults %>% # Reads the default value file

                                                     filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% # filter for selected QC Method

                                                     select(BG.SAA)))

      }else{

        lists$myParams$BG.SAA <- input$chk.BG.SAA

      }



      if(is.null(input$num.Alpha)){

        # Load Defaults for "User" usage

        lists$myParams$Alpha <- unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(Alpha)) %>% pull()

      }else{

        lists$myParams$Alpha <- input$num.Alpha

      }

      



      #   lists$myParams$Masse <- unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(Masse) %>% pull() %>% as.character())

      #   # if not as.character -> Error in dplyr contains() function!

      # }else{

      #   lists$myParams$Masse <- unlist(str_split(gsub(" ","",input$txt.Mass),",",Inf,simplify = FALSE))

      # }

      

      # Load Defaults for "User" usage

      lists$myParams$Signal.Name <- unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(Signal.Name) %>% pull() %>% as.character())

      lists$myParams$Masse <- unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(Masse) %>% pull() %>% as.character())

      # if not as.character -> Error in dplyr contains() function!

      

      if(is.null(input$chk.Means)){

        # Load Defaults for "User" usage

        lists$myParams$UseMeans <- unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(UseMeans)) %>% pull()

      }else{

        lists$myParams$UseMeans <- input$chk.Means

      }

      

      if(is.null(input$chk.Outliers)){

        # Load Defaults for "User" usage

        lists$myParams$outlier <- unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(outlier)) %>% pull()

      }else{

        lists$myParams$outlier <- input$chk.Outliers

      }

      

      if(is.null(input$chk.IntCorr)){

        # Load Defaults for "User" usage

        lists$myParams$UseIntCorr <- unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(UseIntCorr)) %>% pull()

      }else{

        lists$myParams$UseIntCorr <- input$chk.IntCorr

      }

      

      

      

      # Calibration definition

      # ONLY ONE CALIBRATION FILE CAN BE SUPPORTED FOR SAME INPUT FILE!!!!!

      lists$myParams[["Calibration"]] <- list() # empty list for storage of calibration input data

      

      # different possible handlings depending on exported raw-files. 

      if (as.logical(unique(dfs$defaults %>% filter(System %in% input$dropDown.System) %>% select(Cali.User.Selection)))) { # If user input is neccessary

        if(is.null(dfs$cali.files.selected)){ # If no selection performed by user in calibration tab (e.g. no Admin Rights)

          # Get the filename from default

          lists$myParams[["Calibration"]][["filename"]] <- unique(dfs$defaults %>% # Reads the default value file

                                                                    filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% # filter for selected QC Method

                                                                    select(Cali.Default.File)) %>% pull()

          # Get the full path

          lists$myParams[["Calibration"]][["fullpath"]] <- normalizePath(file.path(path.Calibration,lists$myParams[["Calibration"]][["filename"]]))

          

          # Read the file to store the values

          lists$myParams[["Calibration"]][["definition"]] <- read.csv(file = lists$myParams[["Calibration"]][["fullpath"]],

                                                                      header = T,

                                                                      check.names = FALSE,

                                                                      na.strings = "", # defines empty strings to set as NA

                                                                      stringsAsFactors = F)

          

        }else{ # If selection was made in Calibration Tab

          # Get the User-Input Values

          # If the edited calibration df was not saved, the loaded filename and path will be linked with the edited calibration values

          lists$myParams[["Calibration"]][["filename"]] <- input$select.Cali

          lists$myParams[["Calibration"]][["fullpath"]] <- vals$loaded.Cali.File.Path

          lists$myParams[["Calibration"]][["definition"]] <- dfs$cali.files.selected[-1,] %>% `names<-`(dfs$cali.files.selected[1,]) %>% type.convert(as.is = TRUE)

        }

      }else if (is.na(unique(dfs$defaults %>% filter(System %in% input$dropDown.System) %>% select(Cali.Default.File)))){ # If the calibration data is included in the Raw-File (e.g. eQuant)

        # Set all values NA

        lists$myParams[["Calibration"]][["filename"]] <- NA

        lists$myParams[["Calibration"]][["fullpath"]] <- NA

        lists$myParams[["Calibration"]][["definition"]] <- NA

      }else{ # If the calibration data is available in a special Calibration-File (e.g. DMA80evo)

        

        # Get the filename from default

        lists$myParams[["Calibration"]][["filename"]] <- unique(dfs$defaults %>% # Reads the default value file

                                                                  filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% # filter for selected QC Method

                                                                  select(Cali.Default.File)) %>% pull()

        # Get the full path

        lists$myParams[["Calibration"]][["fullpath"]] <- normalizePath(paste0(path.Calibration,"/",lists$myParams[["Calibration"]][["filename"]]),winslash = "\\")

        

        # Reading is not possible without a conversion

        lists$myParams[["Calibration"]][["definition"]] <- NA

      }

      

      


      # [myList] Calculation of myList----

      if (input$dropDown.System == "DMA80evo"){

        # browser() # only for debugging

        # myParams <- isolate(lists$myParams)

        # save(myParams,file = "myParams_DMA80evo.RData") # only for debugging

        lists$myList <- my.DMA80evo.myList(myParams = lists$myParams)

      }else if (input$dropDown.System == "iTeva"){

        # browser() # only for debugging

        # myParams <- isolate(lists$myParams)

        # save(myParams,file = "myParams_iTeva.RData") # only for debugging

        lists$myList <- my.iTeva.myList(myParams = lists$myParams)

      }else if (any(input$dropDown.System == c("eQuant (MS)","eQuant (OES)"))) {

        # browser() # only for debugging

        # myParams <- isolate(lists$myParams)

        # save(myParams,file = "myParams_eQuant_SM.RData")

        lists$myList <- my.eQuant.myList(myParams = lists$myParams)

      }else if (input$dropDown.System == "tQuant (MS)") {


        # myParams <- isolate(lists$myParams)

        # save(myParams,file="myParams_tQuant_Br.RData")

        # save(myParams,file="myParams_tQuant_Pt.RData")

        lists$myList <- my.tQuant.myList(myParams = lists$myParams)

      }

      

      ## Trigger the QC observer

      ## must be triggered by Button click

      # vals$perform.QC <- vals$perform.QC + 1

      

      #Trigger an Error-Modal Error handling during myList calculation

      if (!is.null(lists$myList$ERROR)) {


      }

      

      vals$FileToLoad <- paste0(str_split_fixed(string = basename(vals$FileToLoad),pattern = ".csv",n=2)[1],".RData")

      

      

    }

    

    vals$performRenderSidebar <- vals$performRenderSidebar + 1

    # Trigger variable for re-render the Solids Input-Table

    dfs$Solids.definition <- lists$myList$Solids

    
    removeModal()

  })



# Dynamic sidebar menu generation



  # Generate DYNAMIC MenuItems in the Sidebar in loop ----

  # Generate Menu Items ----

  dynamicResultItems <- eventReactive(vals$performRenderSidebar,{

    # Variables for SideBar creation

    req(!is.null(lists$myList))


    # Generate Sidebar Items to be rendered

    sidebarMenu(id = "tabs",

                #menuItem("Input Parameters",tabName = "inputparameter", icon = icon("archive")), 

                menuItem("Header",tabName = "header", icon = icon("archive")),

                menuItem("Intensities",tabName = "intensities", icon = icon("archive")),

                menuItem("Calibration",tabName = "calibration", icon = icon("archive"),

                         lapply(1:length(lists$myList$Calibration$Info$Analytes), function(i) { # List of Analytes ----

                           menuSubItem(lists$myList$Calibration$Info$Analytes[i], tabName = paste0("analyt",i), icon = icon("chart-bar"))})

                ),

                

                menuItem("Concentration",tabName = "concentration", icon = icon("archive"),

                         menuSubItem("Used Standards",tabName = "ConcUsedSTDs", icon = icon("archive")),

                         menuSubItem("Raw Concentration",tabName = "Concraw", icon = icon("archive")),

                         menuSubItem("Less LOQ",tabName = "ConclessBG", icon = icon("archive")),

                         menuSubItem("Dilution Factor",tabName = "ConcmultiplyVF", icon = icon("archive")),

                         menuSubItem("Final Concentration",tabName = "ConcFinal", icon = icon("archive"))

                ),

                # Tab should be generated if chomatographical data is available

                if(as.logical(unique(dfs$defaults %>% # Get the Chromatography tag in defaults.csv

                                     filter(System %in% input$dropDown.System) %>% 

                                     select(Chromatography)))) menuItem("Chromatogramme",tabName = "chromatogramme", icon = icon("chart-area"),

                                                                        lapply(1:length(lists$myList$Header$Labels), function(i) { # List of Chromatograms ----

                                                                          menuSubItem(lists$myList$Header$Labels[i], tabName = paste0("chromatogramm",i), icon = icon("chart-bar"))})

                                     ),

                if(!is.null(lists$myList[["QC"]])) menuItem("QC",tabName = "qc", icon = icon("archive"),

                                                            lapply(1:length(names(lists$myList[["QC"]])), function(i) { # List of QC Samples ----

                                                              menuSubItem({names(lists$myList[["QC"]])}[i], tabName = paste0("qc",i), icon = icon("chart-bar"))})

                ),

                if(!is.null(lists$myList[["Solids.Conc.nice"]])) menuItem("Solids",tabName = "SolidConc", icon = icon("archive"))

                

    )

    

    

  })

  

  

  

  #__________________________________________

  # Generate Tabs controlled by Menu-Items ----

  dynamic.SidebarTabItems <- eventReactive(vals$performRenderSidebar,{

    req(!is.null(lists$myList))


    dynamic_SidebarTabItems <- c(

      

      list(tabItem(tabName = "header",

                   h2("SampleList"),

                   rHandsontableOutput("tbl.Header")

      )),

      

      list(tabItem(tabName = "intensities",

                   h2("Intensities"),

                   DTOutput("tbl.Intensities")

      )),

      

      list(tabItem(tabName = "ConcUsedSTDs",

                   h2("Used Standards"),

                   DTOutput("tbl.Conc.UsedSTDs")

      )),

      list(tabItem(tabName = "Concraw",

                   h2("Raw Concentration"),

                   DTOutput("tbl.Conc.Raw")

      )),

      list(tabItem(tabName = "ConclessBG",

                   h2("Concentration less LOQ"),

                   DTOutput("tbl.Conc.lessBG")

      )),

      list(tabItem(tabName = "ConcmultiplyVF",

                   h2("Concentration incl. Dilution Factor"),

                   DTOutput("tbl.Conc.DF")

      )),

      list(tabItem(tabName = "ConcFinal",

                   h2("Final Concentration"),

                   DTOutput("tbl.Conc.Final")

      )),

      

      

      lapply(1:length(lists$myList$Calibration$Info$Analytes), function(i){

        tabItem(tabName = paste0("analyt",i),

                uiOutput(paste0("calibration.Method",i)),

                fluidRow(box(title = "Parameter",collapsible = T,width = 3,DTOutput(paste0("tbl.CaliOLS.Param",i))),

                         box(title = "Kalibration Input",collapsible = T,width = 6,DTOutput(paste0("tbl.CaliOLS.Input",i)))),

                fluidRow(box(title = "Segmentierte Kalibration",collapsible = T,width = 4,plotOutput(paste0("plot.CaliPlot",i))),

                         box(title = "Klassische Kalibration",collapsible = T,width = 4,plotOutput(paste0("plot.Plot_fit",i))),

                         box(title = "Residuen",collapsible = T,width = 4,plotOutput(paste0("plot.Plot_res",i))))

        )

      }),

      

      # Tab should be generated if chomatographical data is available

      if(as.logical(unique(dfs$defaults %>% # Get the Chromatography tag in defaults.csv

                           filter(System %in% input$dropDown.System) %>% 

                           select(Chromatography)))) lapply(1:length(lists$myList$Header$Labels), function(i){

                             tabItem(tabName = paste0("chromatogramm",i),

                                     box(title = "Chromatogramm",collapsible = T,width = 8,plotOutput(paste0("plot.chromatogramm",i))),

                                     box(title = "Data",collapsible = T,width = 4,DTOutput(paste0("tbl.chromatogramm",i)))

                             )

                           }),

      

      if(!is.null(lists$myList[["QC"]])) lapply(1:length(names(lists$myList[["QC"]])), function(i){


        tabItem(tabName = paste0("qc",i),

                box(title = "Plot",collapsible = T,width = 6,

                    plotOutput(paste0("plot.qc",i)),

                    verbatimTextOutput(paste0("verb.qc",i))),

                box(title = "Data",collapsible = T,width = 6,

                    actionButton(inputId = paste0("btn.comment.qc",i),label = "Comment the Point"),

                    # actionButton(inputId = paste0("btn.archiev.qc",i),label = "Archiev QC"),

                    # selectInput(inputId = paste0("dropdown.archiev.qc",i),label = "Select QC Archiev",choices = get.QC.Archiev(paste0(path.QCtoWrite,"/QC_Archiev/Index.csv"),lists$myList[["Input.Parameter"]][["QC"]][[i+1]])),

                    # actionButton(inputId = paste0("btn.report.qc",i),label = "Report QC"),

                    if(vals$AdminRights) actionButton(inputId = paste0("btn.delete.qc",i),label = "Delete Point"),

                    hr(),

                    DTOutput(paste0("tbl.qc",i)))

        )

      }),

      

      if(!is.null(lists$myList[["Solids.Conc.nice"]])) list(tabItem(tabName = "SolidConc",

                                                h2("Solid Concentration"),

                                                selectInput(inputId = "dropDown.Solid.Unit",label = "Unit",choices = c("ng/kg","µg/kg","mg/kg","g/kg","%"),selected = "mg/kg"),

                                                DTOutput("tbl.Solid.Conc")

      ))

      

    )

    do.call(tabItems, dynamic_SidebarTabItems)

  })



# Sidebar render trigger



  # Generate the Sidebar Menuitems and their contents

  observeEvent(vals$performRenderSidebar,{

    req(!is.null(lists$myList))


    # Function contains looped render-functions with loop-generated IDs

    render.Calibration()

    

    # Check wether the prepend was already performed

    if(!vals$prependTabDone){

      # if not

      prependTab(inputId = "Main.Tabs",

                 tabPanel(title = "Report",uiOutput("tab.Report")),

                 select = FALSE

      )

      

      prependTab(inputId = "Main.Tabs",

                 tabPanel(title = "Results",uiOutput("DynamicTabs")),

                 select = TRUE

      )

      # set variable for check to omit additional prepending Tabs

      vals$prependTabDone <- TRUE

    }

    

    # Only renders chromatographical tabs, tables and plots if data is available

    if (!is.null(lists$myList[["Chromatography"]])) {

      render.Chromatography()

    }

    

    # Only renders QC tabs, tables and plots if data is available

    if (!is.null(lists$myList[["QC"]])) {

      render.QC()

    }

    

    

  })

  

  # Generate the dynamic sidebar menue tabs (only Names and IDs)

  output$menuOut <- renderMenu({dynamicResultItems()})

  # Linking the empty tabs with render-functions

  output$DynamicTabs <- renderUI({dynamic.SidebarTabItems()})



  # Generate the rendered Outputs included in single Tabs ----

  ## Outputs

  ## Title in Dashboard Header

  output$HeaderTitle <- renderText({lists$myList$System$Aquisition.Mode})

  

  ## Title in Dashboard Body

  output$FileName <- renderUI({h2(lists$myList$Input.Parameter$FileName)})

  

  ## Link to QC-Report and Validation Application

  output$LinkToQC <- renderUI({

    req(!is.null(vals$AdminRights))

    actionLink(

      inputId = "github",

      label = "<-- ZE1 QC Reports",

      icon = icon("calculator"),

      href = "//10.1.5.100:3838/myShinyApps/QC_Viewer",

      onclick = "window.open('//10.1.5.100:3838/myShinyApps/QC_Viewer', '_blank')"

    )

  })

  

  # Pipetten Liste

  output$tbl.PipettenListe <- renderDT({

    # req(lists$myList)

    df.PipettenListe <- as.data.frame(lists$myList$Input.Parameter$PipettenList) %>% `names<-`("Pipetten-ID")

    df.PipettenListe

  },server = TRUE,editable = TRUE)

  

  

  # Header

  

  output$tbl.Header <- renderRHandsontable({


    # req(lists$myList)

    # in iTeva the ColName is "Messzeit". To enable treatment, the ColNames in all applications have to be equal

    if (!is.null(lists$myList$Header$Messzeit)) {

      names(lists$myList$Header)[names(lists$myList$Header) == "Messzeit"] <- "StartTime"

      # Weite Abweichungen durch andere Importe können hier eingefügt werden

    }

    rhandsontable(lists$myList$Header %>% select(-StartTime)) #%>% hot_col("factor_allow", allowInvalid = TRUE)

  })

  

  # REPORT

  # Raw

  output$tbl.Intensities <- renderDT({

    # req(lists$myList)

    myDFround(lists$myList$Report$Raw,0)

  },options=list(scrollX = TRUE,pageLength = 100))

  

  # Used Standards

  output$tbl.Conc.UsedSTDs <- renderDT({

    # req(lists$myList)

    lists$myList$Report$Used.Standards

  },options=list(scrollX = TRUE,pageLength = 100))

  

  # Concentration

  output$tbl.Conc.Raw <- renderDT({

    # req(lists$myList)

    myDFsignif(lists$myList$Report$Concentration,vals$numSigDigits)

  },options=list(scrollX = TRUE,pageLength = 100))

  

  # Concentration Less BG

  output$tbl.Conc.lessBG <- renderDT({

    # req(lists$myList)

    myDFsignif(lists$myList$Report$Conc.less.BG,vals$numSigDigits)

  },options=list(scrollX = TRUE,pageLength = 100))

  

  # Concentration Dilution Factor

  output$tbl.Conc.DF <- renderDT({

    # req(lists$myList)

    myDFsignif(lists$myList$Report$Conc.DF,vals$numSigDigits)

  },options=list(scrollX = TRUE,pageLength = 100))

  

  # Final Concentration

  output$tbl.Conc.Final <- renderDT({

    # req(lists$myList)

    lists$myList$Report$Conc.Final

  },options=list(scrollX = TRUE,pageLength = 100))



  # Download Buttons XLSX ----

  observe({

    # req(lists$myList)

    output$dl.xlsx <- downloadHandler(

      filename = function(){"Report.xlsx"},

      content = function(file){


        if(any(input$dropDown.System == c("eQuant (MS)","eQuant (OES)"))){

          writeData <- list(

            "Header"= lists$myList$Header,

            "Final.Conc" = lists$myList$Concentration$Conc.Final,

            "RawData"= lists$myList$Raw.Data$PreSorted$Raw.Intensity$Data,

            "Raw.Average" = lists$myList$Raw.Data$eQuant$Raw$Raw.Average$Data,

            "Std.Conc" = lists$myList$Calibration$Std.Concentration,

            "BG.Calc" =  myOLS_BG(lists$myList$Calibration$OLS,Parameter = "xBG"),

            "NG.calc" =  myOLS_BG(lists$myList$Calibration$OLS,Parameter = "xNG"),

            "Raw.Conc" = lists$myList$Concentration$Concentration,

            "BG.Conc" = lists$myList$Concentration$Conc.less.BG,

            "VF.Conc" = lists$myList$Concentration$Conc.multiply.VF)

          

          # Append the Internal Correction Data of Flag was set

          if(as.logical(lists$myList$Input.Parameter$UseIntCorr)){

            writeData <- c(writeData[1:4], list("IntStd.Average" = lists$myList$Raw.Data$eQuant$IntStd$Raw.Average,

                                                "IntStd.Factors" = lists$myList$Raw.Data$eQuant$IntStd$Average.Factor,

                                                "IntStd.Assignment" = lists$myList$Raw.Data$eQuant$IntStd$Assignment,

                                                "Corr.Average" = lists$myList$Raw.Data$eQuant$Corrected.Data$Corr.Average),

                           writeData[5:length(writeData)])

          }

          

          

          # Append the Final.Solids dataframe to the Export List on second position

          if(!is.null(lists$myList$Solids.Conc.nice)){

            writeData <- c(writeData[1], list("Final.Solids" = lists$myList$Solids.Conc.nice),writeData[2:length(writeData)])

            writeData <- c(writeData[1:2], list("Solids.Amounts" = dfs$Solids.definition),writeData[3:length(writeData)])

          } 

          

          if (!is.na(lists$myList$Raw.Data$Survey$Raw$Info)) {

            writeData$"Survey.Intensity" = lists$myList$Raw.Data$Survey$Raw$Info

          }

          if (!is.null(lists$myList$Raw.Data$Survey$Raw$Raw.Survey.Intensity)) {

            writeData$"Survey.Intensity" = lists$myList$Raw.Data$Survey$Raw$Raw.Survey.Intensity$Data

          }

          if (!is.null(lists$myList$Raw.Data$Survey$Raw$Raw.Survey.Average)) {

            writeData$"Survey.Average" = lists$myList$Raw.Data$Survey$Raw$Raw.Survey.Average$Data

          }

          if (!is.null(lists$myList$Raw.Data$Survey$Raw$Raw.Survey.STD)) {

            writeData$"Survey.STD" = lists$myList$Raw.Data$Survey$Raw$Raw.Survey.STD$Data

          }

          if (!is.null(lists$myList$Raw.Data$Survey$Raw$Raw.Survey.RSD)) {

            writeData$"Survey.RSD" = lists$myList$Raw.Data$Survey$Raw$Raw.Survey.RSD$Data

          }

          

          # Append QC Data if QC is available

          if(!is.null(lists$myList$QC)){

            for (i in 1:length(lists$myList[["QC"]])) {

              #qc <- names(lists$myList[["QC"]][i])

              writeData[[names(lists$myList[["QC"]][i])]] <- list(lists$myList[["QC"]][[i]][["QC.Table"]])
  }
          

        } else if(input$dropDown.System == "tQuant (MS)"){

            writeData <- list(

              "Header"= lists$myList$Header,

              "Final.Conc" = lists$myList$Concentration$Conc.Final,

              "RawData"= "Zu viel für Excel",

              "PeakStart" =lists$myList$Chromatography$PeakStart$Data,

              "PeakEnd" = lists$myList$Chromatography$PeakEnd$Data,

              "Retention" = lists$myList$Chromatography$Retention$Data,

              "PeakHeight" = lists$myList$Chromatography$PeakHeight$Data,

              "PeakArea" = lists$myList$Chromatography$PeakArea$Data,

              "BaselineHeight" = lists$myList$Chromatography$BaselineHeight$Data,

              "Chromatogram" = lists$myList$Chromatography$Chromatogramm$SortedChromatogramm,

              "Std.Concentration" = lists$myList$Calibration$Std.Concentration,

              "NG.calc" =  myOLS_BG(lists$myList$Calibration$OLS,Parameter = "xNG"),

              "BG.calc" =  myOLS_BG(lists$myList$Calibration$OLS,Parameter = "xBG"),

              "Raw.Conc" = lists$myList$Concentration$Concentration,

              "BG.Conc" = lists$myList$Concentration$Conc.less.BG,

              "VF.Conc" = lists$myList$Concentration$Conc.multiply.VF)

            

            # Append QC Data if QC is available

            if(!is.null(lists$myList$QC)){

              for (i in 1:length(lists$myList[["QC"]])) {

                #qc <- names(lists$myList[["QC"]][i])

                writeData[[names(lists$myList[["QC"]][i])]] <- list(lists$myList[["QC"]][[i]][["QC.Table"]])

              }

            }

          } else if(input$dropDown.System == "iTeva"){

            

            writeData <- list(

              "Header"= lists$myList$Header,

              "Final.Conc" = lists$myList$Concentration$Conc.Final,

              "RawData"= lists$myList$Measured$Values,

              "Raw.Average" = lists$myList$Measured$Means,

              "IntStd.Average" = lists$myList$IntStd$Means,

              "IntStd.Factors" = lists$myList$IntStd$Int.Corr.Factors,

              "IntStd.Assignment" = lists$myList$Measured$ISRef,

              "Corr.Average" = lists$myList$Measured$Corrected.Means,

              "Std.Conc" = lists$myList$Calibration$Definition,

              "NG.calc" =  myOLS_BG(lists$myList$Calibration$OLS,Parameter = "xNG"),

              "BG.calc" =  myOLS_BG(lists$myList$Calibration$OLS,Parameter = "xBG"),

              "Raw.Conc" = lists$myList$Concentration$Concentration,

              "BG.Conc" = lists$myList$Concentration$Conc.less.BG,

              "VF.Conc" = lists$myList$Concentration$Conc.multiply.VF)

            

            # Append the Final.Solids dataframe to the Export List on second position

            if(!is.null(dfs$Solids.definition)){

              writeData <- c(writeData[1], list("Final.Solids" = lists$myList$Solids.Conc.nice),writeData[2:length(writeData)])

              writeData <- c(writeData[1:2], list("Solids.Amounts" = dfs$Solids.definition),writeData[3:length(writeData)])

            } 

            

            

          } else if(input$dropDown.System == "DMA80evo"){

            

            df <- lists$myList$Input.Parameter

            df$PipettenList <- NULL

            writeData <- list("InputParameter"= df,

                              "RawData" = lists$myList$RawData$Raw,

                              "RawData_Clean" = lists$myList$RawData$RawClean,

                              "Calbration" = lists$myList$Calibration$Daten,

                              "NG.calc" =  myOLS_BG(lists$myList$Calibration$OLS,Parameter = "xNG"),

                              "BG.calc" =  myOLS_BG(lists$myList$Calibration$OLS,Parameter = "xBG"),

                              "Samples.Data" = lists$myList$Samples$Samples.Data,

                              "Samples.Average" = lists$myList$Samples$Samples.Average,

                              "Report" = lists$myList$Samples$Report)

            

          }

        

        suppressWarnings(myXLSX.write(writeData,file,Override = F))

      })

    # Download Buttons RData ----

    output$dl.RData <- downloadHandler(

      filename = function(){"RData.RData"},

      content = function(file){

        save(list = c("myList"),file = file)

      })

    # Download Buttons REPORT.PDF ----

    output$dl.pdf <- downloadHandler(

      filename = function() {"report.pdf"},

      content = function(file) {

        # Copy the report file to a temporary directory before processing it, in

        # case we don't have write permissions to the current working dir (which

        # can happen when deployed).

        

        

        ##++++++++++++++++++++++++++++++++++++

        ##+++++++    DEBUGGING     +++++++++++

        ##++++++++++++++++++++++++++++++++++++

         #browser()

        # myList <- lists$myList

        # save(myList,file = "myList.RData")

        ##++++++++++++++++++++++++++++++++++++

        

        

        # Select Report file from defaults.csv

        #______________________________________

        

        rmdFile <- file.path(tempdir(),unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(Report.File)) %>% pull())

        file.copy(unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(Report.File)) %>% pull(),rmdFile,overwrite = TRUE)

        

        # Set up parameters to pass to Rmd document

        params <- list()

        params[["myList"]] <- lists$myList

        # Knit the document, passing in the `params` list, and eval it in a

        # child of the global environment (this isolates the code in the document

        # from the code in this app).

        ### Function to generate compile a PDF with rmarkdown and LaTex

        showModal(modalDialog("Generating Report...Please wait!", footer=NULL))

        # DEBUG PDF Report Generation ----

        

        out <- rmarkdown::render(rmdFile, output_file = file,

                                 params = params,

                                 encoding="UTF-8",

                                 envir = new.env(parent = globalenv()))

        removeModal()

        

        file.rename(out, file)

        

        

      }

    )

  })



    ## Dynamic Outputs in loops ----

  # the lapply functions performes a loop with a list of items in output.

  # In this case the output is a list of shiny-HTML outputs

  # This function can be called within a reactive expression to generate dynamic rendered objects

  

  render.Calibration <- function(){

    if(is.null(lists$myList)){

      return(NULL)

    }

    if(!is.null(lists$myList$Calibration)){

      

      # Generate Output with Calibration Data

      lapply(1:length(lists$myList$Calibration$Info$Analytes), function(i){

        CalMeth <- lists$myList$Calibration$OLS[[lists$myList$Calibration$Info$Analytes[i]]][["Methode"]]

        

        # Print Calibration method as section title

        output[[paste0("calibration.Method",i)]] <- renderUI(h3(CalMeth))

        

        # Make a table with Calibtration parameters

        output[[paste0("tbl.CaliOLS.Param",i)]] <- renderDT({

          # Different outputs depending on the calibration method

          if(CalMeth == "Kalibriergraden-Methode"){

            Parameter <- c("b0","b1","sb0","sb1","R","k","m","n","Alpha","ta","ta/2","yk","xNG","xEG","xBG")

          } else if(CalMeth == "Schätzung nach Kaiser (3s)"){

            Parameter <- c("b0","b1","sb0","sb1","R","n","sl","yk","xNG","xEG","xBG")

          } else if(CalMeth == "Leerwertmethode"){

            Parameter <- c("b0","b1","sb0","sb1","R","k","m","n","Alpha","ta","ta/2","sl","yk","xNG","xEG","xBG")

          } else if(CalMeth == "Signal/Rausch - Methode"){

            Parameter <- c("b0","b1","sb0","sb1","R","m","n","Alpha","Peak.Height","Baseline.Height","SN","xNG","xEG","xBG")

          } else {

            Parameter <- c("b0","b1","sb0","sb1","R","xNG","xEG","xBG")

          }

          df <- data.frame("Parameter" = sapply(Parameter,function(j){lists$myList$Calibration$OLS[[lists$myList$Calibration$Info$Analytes[i]]][[j]]}),

                           row.names = Parameter)

          # make values nice

          myDFsignif(df,vals$numSigDigits)

          

        },options = list(pageLength = 25))

        

        # Render table with Calibration Input Values

        output[[paste0("tbl.CaliOLS.Input",i)]] <- renderDT({

          myDFsignif(data.frame("Concentration" = lists$myList$Calibration$OLS[[lists$myList$Calibration$Info$Analytes[i]]][["x"]],

                                "Intensity" = lists$myList$Calibration$OLS[[lists$myList$Calibration$Info$Analytes[i]]][["y"]],

                                "Residuen" = lists$myList$Calibration$OLS[[lists$myList$Calibration$Info$Analytes[i]]][["Residuen"]],

                                "Outliers" = lists$myList$Calibration$OLS[[lists$myList$Calibration$Info$Analytes[i]]][["Outliers"]][["Outlier.Check"]][["out"]]),vals$numSigDigits)

        })

        

        # Render different Calibration plots

        output[[paste0("plot.CaliPlot",i)]] <- renderPlot({lists$myList$Calibration$Cali.Plots[[lists$myList$Calibration$Info$Analytes[i]]]})

        output[[paste0("plot.Plot_fit",i)]] <- renderPlot({lists$myList$Calibration$OLS[[lists$myList$Calibration$Info$Analytes[i]]][["Plot_fit"]]})

        output[[paste0("plot.Plot_res",i)]] <- renderPlot({lists$myList$Calibration$OLS[[lists$myList$Calibration$Info$Analytes[i]]][["Plot_res"]]})

      })

    }

  } 

  

  render.Chromatography <- function(){

    if(is.null(lists$myList)){

      return(NULL)

    }

      # Generate Output with Chromatography Data

    if(!is.null(lists$myList$Chromatography)){

      lapply(1:length(lists$myList$Header$Labels), function(i){

        output[[paste0("tbl.chromatogramm",i)]] <- renderDT({

          df <- rbind(lists$myList$Chromatography$PeakStart$Data[i,] %>% select(-Index,-Labels),

                      lists$myList$Chromatography$PeakEnd$Data[i,] %>% select(-Index,-Labels),

                      lists$myList$Chromatography$Retention$Data[i,] %>% select(-Index,-Labels),

                      lists$myList$Chromatography$PeakHeight$Data[i,] %>% select(-Index,-Labels),

                      lists$myList$Chromatography$PeakArea$Data[i,] %>% select(-Index,-Labels),

                      stringsAsFactors = F) %>% `row.names<-`(c("Start","End","Retention","Height","Area"))

          df <- myDFsignif(df,vals$numSigDigits)

        })

        output[[paste0("plot.chromatogramm",i)]] <- renderPlot({lists$myList$Chromatography$Chromatogramm.Plots[i]})

      })

    }

  }

  

  render.QC <- function(){

    if(is.null(lists$myList)){

      return(NULL)

    }

    # Generate Output with QC Data

    if(any(!is.na(lists$myList$QC))){

      # generate a list in a loop

      lapply(1:length(dfs$defaults %>% filter(SAA.Menue %in% lists$myList$Input.Parameter$SAA.Selected) %>% select(QC.Name) %>% pull()), function(i){

        # Render modified table

        output[[paste0("tbl.qc",i)]] <- renderDT({

          # Read QC Table

          df <- lists$myList$QC[[{dfs$defaults %>% 

              filter(SAA.Menue %in% lists$myList$Input.Parameter$SAA.Selected) %>% 

              select(QC.Name) %>% pull()}[i]]][["QC.Table"]]

          # Convert in numeric

          if(!is.null(df$Intensity)){df$Intensity %<>% as.numeric()}

          if(!is.null(df$Concentration)){df$Concentration %<>% as.numeric()}

          if(!is.null(df$WFR)){df$WFR %<>% as.numeric()}

          if(!is.null(df$Defined)){df$Defined %<>% as.numeric()}

          

          # Exclude Comment column

          if(!is.null(df$Comment)){df <- df %>% select(-Comment,-TimeStamp,-Operator,-Outlier)}

          

          # Round to 3 significant digits

          df <- myDFsignif(df,3)

          

          datatable(df,options=list(pageLength = 10),selection="single")

        },rownames = FALSE)

        

        # Render Plots

        output[[paste0("plot.qc",i)]] <- renderPlot({

          lists$myList$QC[[{dfs$defaults %>% 

              filter(SAA.Menue %in% lists$myList$Input.Parameter$SAA.Selected) %>% 

              select(QC.Name) %>% pull()}[i]]][["QC.Plot"]]

        })


        # Render Comments as verbatim text output

        output[[paste0("verb.qc",i)]] <- renderText({

          # Debug for older QC files

          #browser()

          req(lists$myList$QC[[{dfs$defaults %>% 

              filter(SAA.Menue %in% lists$myList$Input.Parameter$SAA.Selected) %>% 

              select(QC.Name) %>% pull()}[i]]][["QC.Table"]]["Comment"])

          # Filter rows with comments

          verb <- lists$myList$QC[[{dfs$defaults %>% 

              filter(SAA.Menue %in% lists$myList$Input.Parameter$SAA.Selected) %>% 

              select(QC.Name) %>% pull()}[i]]][["QC.Table"]] %>% filter(!Comment %in% "")

          # Proof if no rows with comments are available

          if (nrow(verb) == 0) {

            Text <- ""

          }else{

            Text <- c("Kommentare:\n")

            Text <- c(Text,paste0(verb$Labels," gemessen am ",verb$StartTime,":  ",verb$Comment,"(",verb$TimeStamp," ",verb$Operator,")\n"))

          }

          

          Text

        })

        

        

      })
  }



  # Edit Data in editable DataTables ----

  # "ini.tbl.PipettenList"

  output$ini.tbl.PipettenList <- renderDT({

    df.ini.PipettenList

  },server = TRUE,editable = TRUE)

  proxy.ini.PipettenList = dataTableProxy('ini.tbl.PipettenList')

  observeEvent(input$ini.tbl.PipettenList_cell_edit, {

    info = input$ini.tbl.PipettenList_cell_edit

    i = info$row

    j = info$col

    v = info$value

    # Die dargestellte Tabelle ist ein Dataframe und basiert auf den Parametern in myList. 

    # Daher muss die dargestellte Tabelle angepasst werden und die jeweiligen Parameter in myList

    df.ini.PipettenList[i,j] <<-DT::coerceValue(v, df.ini.PipettenList[i,j])

    replaceData(proxy.ini.PipettenList, df.ini.PipettenList,resetPaging = FALSE)  # important

    df.ini.PipettenList[i,j] <<- df.ini.PipettenList[i,j]

    # Übertragung der neuen Daten in myList$Input.Parameter. Dies ist wichtig für die Neuberechnung mittels Recalc Button

  })



  #__________________________________________

  ## [UI Tabs] Output depending on LogIn-User and existance of myList----

  output$initTabs <- renderUI({#dynamic.BodyTabItems

    req(!is.null(vals$AdminRights)) 

    if(vals$AdminRights){ # If an admin logged in

      tabsetPanel(id = "Main.Tabs",

        tabPanel(title = "Input",uiOutput("tab.InitialInputs")),

        tabPanel(title = "Calibration",uiOutput("tab.Calibration")),

        tabPanel(title = "Administration",uiOutput("tab.Administration")),

        tabPanel(title = "Config Defaults",uiOutput("tab.defaults"))

      )

    } else {

      tabsetPanel(id = "Main.Tabs",

        tabPanel(title = "Input",uiOutput("tab.InitialInputs"))

      )

    }

  })

  

  

  

  # [Input Tab] ----

  output$tab.InitialInputs <- renderUI({

    req(!is.null(vals$AdminRights)) 

    box(title = h2("Initial Inputs"),width = 12,

        fluidRow(

          box(title = "User Inputs",width = 3,

              verbatimTextOutput("Initial.Operator"),

              selectInput(inputId = "dropDown.System",label = "Measurement-Method",choices = unique(dfs$defaults[["System"]])),

              uiOutput("SAA.Selection"),

              if (vals$AdminRights) uiOutput("SAA.Admin.SigDigit"),

              if (vals$AdminRights) uiOutput("SAA.Admin.BG.SAA"),

              hr(),

              # Static File-Input Object to load Data

              shinyFilesButton(id = 'FileInput',label = 'Upload',title = 'Please select a file',multiple =  FALSE,icon = icon("file-upload"))

          ),

          box(title = "Used pipets",width = 3,

              DTOutput(outputId = "ini.tbl.PipettenList")

          )

        )

    )

  })

  

  # [Calibration Tab] ----

  output$tab.Calibration <- renderUI({

    if (vals$AdminRights) {

      uiOutput("box.CaliDef")

    }else{

      div(tags$b("You have no permission to access this tab. Please contact your administrator!", style = "color: red;"))

    }

  })

  

  

  # [Report Tab] ----

  output$tab.Report <- renderUI({

    # if (vals$AdminRights) {

    fluidPage(

      fluidRow(

        box(title = "Report",width = 3,

            # Button for download of PDF and XLSX Reports

            downloadButton("dl.xlsx",label = "xlsx"),

            downloadButton("dl.pdf",label = "PDF-Report")

        ),

        # Only render box if Solid is TRUE in defaults.csv

        if(!is.null(lists$myList) & as.logical(unique(dfs$defaults %>% # Reads the default value file

                                                      filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% # filter for selected QC Method

                                                      select(Solids)))) {

          box(title = "Amount weight",width = 4,

              actionButton(inputId = "btn.calc.solids",label = "Calculate solid concentrations"),

              hr(),

              rHandsontableOutput("tbl.Solids")

          )

        },

        # Selection of signal and element assignment

        if(!is.null(lists$myList) & as.logical(unique(dfs$defaults %>% # Reads the default value file

                                                      filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% # filter for selected QC Method

                                                      select(Analyt.User.Selection)))) {

          box(title = "Signal assignment",width = 3,

              #actionButton(inputId = "btn.perform.QC",label = "Perform QC"),

              uiOutput("Trace.Assign")#,

              #actionButton(inputId = "btn.perform.QC",label = "Perform QC")

          )

        },

        box(title = "QC-Definition",width = 2,

            fluidRow(

              column(width = 6,checkboxInput(inputId = "chk.QC.perform",label = "Perform QC",value = if(!is.null(input$chk.QC.perform)){input$chk.QC.perform}else{FALSE})),

              column(width = 6,uiOutput("QC.btn.Perform.QC"))),

            fluidRow(

              column(width = 12,uiOutput("QC.Definition")))

        )

      )

    )

    # }else{

    #   div(tags$b("You have no permission to access this tab. Please contact your administrator!", style = "color: red;"))

    # }

  })



  

  #__________________________________________

  # [UI Outputs] for Initial Selection----

  

  # Name of Operator initially Blank, but filled by LogIn ----

  output$Initial.Operator <- renderText({vals$Operator })

  # DropDown Menu for initial LoG Evaluation ----

  output$BG.Selection <- renderUI({

    req(input$dropDown.System)

    # Tab should be generated if chomatographical data is available

    if(as.logical(unique(dfs$defaults %>% # Get the Chromatography tag in defaults.csv

                         filter(System %in% input$dropDown.System) %>% 

                         select(Chromatography)))){

      selectInput(inputId = "dropDown.BGMethod",label = "LoQ Method",choices = c("Signal/Rausch","Signal/Rausch (vor Okt 2020)" ,"Kalibriergraden-Methode"))

    }else{

      selectInput(inputId = "dropDown.BGMethod",label = "LoQ Method",choices =c("Nach Kaiser","Leerwertnethode","Kalibriergraden-Methode"))

    }

  })

  

  

  # DropDown Menu for initial Input of QC-Information ----

  output$SAA.Selection <- renderUI({

    req(input$dropDown.System)

    selectInput(inputId = "dropDown.SAA.Menu",

                label = "Calibration-Method",

                choices = unique(dfs$defaults %>% filter(System %in% input$dropDown.System) %>% select(SAA.Menue)) %>% pull())

  })

  output$SAA.Admin.SigDigit <- renderUI({

    req(input$dropDown.SAA.Menu)

    numericInput(inputId = "num.SigDigit",

                 label = "Significant.Digits",

                 value = unique(dfs$defaults %>% # Reads the default value file

                                  filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% # filter for selected QC Method

                                  select(Significant.Digits)))

  })

  output$SAA.Admin.BG.SAA <- renderUI({

    req(input$dropDown.SAA.Menu)

    checkboxInput(inputId = "chk.BG.SAA",

                  label = "Report BG SAA",

                  value = as.logical(unique(dfs$defaults %>% # Reads the default value file

                                              filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% # filter for selected QC Method

                                              select(BG.SAA))))

  })

  

  # QC-Information and Button for performing QC tracking ----

  output$QC.Definition <- renderUI({

    req(input$dropDown.SAA.Menu)

    req(input$chk.QC.perform == TRUE)

    #browser()

    QCSoll <- dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% filter(QC.Kind %in% "QC") %>% select(QC.Name,SAA,QC.Norm,Unit.Norm)

    if (nrow(QCSoll)>0){

      lapply(1:nrow(QCSoll), function(i) {

        textInput(inputId = paste0("QCSoll.",QCSoll$SAA[i],".",QCSoll$QC.Name[i]), 

                  label = paste0(QCSoll$QC.Name[i]," (",QCSoll$Unit.Norm[i],")"),

                  value = QCSoll$QC.Norm[i])}

      ) 

    }

  })

  

  output$QC.btn.Perform.QC <- renderUI({

    if (input$chk.QC.perform) {

      actionButton(inputId = "btn.perform.QC",label = "QC2Chart",icon = icon("thumbs-up"))

    }

  })

  

  observeEvent(input$dropDown.System,{

    req(input$dropDown.System)

    # Checkbox for the use of Internal Standard Correction ----

    output$UI.IntCorr <- renderUI({

      if (!any(is.na(dfs$defaults %>% filter(System %in% input$dropDown.System) %>% select(UseIntCorr)))) {

        checkboxInput(inputId = "chk.IntCorr",label = "Internal Correction",

                      value = all(dfs$defaults %>% filter(System %in% input$dropDown.System) %>% select(UseIntCorr)))

      }

    })

    

    # Checkbox for use of averaged values for computation ----

    output$UI.Means <- renderUI({

      if (!any(is.na(dfs$defaults %>% filter(System %in% input$dropDown.System) %>% select(UseMeans)))) {

        checkboxInput(inputId = "chk.Means",label = "Use means",

                      value = all(dfs$defaults %>% filter(System %in% input$dropDown.System) %>% select(UseMeans)))

      }

    })

    

    # Checkbox for use of outlier test for computation ----

    output$UI.Outliers <- renderUI({

      if (!any(is.na(dfs$defaults %>% filter(System %in% input$dropDown.System) %>% select(outlier)))) {

        checkboxInput(inputId = "chk.Outliers",label = "Exclude outliers",

                      value = all(dfs$defaults %>% filter(System %in% input$dropDown.System) %>% select(outlier)))

      }

    })

  })

  

  

  

  # Select traces (mmass or wavelength) for QC and Report ----

  output$Trace.Assign <- renderUI({

    req(!is.null(lists$myList))

    if(as.logical(unique(dfs$defaults %>% filter(SAA.Menue %in% lists$myList$Input.Parameter$SAA.Selected) %>% select(Analyt.User.Selection)))){ 

      Assign <- data.frame("Element" = lists$myList$Calibration$Info$Elements,

                           "Analyte" = lists$myList$Calibration$Info$Analytes,

                           stringsAsFactors = F)

      

      

      # Element <- dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(Element,Analyt)

      lapply(1:length(unique(Assign$Element)), function(i) {

        

        choices <- Assign %>% filter(Element %in% unique(Assign$Element)[i]) %>% select(Analyte) %>% pull() # get the i-Element corresponding signal traces

        selected <- unique(dfs$defaults %>% filter(SAA.Menue %in% lists$myList$Input.Parameter$SAA.Selected) %>% select(Element,Analyt))[["Analyt"]] # get defaults.csv defined traces

        

        selectInput(inputId = paste0("Element.",unique(Assign$Element)[i]),

                    label = unique(Assign$Element)[i],

                    choices = choices, 

                    selected = intersect(choices,selected),

                    multiple=TRUE, 

                    selectize=FALSE)}

      )

      

    }

    

  })

  

  # TextInput for as information for pdf Report ----
  # })

  



  # Observer for changes in "Amoun weight" table

    # This observer will override the dataframe, that is rendered in below output$tbl.Solids. 

    # The change of this dataframe cause an automated trigger and re-render of output$tbl.Solids

  observeEvent(input$tbl.Solids,{

    #browser()

    dfs$Solids.definition <- hot_to_r(input$tbl.Solids)

  })

  

  output$tbl.Solids <- renderRHandsontable({

    req(lists$myList)


    # If the header of exported file doesn't have any information about weight amounts, an empty dataframe will be generated

    # The render output

    rhandsontable(dfs$Solids.definition,

                  #colHeaders = NULL,

                  rowHeaders = NULL,

                  useTypes = FALSE) %>%

      hot_col(col = "Unit.Amount", type = "dropdown", source = c("kg","g","mg","µg","ng")) %>%

      hot_col(col = "Unit.Volume", type = "dropdown", source = c("L","mL","µL"))

  })

  

  

  observeEvent(input$btn.calc.solids,{

    showModal(modalDialog("Calculate solid concentrations...Please wait!", footer=NULL))




    

    if (is.null(input$dropDown.Solid.Unit)) {

      Solid.Unit <- "mg/kg"

    } else {

      Solid.Unit <- input$dropDown.Solid.Unit

      

    }

    

    # Store the new Solids table in myList container

    lists$myList$Solids <- dfs$Solids.definition

    

    # Calculate the solid concentration. Sample rows without a weight input in tbl.solids results in NA output

    df.solids.Conc <- calc.Concentration.Solids(df.concVF = lists$myList$Concentration$Conc.multiply.VF, 

                                                str.concVF.Unit = unique(lists$myList$Calibration$Info$Units),

                                                df.solids = dfs$Solids.definition)

    

    # Get only vailed Samples, i.e. exclude rows with NAs, because this samles are not solid

    df.solids.Conc.vailed <- df.solids.Conc[complete.cases(df.solids.Conc),]

    

    # Calculate the factor of selected unit

    df.solids.Conc.vailed.Unit <- df.solids.Conc.vailed

    df.solids.Conc.vailed.Unit[names(df.solids.Conc)[c(-1,-2)]] <- df.solids.Conc.vailed[names(df.solids.Conc)[c(-1,-2)]]/mySIUnits(Solid.Unit)$Factor

    # Format the values. Replace zeros by "< BG" and round to significant values.

    

    # # [BUG] if the User has no admin permission, the input$num.SigDigit is not defined initially and must be loaded from defaults.csv

    if (is.null(input$num.SigDigit)) {

       SigDigit <- unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(Significant.Digits))  %>% pull()

    } else {

       SigDigit <- input$num.SigDigit

    }


    df.solids.Conc.nice <- myConcFinal(conc = df.solids.Conc.vailed.Unit,

                                       Analytes = lists$myList$Calibration$Info$Analytes,

                                       SignifDigits = as.numeric(SigDigit),

                                       BGs = NULL)

    

    attributes(df.solids.Conc.nice)[["Unit"]] <- Solid.Unit

    

    lists$myList[["Solids.Conc.vailed"]] <- df.solids.Conc.vailed

    lists$myList[["Solids.Conc.nice"]] <- df.solids.Conc.nice

    #browser()

    # Trigger variable to re-render the side-bar in order to add Solids-Tab

    vals$performRenderSidebar <- vals$performRenderSidebar + 1

    removeModal()

  })

  

  output$tbl.Solid.Conc <- renderDT({

    lists$myList$Solids.Conc.nice

  },options=list(scrollX = TRUE,pageLength = 100))

  

  

  

  

  observeEvent(input$dropDown.Solid.Unit,{

    req(lists$myList[["Solids.Conc.vailed"]]) # Only vailed when button for solid calculation was triggert minimum once

     

    # Get the concentration data from list

    df.solids.Conc.vailed.Unit <- lists$myList[["Solids.Conc.vailed"]]

    

    # Calculate the factor of selected unit

    df.solids.Conc.vailed.Unit[names(df.solids.Conc.vailed.Unit)[c(-1,-2)]] <- df.solids.Conc.vailed.Unit[names(df.solids.Conc.vailed.Unit)[c(-1,-2)]]/mySIUnits(input$dropDown.Solid.Unit)$Factor

    

    # # [BUG] if the User has no admin permission, the input$num.SigDigit is not defined initially and must be loaded from defaults.csv

    if (is.null(input$num.SigDigit)) {

      SigDigit <- unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(Significant.Digits)) %>% pull()

    } else {

      SigDigit <- input$num.SigDigit

    }

      

    # Format the values. Replace zeros by "< BG" and round to significant values.

    df.solids.Conc.nice <- myConcFinal(conc = df.solids.Conc.vailed.Unit,

                                       Analytes = lists$myList$Calibration$Info$Analytes,

                                       SignifDigits = as.numeric(SigDigit),

                                       BGs = NULL)

    

    # Change the attribute Unit to new selection

    attributes(df.solids.Conc.nice)[["Unit"]] <- input$dropDown.Solid.Unit

    

    # Store the new data.frame in the reactive list, that triggers all dependent observers

    lists$myList[["Solids.Conc.nice"]] <- df.solids.Conc.nice

  })



  #__________________________________________

  # [QUALITYCONTROL] ----

  #__________________________________________

  # Tab to define 

  # - new Methods (but already knows Systems, due to system-specific import-files), 

  # - like new SAAs and liked with,

  # - new QC preselections. 

  # - Further define new QC-Standards, 

  # - new or other Standard calibration-files,

  # - Regular Expressions for QC Data identification

  #

  # Best way will be a rHandsOnTable object with editable cells.

  # Plus Button to insert a new line and copy/paste options.

  # -

  # This procedure won't work with SQLite DB!!!

  # -

  #

  # ___ Stll in work ___

  # 

  # Write function can be inherit by write.qc()

  #__________________________________________

  #

  #

  #

  #__________________________________________

  # [CALIBRATION] ----

  ## Table to define the Calibration x-Values

  

  # Function to determine the pre_selection of QC_Dropdown Menue in Input tab and return a pre-defined Calibration file

  # is only triggert by the UI Elements selectInput() and textInput() of the Calibration Box.

  # The preselection is defined in a "default"-Parameter .csv file and loaded by --> dfs$defaults 

  cali.files.pre_selection <- function (cali.files.all) { # Function to determine the pre_selection of QC_Dropdown Menue ----

    # Read the values of stored defaults

    selCali <-  unique(dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(Cali.Default.File))

    if (nrow(selCali) == 0) { # If the Selected SAA.Menue is not Default, the first File is loaded

      return(cali.files.all[[1]])

    }

    if (is.na(selCali)){return()} # If NA is readback, the calibration data is included in export Raw-file

    return(selCali) # If valid matched

  }

  

  # Function to get a vector of filenames relevant for calibration. 

  # Defines exclusions for defaults.csv and DMA80evo calibration File. This files should not been changed. 

  cali.choices <- function(path){ # Exclude some files ----

    exclude <- c("default.csv",

                 "Calib_2018.csv",

                 "SAA_5.1.3_Calibration.csv")

    a <- dir(path,".csv$",full.names = F)

    return(a[!a %in% exclude])

  }

  

  

  

  output$box.CaliDef <- renderUI({  ## Build Calibration Elements in UI ----

    fluidRow(

      box(title = "Limit of quantification",width = 3,

          uiOutput("BG.Selection"),

          numericInput(inputId = "num.Alpha",label = "Alpha",value = 0.01,step = 0.01),

          uiOutput("UI.IntCorr"),

          uiOutput("UI.Means"),

          uiOutput("UI.Outliers")

      ),

    if (as.logical(unique(dfs$defaults %>% filter(System %in% input$dropDown.System) %>% select(Cali.User.Selection)))){

      

       

        box(title = "Calibration Definition",width = 9,

            fluidRow(

              # Structure the Row-Content in diffenrent columns

              column(selectInput(inputId = "select.Cali",

                                 label = "",

                                 choices = cali.choices(path.Calibration),

                                 selected = cali.files.pre_selection(cali.choices(path.Calibration)) # Function to check the pre_selection of QC_Dropdown Menue in Input tab

              ),

              width = 3),

              column(actionButton(inputId = "btn_loadCali",label = "Load"),

                     actionButton(inputId = "btn_delCali",label = "Delete"),

                     width = 12)

            ),

            hr(),

            rHandsontableOutput("tbl.CaliDef"),

            hr(),

            fluidRow(

              column(numericInput(inputId = "num.Rows",label = "Rows",value = NULL,step = 1,min = 2,width = "100%"),width = 2),

              column(numericInput(inputId = "num.Cols",label = "Cols",value = NULL,step = 1,min = 3,width = "100%"),width = 2)

            ),

            hr(),

            textInput(inputId = "txt_Save",

                      label = "Save as:",

                      value = isolate(input$select.Cali), # isolate() avoids a dependancy of the reactive value (recreation of the rendered Object)

                      width = "100%"),

            helpText("The Filename must not end with \".csv\"."),

            hr(),

            actionButton(inputId = "btn_save", "Save"),

            textOutput("txt_ConfirmSave")

        )

    }else{

      helpText("The Calibration will be loaded automatically from loaded Raw-File!")

    }

    )

  })

  

  

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # !!!!!!!!!!!!!!!!!!!!!!!! NEW CALIBRATION FILE FORMAT !!!!!!!!!!!!!!!!!!!!!


  

  observeEvent(input$select.Cali,{ ## Observed functions triggert by changed selected calibration file.

    

    # Store new filename as reactiveValue for further usage in myList

    vals$loaded.Cali.File.Path <- file.path(path.Calibration,input$select.Cali)

    # loading the selected calibration file and store in a reactive list

    dfs$cali.files.selected <- read.csv(file = vals$loaded.Cali.File.Path,

                                        header = F,

                                        na.strings = "",

                                        stringsAsFactors = F)

    # Change the values of UI-Elements to readbacks of loaded file

    updateTextInput(session = session,inputId = "txt_Save",label = "Save as:",value = input$select.Cali)

  })

  

  # Observer triggered by changed values in the handsontable object (table with calibration values).

  # The changes cause the reactivelly stored dataframe to update and further a trigger of the dependent observeEvent

  observeEvent(input$tbl.CaliDef,{

    dfs$cali.files.selected <- hot_to_r(input$tbl.CaliDef)

  })

  

  # Output Observer for the rendering of the table

  output$tbl.CaliDef <- renderRHandsontable({

    rhandsontable(dfs$cali.files.selected,

                  colHeaders = NULL,

                  rowHeaders = NULL,

                  useTypes = FALSE)

  })

  

  # Observed functions triggert by changed loaded calibration file.

  # Further the resulted update tirggers the dependant observeEvent

  observeEvent(dfs$cali.files.selected,{

    # Change the values of UI-Elements to readbacks of loaded file

    updateNumericInput(session = session,inputId = "num.Rows",label = "Rows",value = nrow(dfs$cali.files.selected),step = 1,min = 2)

    updateNumericInput(session = session,inputId = "num.Cols",label = "Cols",value = ncol(dfs$cali.files.selected),step = 1,min = 3)

  })

  

  

  

  observeEvent(input$num.Rows,{ # Dynamic change of Table Row-Dimension ----

    # Abort the initail call without loaded calibration file

    req(input$num.Cols)

    # reset the information text about saving file

    output$txt_ConfirmSave <- renderText({""})

    # Two Conditions are possible

    # 1. Row Value is getting smaller <- Delete a row

    if (nrow(dfs$cali.files.selected) > 0 & nrow(dfs$cali.files.selected) > input$num.Rows) {

      dfs$cali.files.selected <- dfs$cali.files.selected[-nrow(dfs$cali.files.selected),]

    }

    # 2. row Value is getting larger  <- Add an empty row

    if (nrow(dfs$cali.files.selected) > 0 & nrow(dfs$cali.files.selected) < input$num.Rows) {

      dfs$cali.files.selected[nrow(dfs$cali.files.selected) + 1,] <- c(input$num.Rows-1,paste0("S",input$num.Rows-2),rep(NA,ncol(dfs$cali.files.selected)-2))

    }

  })

  

  # [OK] Dynamic change of Table Column-Dimension ----

  observeEvent(input$num.Cols,{


    # Abort the initail call without loaded calibration file

    req(input$num.Cols)

    # reset the information text about saving file

    output$txt_ConfirmSave <- renderText({""})

    # Two Conditions are possible

    # 1. Column Value is getting smaller <- Delete a column

    if (ncol(dfs$cali.files.selected) > 0 & ncol(dfs$cali.files.selected) > input$num.Cols) {

      dfs$cali.files.selected <- dfs$cali.files.selected[,-ncol(dfs$cali.files.selected)]

    }

    # 2. Column Value is getting larger  <- Add an empty column

    if (ncol(dfs$cali.files.selected) > 0 & ncol(dfs$cali.files.selected) < input$num.Cols) {

      dfs$cali.files.selected <- data.frame(dfs$cali.files.selected,

                                            c(paste0("Analyt",ncol(dfs$cali.files.selected)-1),rep(NA,input$num.Rows-1)),

                                            stringsAsFactors = F)

    }

  })

  

  # [OK] Save the Table in csv ----

  observeEvent(input$btn_save, {

    # TODO replace Path----

    fileName <- file.path(path.Calibration,input$txt_Save)

    # Check for Fileformat

    if(substr(fileName,(nchar(fileName)+1)-4,nchar(fileName)) != ".csv"){

      fileName <- paste0(fileName,".csv")

    }

    # Check for override----

    if(!file.exists(fileName)){

      # Write the csv file

      write.table(x =  dfs$cali.files.selected, file = fileName, row.names=F, col.names=F, sep=",",na = "")

      # Display information text

      output$txt_ConfirmSave <- renderText({

        "Successfully saved!"

      })

      # Store new filename as reactiveValue for further usage in myList

      vals$loaded.Cali.File.Path <- fileName

    }else{

      # Show window with question for override with two buttons ("Ok" and "Cancel")

      showModal(

        modalDialog(

          title = "Override?",

          helpText("The file already exists.\nOverride?"),

          footer = tagList(

            actionButton(inputId = "btn_override", label = "OK"),

            modalButton("Cancel")

          )

        )

      )

      

    }

    # Update the selectItem List

    updateSelectInput(session = session,

                      inputId = "select.Cali",

                      label = "",

                      choices = cali.choices(path.Calibration),

                      selected = input$txt_Save)

  })

  

  # [BUG][Solved] When replacement is done twice, the replacement is confirmed without modal! ----

  # SOLVED by argument "ignoreInit = T"

  observeEvent(input$btn_override,ignoreInit = T,{

    # if confirmed for override, write the csv file

    write.table(x =  dfs$cali.files.selected, file = isolate(vals$loaded.Cali.File.Path), row.names=F, col.names=F, sep=",",na = "")

    # Display information text for override

    output$txt_ConfirmSave <- renderText({

      "Successfully replaced!"

    })

    # remove the additional override window

    removeModal()

  })

  

  # [OK] Delete a Calibration file ----

  observeEvent(input$btn_delCali,{

    # reset the information text about saving file

    output$txt_ConfirmSave <- renderText({""})

    # Show window with question for override with two buttons ("Ok" and "Cancel")

    showModal(

      modalDialog(

        title = "Delete?",

        helpText("Please confirm that you really want to delete this file permanently!!!"),

        footer = tagList(

          actionButton(inputId = "btn_delete", label = "Yes"),

          modalButton("NO")

        )

      )

    )

  })

  

  # Delete the file ----

  observeEvent(input$btn_delete,{

    # if confirmed to delete, remove the file from starage

    file.remove(file.path(path.Calibration,input$select.Cali))

    # Display information text for override

    output$txt_ConfirmSave <- renderText({

      "Successfully removed file!"

    })

    # Update the selectItem List

    updateSelectInput(session = session,

                      inputId = "select.Cali",

                      label = "",

                      choices = cali.choices(path.Calibration))

    # Store the selected filename as reactiveValue for further usage in myList

    vals$loaded.Cali.File.Path <- input$select.Cali

    # remove the additional window

    removeModal()

  },ignoreInit = T)



# [Administration Tab] ----

output$tab.Administration <- renderUI({

  fluidRow(

    box(title = "User Controls", width = 6,

        actionButton(inputId = "btn.new.User", label = "New User"),

        actionButton(inputId = "btn.reset.PW", label = "Reset Password"),

        actionButton(inputId = "btn.edit.UserName", label = "Edit Username"),

        actionButton(inputId = "btn.edit.Group", label = "Edit Group"),

        actionButton(inputId = "btn.remove.User", label = "Remove User"),

        hr(),

        DTOutput(outputId = "tbl.Users")

    )

  )

})



  #__________________________________________

  # [ADMINISTRATION] ----

  # [OK] Admin-UserTable ----

  

  

  # Observer zum Reload der User-Datei

  observeEvent(vals$reload_adminDF,{

    dfs$df.ini.UserTable <- read.csv(file = str.UserNames,stringsAsFactors = F)

  })

  

  # Observer für die Generierung der Tabelle in der UI

  output$tbl.Users <- renderDT({

    datatable(dfs$df.ini.UserTable,options=list(pageLength = 100),selection="single")

  })

  

  # [OK] Admin-User Add ----

  # [BUG] Beim erstellen eines User-Dublikats wird der innere Observer für den btn.add.new.user2 Klick so oft wiederholt

  # wie der New-User Button gedrückt wurde. Bisher noch keine Lösung für dieses Problem, da die Dependance nicht klar ist.

  # [LÖSUNG] Durch eine Hilfsvariable (vals$PerformObserver) wird eine If Bedingung gesteuert, die die mehrfache Durchführung

  # des Observers verhindert.

  observeEvent(input$btn.new.User,ignoreInit = T,ignoreNULL = TRUE,{

    showModal(NewUserModal())

    # bei Cancel wird der gesamte Observer neu ausgeführt indem ein künstlicher Button-Click Event generiert wird.

    # Bei einem Button-Clicks wird intern von Shiny der Reactive-Value input$... inkrementiert. Dies wird nun manuell durchgeführt

  })

  

  # Observer, der auf den Add Button reagiert

  observeEvent(input$btn.add.new.user, ignoreInit = T,ignoreNULL = TRUE,{

    if (input$txt.add.password != input$txt.rep.password) {

      removeModal()

      showModal(NewUserModal(checkPW = T,checkUser = F))

    }else if(isFALSE(new.user(str.LoginFile,

                              Vorname = input$txt.add.1stName, 

                              Nachname = input$txt.add.Name,

                              group = input$txt.add.group,

                              password = input$txt.add.password,

                              check.doubles = TRUE ))){

      # new.user() führt einen dubletten check durch und gibt FALSE zurück bei positiven test

      # wenn der test negativ ausfällt wird die R_Viewer.usr Datei automatisch neugeschrieben

      removeModal()

      # Wenn der Dubletten Check eine Dublette registriert hat wird ein neues Modal durch eine Trigger-Variable aufgerufen, 

      # die einen Observer auslöst

      showModal(NewDoubleUserModal())

      vals$PerformObserver <- TRUE

      # Das Modal hat nur zwei Buttons, Add und Cancel, die jeweils einen eigenen Observer auslösen (triggern).

    }else{

      # Wenn bei Erstaufruf der Funktion new.user(...,check.doubles = TRUE) der Dubletten Check negariv war, 

      # wurde die R_Viewer.usr Datei bereits geschrieben und 

      # das Modal wird nur noch entfernt 

      removeModal()

      # Eine Änderung dieses Reactive-Values bewirkt einen automatischen Aufruf des ReactiveEvents 

      # zum einlesen der R_Viewer.usr Datei und damit eine automatische Aktualisierung der Tabelle

      vals$reload_adminDF <- vals$reload_adminDF +1

    }

  })

  

  observeEvent(input$btn.change.new.user, ignoreInit = T,ignoreNULL = TRUE,autoDestroy = TRUE,{

    removeModal()

    showModal(NewUserModal(checkPW = F,checkUser = T))

  })

  

  # Bei Add wird die R-Viewer.usr ohne Dubletten Check neu ausgeführt,

  observeEvent(input$btn.add.new.user2,ignoreInit = TRUE,ignoreNULL = TRUE,autoDestroy = TRUE,{

    removeModal()

    if (vals$PerformObserver) {

      if (new.user(str.LoginFile,

                   Vorname = input$txt.add.1stName, 

                   Nachname = input$txt.add.Name,

                   group = input$txt.add.group,

                   password = input$txt.add.password,

                   check.doubles = FALSE )){

        

        # Triggert den Observer zum Reload der User-Datei

        vals$reload_adminDF <- vals$reload_adminDF +1

        vals$PerformObserver <- FALSE

      }

    }

  })

  # [OK] Admin-User Edit Password ----

  observeEvent(input$btn.reset.PW,ignoreInit = TRUE,{

    showModal(NewPasswordModal())

  })

  

  observeEvent(input$btn.change.password,ignoreInit = TRUE, {

    req(input$tbl.Users_rows_selected)

    if (input$txt.add.password != input$txt.rep.password) {

      removeModal()

      showModal(NewPasswordModal(checkPW = T))

    }else{

      removeModal()

      if (new.pw(str.LoginFile,

                 UserName = dfs$df.ini.UserTable[[input$tbl.Users_rows_selected,"username"]],

                 newPW = input$txt.add.password)) {

        showModal(modalDialog(title = "Successfully changed password",easyClose = T,footer = modalButton("Ok")))

      } else{

        showModal(modalDialog(title = "Ups...something went wrong!",easyClose = T,footer = modalButton("Ok"))) 

      }

    }

    

    vals$reload_adminDF <- vals$reload_adminDF +1

  })

  

  # [OK] Admin-User Remove User ----

  observeEvent(input$btn.remove.User,{

    showModal(RemoveUserModal())

  })

  

  observeEvent(input$btn.rm.user,ignoreInit = TRUE, {

    req(input$tbl.Users_rows_selected)

    remove.user(str.LoginFile,dfs$df.ini.UserTable[[input$tbl.Users_rows_selected,"id"]])

    removeModal()

    

    vals$reload_adminDF <- vals$reload_adminDF +1

  })

  

  # [OK] Admin-User Edit Username ----

  observeEvent(input$btn.edit.UserName,{

    showModal(NewUsernameModal())

  })

  

  observeEvent(input$btn.edit.username,ignoreInit = TRUE, {

    req(input$tbl.Users_rows_selected)

    if (check.username(str.LoginFile,input$txt.edit.Username)) {

      removeModal()

      showModal(NewUsernameModal(checkUser = TRUE))

    } else {

      new.username(str.LoginFile,

                   old.username = dfs$df.ini.UserTable[[input$tbl.Users_rows_selected,"username"]],

                   new.Username = input$txt.edit.Username)

      removeModal()

      

      vals$reload_adminDF <- vals$reload_adminDF +1

    }

  })

  

  # [OK] Admin-User Edit Group ----

  observeEvent(input$btn.edit.Group,{

    showModal(NewGroupModal())

  })

  

  observeEvent(input$btn.edit.group,ignoreInit = TRUE, {

    req(input$tbl.Users_rows_selected)

    new.group(str.LoginFile,

              UserName = dfs$df.ini.UserTable[[input$tbl.Users_rows_selected,"username"]],

              new.Group = input$txt.edit.Group)

    removeModal()

    

    vals$reload_adminDF <- vals$reload_adminDF +1

  })

  

  #__________________________________________

  # [MODALS] for additional Inputs ----

  # [OK] Admin-User Add new User Modal ----

  NewUserModal <- function(checkPW = FALSE,checkUser = FALSE) {

    modalDialog(

      title = "Add new user",

      

      if (checkPW)

        div(tags$b("Password repetition failed", style = "color: red;")),

      if (checkUser)

        div(tags$b("User already exists", style = "color: red;")),

      

      textInput(inputId = "txt.add.1stName",label = "1st Name",placeholder = "Max"),

      textInput(inputId = "txt.add.Name",label = "Surname",placeholder = "Mustermann"),

      selectInput(inputId = "txt.add.group",label = "User group:",choices = c("admin","user"),selected = "user"),

      hr(),

      passwordInput("txt.add.password", "Password:"),

      passwordInput("txt.rep.password", "Repeat Password:"),

      footer = tagList(

        actionButton(inputId = "btn.add.new.user", "Add User"),

        modalButton("Cancel")

      )

    )

  }

  

  NewDoubleUserModal <- function() {

    modalDialog(

      title = "New user exists",

      

      div(tags$b("The user already exists. Do you wish to proceed?", style = "color: red;")),

      footer = tagList(

        actionButton(inputId = "btn.add.new.user2", "Add User"),

        actionButton(inputId = "btn.change.new.user", "Cancel")

      )

    )

  }

  

  # [OK] Admin-User Reset Password Modal ----

  NewPasswordModal <- function(checkPW = FALSE) {

    if (is.null(input$tbl.Users_rows_selected)) {

      modalDialog(title = "Selection failed",

                  helpText("You have to select a user prior to select an action"),

                  footer = modalButton("Ok")

      )

    }else {

      modalDialog(title = dfs$df.ini.UserTable[[input$tbl.Users_rows_selected,"username"]],

                  

                  if (checkPW)

                    div(tags$b("Password repetition failed", style = "color: red;")),

                  hr(),

                  passwordInput("txt.add.password", "Password:"),

                  passwordInput("txt.rep.password", "Repeat Password:"),

                  footer = tagList(

                    actionButton(inputId = "btn.change.password", "Apply"),

                    modalButton("Cancel")

                  )

      )

    }

  }

  

  # [OK] Admin-User Remove User Modal ----

  RemoveUserModal <- function() {

    if (is.null(input$tbl.Users_rows_selected)) {

      modalDialog(title = "Selection failed",

                  helpText("You have to select a user prior to select an action"),

                  footer = modalButton("Ok")

      )

    }else if(dfs$df.ini.UserTable[[input$tbl.Users_rows_selected,"username"]] == "admin"){

      modalDialog(title = "Selection failed",

                  helpText("The admin can't be removed"),

                  footer = modalButton("Ok"))

    }else{

      modalDialog(

        title = paste("Remove",

                      dfs$df.ini.UserTable[[input$tbl.Users_rows_selected,"id"]]),

        p("Are you sure?"),

        footer = tagList(

          actionButton(inputId = "btn.rm.user", "Apply"),

          modalButton("Cancel")

        )

      )

    }

  }

  

  # [OK] Admin-User Edit Username Modal ----

  NewUsernameModal <- function(checkUser = FALSE) {

    if (is.null(input$tbl.Users_rows_selected)) {

      modalDialog(title = "Selection failed",

                  helpText("You have to select a user prior to select an action"),

                  footer = modalButton("Ok")

      )

    }else if(dfs$df.ini.UserTable[[input$tbl.Users_rows_selected,"username"]] == "admin"){

      modalDialog(title = "Selection failed",

                  helpText("The admin can't be edited"),

                  footer = modalButton("Ok"))

    }else{

      modalDialog(

        title = paste("Change the Username of",dfs$df.ini.UserTable[[input$tbl.Users_rows_selected,"Vorname"]],

                      dfs$df.ini.UserTable[[input$tbl.Users_rows_selected,"Nachname"]]),

        if (checkUser)

          div(tags$b("This Username is already in use", style = "color: red;")),

        

        textInput("txt.edit.Username", "New Username:"),

        footer = tagList(

          actionButton(inputId = "btn.edit.username", "Apply"),

          modalButton("Cancel")

        )

      )

    }

  }

  

  # [OK] Admin-User Edit Group Modal ----

  NewGroupModal <- function() {

    if (is.null(input$tbl.Users_rows_selected)) {

      modalDialog(title = "Selection failed",

                  helpText("You have to select a user prior to select an action"),

                  footer = modalButton("Ok")

      )

    }else if(dfs$df.ini.UserTable[[input$tbl.Users_rows_selected,"username"]] == "admin"){

      modalDialog(title = "Selection failed",

                  helpText("The admin can't be edited"),

                  footer = modalButton("Ok"))

    }else{

      modalDialog(title = paste("Change Group of", dfs$df.ini.UserTable[[input$tbl.Users_rows_selected,"username"]]),

                  selectInput("txt.edit.Group", "New Username:",c("admin","user"),"user"),

                  footer = tagList(

                    actionButton(inputId = "btn.edit.group", "Apply"),

                    modalButton("Cancel")

                  )

      )

    }

  }



  #__________________________________________

 

  

  

  #______________________________________________________________________________________________________________________________________

  #______________________________________________________________________________________________________________________________________

  # [Modify defaults.csv Tab] ----

  # _________________________________________________

  # Hier soll ein Tab entstehen, dass die Anzeige und Modifikation gespeicherter Standard-Werte wie QC Regelkarten ermöglicht. 

  # Zudem soll hier das Anlegen neuer bzw. ändern bestehender QC Methoden ermöglicht werden, 

  # d.h., hier sollen neuen Einträge in die defaults.csv möglich sein.

  # _________________________________________________

  output$tab.defaults <- renderUI({

    fluidPage(

      fluidRow(width = 24,

               selectInput(inputId = "dropDown.Defaults.SAA.Menu",label = "SAA Method",choices = unique(dfs$defaults %>% select(SAA.Menue)) %>% pull()),

               #actionButton(inputId = "btn.Defaults.New",label = "New SAA"),

               actionButton(inputId = "btn.Defaults.Save",label = "Save"),

               #actionButton(inputId = "btn.Defaults.Delete",label = "Delete"),

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

  })

  

  # Extract the Single Defaults Parameter for render as Textbox or Checkbox

  output$Single.Defaults <- renderUI({

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



# QC Performance and Evaluation



  # Trigger the Button Click for QC evaluation

  observeEvent(input$btn.perform.QC,{

    # Button click cause the change of a trigger reactive Value. This value is a help variable, to trigger the observer below.

    vals$perform.QC <- vals$perform.QC + 1

  })

  

  # This observer is triggered by the reactive value vals$perform.QC, which is changed by click the "Perform QC" Button.

  # It can also be changed by an other event, for automatically performance.

  observeEvent(vals$perform.QC,{

    req(lists$myList)

     #browser()

    

    # Check if QC is calculated for the first time

    if(is.null(lists$myList$QC)){

      # Trigger variable to re-render the Sidebar and in order to add the QC tab with rendered plots and tables

      vals$performRenderSidebar <-  vals$performRenderSidebar + 1

    }

    

    vec <- c()

    if(as.logical(unique(dfs$defaults %>% filter(SAA.Menue %in% lists$myList$Input.Parameter$SAA.Selected) %>% select(Analyt.User.Selection)))){

      for (i in 1:length(unique(lists$myList$Calibration$Info$Elements))) {

        vec <- c(vec,input[[paste0("Element.",unique(lists$myList$Calibration$Info$Elements)[i])]])

      }

    } else {

      vec <- NULL

    }

    

    

    vals$NonDefaultAnalyt.vec <- vec

    

    

    # Load QC Information

    # Move to QC Tab and click Event!!!!!!

    #____________________________________________________________________________________________________________________________

    # Read values from defaults

    QCDefaults <- dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% select(SAA,QC.Kind,QC.Name,Filename)

    QCSoll <- dfs$defaults %>% filter(SAA.Menue %in% input$dropDown.SAA.Menu) %>% filter(QC.Kind %in% "QC") %>% select(QC.Name,SAA,QC.Norm,Unit.Norm)



    if(!is.null(input[[paste0("QCSoll.",QCSoll$SAA[1],".",QCSoll$QC.Name[1])]])){ # Check if input is given

      # Change defaults based on input

      for (i in 1:nrow(QCSoll)) {

        QCSoll$QC.Norm[i] <- input[[paste0("QCSoll.",QCSoll$SAA[i],".",QCSoll$QC.Name[i])]]

      }

      lists$myList[["Input.Parameter"]][["QC"]][["QCSoll"]] <- QCSoll

      # Read values from defaults

      for (i in 1:nrow(QCDefaults)) {

        lists$myList[["Input.Parameter"]][["QC"]][[QCDefaults$QC.Name[i]]] <- QCDefaults[i,"Filename"]

      }

    }else {

      lists$myParams$QC.Soll <- NA

    }

    #____________________________________________________________________________________________________________________________

    

    

    


    

    # Generate a converted results.list and

    # filter QC relevant Analyt

    # write new QC data in QC Control Chart csv

    # output a list with found QC Data, Control Chart Tables and Diagrams 

    lists$myList[["QC"]] <- myQC.Defaults.csv(results.list = myList.Converter(lists$myList), # Converts the measurement results in a uniform list

                                              df.defaults.QC = dfs$defaults %>% # Extract QC default data

                                                filter(SAA.Menue %in% lists$myList$Input.Parameter$SAA.Selected) %>% # Info about stored QC-Values defined by QC-Method (e.g. "VBW", "MM2" and "MM3" from "SAA 2.3.1 Pt-Gesamt")

                                                select(Analyt,Element,SAA,QC.Kind,QC.Name,QC.RegEx,Filename,Zielwert,Unsicherheit,LimitBG,Unit.Accept,Masse,QC.Spike), # Needed Values

                                              QCSoll = lists$myList$Input.Parameter$QC$QCSoll,

                                              NonDefaultAnalyt.vec = vals$NonDefaultAnalyt.vec,

                                              path.QCtoWrite = path.QCtoWrite) # In the UI defined QCSoll values

    

  })

  

  

  # [TODO] 

  observeEvent(input$chk.BG.SAA,{

    # Function to recalculate final concentrations based on limiit of quantification defined in SAA and stored in defaults.csv 

    # This function can only be used if an correlation between the Analyte and a Signal trace is made. This can be done in defaults.csv

    # like in eQuant, tQuant or DMA80evo, but not in iTeva.

    # In iTeva the user has to choose manually corresponding Traces. Only if this is done and all choices are made, the LoQ can be compared for Report.

    # Additionally, if an SAA for solids was choosen, the conversion of results to solids is required because the BG in SAA is given for solids.

    

    

    if(input$chk.BG.SAA){

      #__________________________________

      # ABORT CONDITIONS HERE

      # Four queries are necessary.

      #

      # 1. defaults.csv: Analyt.User.Selection == TRUE?

      # 2.  NonDefaultAnalyt.vec length same as analytes? (do not count NA's!)

      # 3. defaults.csv: Solids == TRUE?

      # 4.  Was the calculation performed?

      #__________________________________

      

      req(input$dropDown.SAA.Menu)

      # Load Default BGs from defaults.csv 

      BGs <- dfs$defaults %>% # Reads the default value file

        filter(SAA.Menue %in% lists$myList$Input.Parameter$SAA.Selected) %>% # filter for selected QC Method

        filter(QC.Kind %in% "VBW") %>% # filter for selected QC.Kind (LimitBG is only stored for VBW)

        select(LimitBG)

      names(BGs) <- dfs$defaults %>% # Reads the default value file

        filter(SAA.Menue %in% lists$myList$Input.Parameter$SAA.Selected) %>% # filter for selected QC Method

        filter(QC.Kind %in% "VBW") %>% # filter for selected QC.Kind (LimitBG is only stored for VBW)

        select(Analyt)

    }else{

      lists$myParams$BG.SAA <- input$chk.BG.SAA

    }

  })



# QC Delete and Comment Operations



  # Modify QC Table and Charts

  #______________________________________________________________________________________________________________________________________

  #______________________________________________________________________________________________________________________________________

  

  # [BUG] the trigger for delete.QC button stay active after first click. 

  # A help-reactive value is needed for activation of delete.QC function that checks for real trigger click

  

  

  # This function to initiate a reactive value for counting real clicks in dynamically generated QC tabs

  observeEvent(vals$perform.QC,{

    req(lists[["myList"]][["QC"]])

    # get the number of QC tabs and create a vector with equal number of variable = 0

    btns <- rep(0,length(lists[["myList"]][["QC"]]))

    # create an emtpy vector as storage for looped results

    a <- c()

    # loop to generate the names of possible input$tabs names

    for (i in 1:length(btns)) {

      a <- c(a,paste0("qc",i))

    }

    # set names of vector to adress variable in that vector

    names(btns) <- a



    # set this vector as reactive Values for clicks

    vals$btn.click.del.qc <- btns

    vals$btn.click.comment.qc <- btns

  })

  

  ##___________________

  ## DELETE

  #____________________

  # This function proofes the changes in number of clicks and set activate trigger for delete.QC function 

  

  observeEvent(input[[paste0("btn.delete.",input$tabs)]],{

    req(lists[["myList"]][["QC"]])


    if (vals$btn.click.del.qc[[input$tabs]] < unclass(input[[paste0("btn.delete.",input$tabs)]])) {

      # Triggers the Observer for ButtonClick

      vals$btn.click.del.qc[[input$tabs]] <-  unclass(input[[paste0("btn.delete.",input$tabs)]])

    }

  })

  

  # Function to delete a selected QC point

  observeEvent(vals$btn.click.del.qc,{

    req(lists[["myList"]][["QC"]])

    if (any(vals$btn.click.del.qc > 0)) {

      showModal(RemoveQCModal())

    }

  })

  

  # Observer triggered by "Apply" button in modal

  observeEvent(input$btn.rm.qc, {

    req(input[[paste0("tbl.",input$tabs,"_rows_selected")]])

    # Wie bekomme ich nun die Info welche Kontrollkarte gerade angezeigt wird?

    # leider wird der Index des Tabs nicht wiedergegeben. Daher muss ich den aus dem string "qc1" herauslesen

    

    # get index of selected qc tab

    idx <- as.numeric(str_extract(input$tabs,"[[:digit:]]"))

    # get file name of qc table

    FileName <- suppressWarnings(normalizePath(file.path(path.QCtoWrite,lists$myList[["QC"]][[idx]][["QC.defaults"]][["Filename"]])))

    # get StartTime (unique identifier) of selected point

    StartTime <- lists$myList[["QC"]][[idx]][["QC.Table"]][input[[paste0("tbl.",input$tabs,"_rows_selected")]],"StartTime"]

    # delete the selected point from selected QC file

    delete.QC(FileName,StartTime)

    

    # Update the QC Data in myList and triggers the re-render of QC Plots and Tables, since "lists" is reactive.

    lists$myList[["QC"]] <- myQC.Defaults.csv(results.list = myList.Converter(lists$myList), # Converts the measurement results in a uniform list

                                              df.defaults.QC = dfs$defaults %>% # Extract QC default data

                                                filter(SAA.Menue %in% lists$myList$Input.Parameter$SAA.Selected) %>% # Info about stored QC-Values defined by QC-Method (e.g. "VBW", "MM2" and "MM3" from "SAA 2.3.1 Pt-Gesamt")

                                                select(Analyt,Element,SAA,QC.Kind,QC.Name,QC.RegEx,Filename,Zielwert,Unsicherheit,LimitBG,Unit.Accept,Masse,QC.Spike), # Needed Values

                                              QCSoll = lists$myList$Input.Parameter$QC$QCSoll, # In the UI defined QCSoll values

                                              NonDefaultAnalyt.vec = vals$NonDefaultAnalyt.vec,

                                              path.QCtoWrite = path.QCtoWrite,

                                              perform.RegEx = FALSE) 

    # remove the modal

    removeModal()

  })

  

  

  ##___________________

  ## COMMENT

  #____________________

  

  observeEvent(input[[paste0("btn.comment.",input$tabs)]],{

    req(lists[["myList"]][["QC"]])


    if (vals$btn.click.comment.qc[[input$tabs]] < unclass(input[[paste0("btn.comment.",input$tabs)]])) {

      # Triggers the Observer for ButtonClick

      vals$btn.click.comment.qc[[input$tabs]] <- unclass(input[[paste0("btn.comment.",input$tabs)]])

    }

  })

  

  # Function to add a comment to a selected QC point

  # observeEvent(vals$btn.click.comment.qc,{

  observeEvent(vals$btn.click.comment.qc,{


    req(lists[["myList"]][["QC"]])

    if (any(vals$btn.click.comment.qc > 0)) {

      showModal(CommentQCModal())

    }

  })

  

  # Observer triggered by the Apply-Button in CommentQCModal()

  observeEvent(input$btn.cmt.qc, {

    req(input[[paste0("tbl.",input$tabs,"_rows_selected")]])

    #browser()

    # Wie bekomme ich nun die Info welche Kontrollkarte gerade angezeigt wird?

    # leider wird der Index des Tabs nicht wiedergegeben. Daher muss ich den aus dem string "qc1" herauslesen

    

    # get index of selected qc tab

    idx <- as.numeric(str_extract(input$tabs,"[[:digit:]]"))

    # get file name of qc table

    FileName <- suppressWarnings(normalizePath(file.path(path.QCtoWrite,lists$myList[["QC"]][[idx]][["QC.defaults"]][["Filename"]])))

    # get StartTime (unique identifier) of selected point

    StartTime <- lists$myList[["QC"]][[idx]][["QC.Table"]][input[[paste0("tbl.",input$tabs,"_rows_selected")]],"StartTime"]

    Label <- lists$myList[["QC"]][[idx]][["QC.Table"]][input[[paste0("tbl.",input$tabs,"_rows_selected")]],"Labels"]

    

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

    comment.QC(FileName,StartTime,str.comment,TimeStamp,Operator)

    

    # Update the QC Data in myList

    lists$myList[["QC"]] <- myQC.Defaults.csv(results.list = myList.Converter(lists$myList), # Converts the measurement results in a uniform list

                                              df.defaults.QC = dfs$defaults %>% # Extract QC default data

                                                filter(SAA.Menue %in% lists$myList$Input.Parameter$SAA.Selected) %>% # Info about stored QC-Values defined by QC-Method (e.g. "VBW", "MM2" and "MM3" from "SAA 2.3.1 Pt-Gesamt")

                                                select(Analyt,Element,SAA,QC.Kind,QC.Name,QC.RegEx,Filename,Zielwert,Unsicherheit,LimitBG,Unit.Accept,Masse,QC.Spike), # Needed Values

                                              QCSoll = lists$myList$Input.Parameter$QC$QCSoll, # In the UI defined QCSoll values

                                              NonDefaultAnalyt.vec = vals$NonDefaultAnalyt.vec,

                                              path.QCtoWrite = path.QCtoWrite,

                                              perform.RegEx = FALSE) 

    # remove the modal

    removeModal()

  })

  RemoveQCModal <- function() {

    if (is.null(input[[paste0("tbl.",input$tabs,"_rows_selected")]])) { # If no selection was made

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

  

  CommentQCModal <- function() {

    if (is.null(input[[paste0("tbl.",input$tabs,"_rows_selected")]])) { # If no selection was made

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





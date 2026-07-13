#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(dplyr)
require(tidyr)
require(magrittr)
require(stringr)
require(ggplot2)
require(outliers)
require(RColorBrewer)
require(kableExtra)
require(xlsx)
require(DT)
require(tcltk2)


# Source Functions
#Unterfunktionen werden geladen

# Directory Structure
path.root <- normalizePath(getwd())
path.Functions <- normalizePath(paste0(path.root,"/Functions"),winslash = "\\")


# Regular Expression "^func.*.R$" defines that only R-Files beginning by "func" and ending with ".R" are loaded
funcs <- normalizePath(dir(path.Functions,"^func.*.R$",full.names = T))
for (i in funcs) {
  source(i)
}

# Define UI for application that draws a histogram
ui <- 
 
  fluidPage(
    titlePanel("ZE1 Report Generator"),
    navbarPage("Messsystem",
               navbarMenu("ICap-Q",
                          tabPanel("SAA 7.3.1 Pt-Gesamt",value = "Pt-Gesamt",
                                   sidebarLayout(
                                     sidebarPanel(width = 4,
                                                  
                                                  h3('SAA 7.3.1 Pt-Gesamt',style = "color:blue"),
                                                  p('Achtung: Nur mit "Export_Dro03" verwendbar!',style = "color:red"),
                                                  
                                                  fluidRow(
                                                    column(12, 
                                                           textInput(inputId = "ICapQ.731.Operator",
                                                                     label = "Operator",
                                                                     value = "Sadlowski"
                                                           ),
                                                           radioButtons(inputId = "ICapQ.731.BG",
                                                                        label = "BG-Methode",
                                                                        choiceNames = c("DIN32645",
                                                                                        "Leerwertschätzung"),
                                                                        choiceValues = c("DIN32645","Kaiser"))
                                                    )
                                                  ),
                                                  
                                                  fluidRow(
                                                    column(4, 
                                                           br(),
                                                           h4('Verwendete Pipetten'),
                                                           textInput(inputId = "ICapQ.731.Pipette1",
                                                                     label = "Pipetten ID",
                                                                     value = "1067",
                                                                     width = "4cm"
                                                                     
                                                           ),
                                                           textInput(inputId = "ICapQ.731.Pipette2",
                                                                     label = "Pipetten ID",
                                                                     value = "1097",
                                                                     width = "4cm"
                                                           ),
                                                           textInput(inputId = "ICapQ.731.Pipette3",
                                                                     label = "Pipetten ID",
                                                                     value = "1099",
                                                                     width = "4cm"
                                                           ),
                                                           textInput(inputId = "ICapQ.731.Pipette4",
                                                                     label = "Pipetten ID",
                                                                     value = "",
                                                                     width = "4cm"
                                                           ),
                                                           textInput(inputId = "ICapQ.731.Pipette5",
                                                                     label = "Pipetten ID",
                                                                     value = "",
                                                                     width = "4cm"
                                                           )
                                                    ),
                                                    column(8,
                                                           br(),
                                                           br(),
                                                           h4('QC - SAA 7.3.1'),
                                                           fluidRow(column(5,textInput(inputId = "ICapQ.731.QCInput1",
                                                                                       label = "MM2",
                                                                                       value = "50",
                                                                                       width = "4cm")),
                                                                    
                                                                    column(5,selectInput("ICapQ.731.QCUnit1",
                                                                                         label = "Einheit",
                                                                                         choices = c("ng/L","µg/L","mg/L"),
                                                                                         selected = "ng/L",
                                                                                         width = "2cm"))
                                                           ),
                                                           
                                                           
                                                           fluidRow(column(5,textInput(inputId = "ICapQ.731.QCInput2",
                                                                                       label = "MM3",
                                                                                       value = "8216",
                                                                                       width = "4cm")),
                                                                    
                                                                    column(5,selectInput("ICapQ.731.QCUnit2",
                                                                                         label = "Einheit",
                                                                                         choices = c("ng/L","µg/L","mg/L"),
                                                                                         selected = "ng/L",
                                                                                         width = "2cm"))
                                                           )
                                                    )
                                                  ),
                                                  fluidRow(
                                                    br(),
                                                    br(),
                                                    column(2,checkboxInput("ICapQ.731.checkUseMeans","Use Means?",T,width = "2cm")),
                                                    column(2,checkboxInput("ICapQ.731.checkUseIntCorr","Use Internal Correction?",T,width = "4cm")),
                                                    column(8,downloadButton(outputId = "ICapQ.731.report", label = "Generate report"))
                                                  )
                                     ),
                                     mainPanel(
                                       tabsetPanel(id = "PlotsOfSAA731",
                                                           tabPanel("VBW",
                                                                    # selectInput("PlotInput.731VBW",
                                                                    #             label = "Intensität oder Konzentration?",
                                                                    #             choices = c("Intensität","Konzentration"),
                                                                    #             selected = "Konzentration"),
                                                                    plotOutput(outputId = "resultsPlot.731VBW"),
                                                                    verbatimTextOutput("statsOf.731VBW"),
                                                                    dataTableOutput(outputId = "resultsTable.731VBW")
                                                           ),
                                                           tabPanel("MM2",
                                                                    plotOutput(outputId = "resultsPlot.731MM2"),
                                                                    verbatimTextOutput("statsOf.731MM2"),
                                                                    dataTableOutput(outputId = "resultsTable.731MM2")
                                                           ),
                                                           tabPanel("MM3",
                                                                    plotOutput(outputId = "resultsPlot.731MM3"),
                                                                    verbatimTextOutput("statsOf.731MM3"),
                                                                    dataTableOutput(outputId = "resultsTable.731MM3")
                                                           )
                                               )         
                                     )
                                   )
                          ),
                          tabPanel("SAA 7.3.2 Pt-Spezies",value = "Pt-Spezies",
                                   sidebarLayout(
                                     sidebarPanel(width = 4,
                                                  
                                                  h3('SAA 7.3.2 Pt-Spezies',style = "color:green"),
                                                  p('Achtung: Nur mit "Export_DroChrom02" verwendbar!',style = "color:red"),
                                                  
                                                  fluidRow(
                                                    column(12, 
                                                           textInput(inputId = "ICapQ.732.Operator",
                                                                     label = "Operator",
                                                                     value = "Sadlowski"
                                                           ),
                                                           radioButtons(inputId = "ICapQ.732.BG",
                                                                        label = "BG-Methode",
                                                                        choiceNames = c("DIN32645",
                                                                                        "SN (nur bei LC/IC)"),
                                                                        choiceValues = c("DIN32645","SN"),
                                                                        selected = "SN")
                                                    )
                                                  ),
                                                  
                                                  fluidRow(
                                                    column(4, 
                                                           br(),
                                                           h4('Verwendete Pipetten'),
                                                           textInput(inputId = "ICapQ.732.Pipette1",
                                                                     label = "Pipetten ID",
                                                                     value = "1067",
                                                                     width = "4cm"
                                                                     
                                                           ),
                                                           textInput(inputId = "ICapQ.732.Pipette2",
                                                                     label = "Pipetten ID",
                                                                     value = "1097",
                                                                     width = "4cm"
                                                           ),
                                                           textInput(inputId = "ICapQ.732.Pipette3",
                                                                     label = "Pipetten ID",
                                                                     value = "1099",
                                                                     width = "4cm"
                                                           ),
                                                           textInput(inputId = "ICapQ.732.Pipette4",
                                                                     label = "Pipetten ID",
                                                                     value = "",
                                                                     width = "4cm"
                                                           ),
                                                           textInput(inputId = "ICapQ.732.Pipette5",
                                                                     label = "Pipetten ID",
                                                                     value = "",
                                                                     width = "4cm"
                                                           )
                                                    ),
                                                    column(8,
                                                           br(),
                                                           br(),
                                                           h4('QC - SAA 7.3.2'),
                                                           fluidRow(column(5,textInput(inputId = "ICapQ.732.QCSoll.Cis",
                                                                                       label = "Cis-Platin",
                                                                                       value = "5.0",
                                                                                       width = "4cm")),
                                                                    
                                                                    column(5,selectInput("ICapQ.732.QCSollUnit.Cis",
                                                                                         label = "Einheit",
                                                                                         choices = c("ng/L","µg/L","mg/L"),
                                                                                         selected = "µg/L",
                                                                                         width = "2cm"))
                                                           ),
                                                           fluidRow(column(5,textInput(inputId = "ICapQ.732.QCSoll.Carbo",
                                                                                       label = "Carbo-Platin",
                                                                                       value = "5.0",
                                                                                       width = "4cm")),
                                                                    
                                                                    column(5,selectInput("ICapQ.732.QCSollUnit.Carbo",
                                                                                         label = "Einheit",
                                                                                         choices = c("ng/L","µg/L","mg/L"),
                                                                                         selected = "µg/L",
                                                                                         width = "2cm"))
                                                           ),
                                                           fluidRow(column(5,textInput(inputId = "ICapQ.732.QCSoll.Oxali",
                                                                                       label = "Oxali-Platin",
                                                                                       value = "5.0",
                                                                                       width = "4cm")),
                                                                    
                                                                    column(5,selectInput("ICapQ.732.QCSollUnit.Oxali",
                                                                                         label = "Einheit",
                                                                                         choices = c("ng/L","µg/L","mg/L"),
                                                                                         selected = "µg/L",
                                                                                         width = "2cm"))
                                                           )
                                                    )
                                                  ),
                                                  fluidRow(
                                                    br(),
                                                    br(),
                                                    column(8,downloadButton(outputId = "ICapQ.732.report", label = "Generate report"))
                                                  )
                                     ),
                                     mainPanel(
                                       tabsetPanel(id = "PlotsOfSAA732",
                                                   tabPanel("VBW Cis-Platin",
                                                            plotOutput(outputId = "resultsPlot.Cis.VBW"),
                                                            verbatimTextOutput("statsOf.Cis.VBW"),
                                                            dataTableOutput(outputId = "resultsTable.Cis.VBW")
                                                   ),
                                                   tabPanel("VBW Carbo-Platin",
                                                            plotOutput(outputId = "resultsPlot.Carbo.VBW"),
                                                            verbatimTextOutput("statsOf.Carbo.VBW"),
                                                            dataTableOutput(outputId = "resultsTable.Carbo.VBW")
                                                   ),
                                                   tabPanel("VBW Oxali-Platin",
                                                            plotOutput(outputId = "resultsPlot.Oxali.VBW"),
                                                            verbatimTextOutput("statsOf.Oxali.VBW"),
                                                            dataTableOutput(outputId = "resultsTable.Oxali.VBW")
                                                   ),
                                                   tabPanel("MM3 Cis-Platin",
                                                            plotOutput(outputId = "resultsPlot.Cis.MM3"),
                                                            verbatimTextOutput("statsOf.Cis.MM3"),
                                                            dataTableOutput(outputId = "resultsTable.Cis.MM3")
                                                   ),
                                                   tabPanel("MM3 Carbo-Platin",
                                                            plotOutput(outputId = "resultsPlot.Carbo.MM3"),
                                                            verbatimTextOutput("statsOf.Carbo.MM3"),
                                                            dataTableOutput(outputId = "resultsTable.Carbo.MM3")
                                                   ),
                                                   tabPanel("MM3 Oxali-Platin",
                                                            plotOutput(outputId = "resultsPlot.Oxali.MM3"),
                                                            verbatimTextOutput("statsOf.Oxali.MM3"),
                                                            dataTableOutput(outputId = "resultsTable.Oxali.MM3")
                                                   )
                                       )         
                                     )
                                   )
               ),
               tabPanel("Bromid/Bromat",value = "Bromid/Bromat",
                        sidebarLayout(
                          sidebarPanel(h3(expression("Eingaben Bromid/Bromat"))),
                          mainPanel(h3("Plots Bromid/Bromat")))
               ),
               tabPanel("Metalle",value = "iCAPQ-Metalle",
                        sidebarLayout(
                          sidebarPanel(h3("Eingaben Me")),
                          mainPanel(h3("Plots Me")))
               ),
               tabPanel("Aufschluss",value = "iCAPQ-Aufschluss",
                        sidebarLayout(
                          sidebarPanel(h3("Eingaben Aufschluss")),
                          mainPanel(h3("Plots Aufschluss")))
               )
    ),
    navbarMenu("ICap6000",
               tabPanel("Metalle",value = "iCAP6000-Metalle",
                        sidebarLayout(
                          sidebarPanel(h3("Eingaben Metalle")),
                          mainPanel(h3("Plots Metalle")))
               ),
               tabPanel("Aufschluss",value = "iCAP6000-Aufschluss",
                        sidebarLayout(
                          sidebarPanel(h3("Eingaben Aufschluss")),
                          mainPanel(h3("Plots Aufschluss")))
               )
    ),
    navbarMenu("DMA-80 evo",
               tabPanel("SAA 5.1.3 Hg-Gesamt in Rest- und Brennstoffen",value = "Hg-Gesamt",
                        sidebarLayout(
                          sidebarPanel(width = 4,
                                       
                                       h3('SAA 5.1.3 Hg-Gesamt',style = "color:blue"),
                                       
                                       fluidRow(
                                         column(12, 
                                                textInput(inputId = "DMA.513.Operator",
                                                          label = "Operator",
                                                          value = "Sadlowski"
                                                ),
                                                radioButtons(inputId = "DMA.513.BG",
                                                             label = "BG-Methode",
                                                             choiceNames = c("DIN32645",
                                                                             "Leerwertschätzung"),
                                                             choiceValues = c("DIN32645","Kaiser"))
                                         )
                                       ),
                                       
                                       fluidRow(
                                         
                                         column(8,
                                                br(),
                                                br(),
                                                h4('QC - SAA 5.1.3'),
                                                fluidRow(column(5,textInput(inputId = "DMA.513.QCInput1",
                                                                            label = "NIST",
                                                                            value = "92.8",
                                                                            width = "4cm")),
                                                         
                                                         column(5,selectInput("DMA.513.QCUnit1",
                                                                              label = "Einheit",
                                                                              choices = c("ng/kg","µg/kg","mg/kg"),
                                                                              selected = "µg/kg",
                                                                              width = "2cm"))
                                                )
                                         )
                                       ),
                                       fluidRow(
                                         br(),
                                         br(),
                                         column(2,checkboxInput("DMA.513.checkUseMeans","Use Means?",T,width = "2cm")),
                                         column(8,downloadButton(outputId = "DMA.513.report", label = "Generate report"))
                                       )
                          ),
                          mainPanel(
                            tabsetPanel(id = "PlotsOfSAA513",
                                        tabPanel("VBW",
                                                 # selectInput("PlotInput.513VBW",
                                                 #             label = "Intensität oder Konzentration?",
                                                 #             choices = c("Intensität","Konzentration"),
                                                 #             selected = "Konzentration"),
                                                 plotOutput(outputId = "resultsPlot.513VBW"),
                                                 verbatimTextOutput("statsOf.513VBW"),
                                                 dataTableOutput(outputId = "resultsTable.513VBW")
                                        ),
                                        tabPanel("NIST",
                                                 plotOutput(outputId = "resultsPlot.513NIST"),
                                                 verbatimTextOutput("statsOf.513NIST"),
                                                 dataTableOutput(outputId = "resultsTable.513NIST")
                                        )
                            )         
                          )
                        )
               )
    ),
    navbarMenu("TXRF",
               tabPanel("Alles",value = "TXRF-Alles",
                        sidebarLayout(
                          sidebarPanel(h3("Eingaben TXRF")),
                          mainPanel(h3("Plots TXRF")))
               )
    )
  )

)

# Define server logic required to generate Report and plot diagrams
server <- 
  function(input, output) {
    
    # Directory Structure
    path.root <- normalizePath(getwd(),winslash = "\\")
    path.QC <- normalizePath(paste0(path.root,"/QC"),winslash = "\\")
    path.TargetValues <- normalizePath(paste0(path.QC,"/Akzeptanzkriterien"),winslash = "\\")
    path.QCtoWrite <- normalizePath(paste0(path.QC,"/Regelkarten"),winslash = "\\")
    path.Functions <- normalizePath(paste0(path.root,"/Functions"),winslash = "\\")
    path.Calibration <- normalizePath(paste0(path.root,"/Calibration"),winslash = "\\")
    
    ## SAA 7.3.1 
    # Akzeptanzkriterien
    Zielwert.SAA731.VBW    <- normalizePath(paste0(path.TargetValues,"/SAA_7.3.1_MM2_Zielwert.csv"),winslash = "\\")
    Zielwert.SAA731.MM2    <- normalizePath(paste0(path.TargetValues,"/SAA_7.3.1_MM2_Zielwert.csv"),winslash = "\\")
    Zielwert.SAA731.MM3    <- normalizePath(paste0(path.TargetValues,"/SAA_7.3.1_MM3_Zielwert.csv"),winslash = "\\")
    
    # Regelkarten
    Regelk.SAA731.VBW <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.1_VBW.csv"),winslash = "\\")
    Regelk.SAA731.MM2 <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.1_MM2_QC.csv"),winslash = "\\")
    Regelk.SAA731.MM3 <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.1_MM3_QC.csv"),winslash = "\\")
    
    
    ## SAA 7.3.2#
    # Akzeptanzkriterien
    Zielwert.SAA732.CisPt      <- normalizePath(paste0(path.TargetValues,"/SAA_7.3.2_CisPt_Zielwert.csv"),winslash = "\\")
    Zielwert.SAA732.CarboPt    <- normalizePath(paste0(path.TargetValues,"/SAA_7.3.2_CarboPt_Zielwert.csv"),winslash = "\\")
    Zielwert.SAA732.OxaliPt    <- normalizePath(paste0(path.TargetValues,"/SAA_7.3.2_OxaliPt_Zielwert.csv"),winslash = "\\")
    
    # Regelkarten
    Regelk.SAA732.CisPt.VBW   <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.2_CisPt_VBW.csv"),winslash = "\\")
    Regelk.SAA732.CarboPt.VBW <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.2_CarboPt_VBW.csv"),winslash = "\\")
    Regelk.SAA732.OxaliPt.VBW <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.2_OxaliPt_VBW.csv"),winslash = "\\")
    
    Regelk.SAA732.CisPt.QC   <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.2_CisPt_QC.csv"),winslash = "\\")
    Regelk.SAA732.CarboPt.QC <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.2_CarboPt_QC.csv"),winslash = "\\")
    Regelk.SAA732.OxaliPt.QC <- normalizePath(paste0(path.QCtoWrite,"/SAA_7.3.2_OxaliPt_QC.csv"),winslash = "\\")
    
    
    
    
    
    
    # #### SAA 731 ######
    
    output$resultsPlot.731VBW <- renderPlot({
      
      ## Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
      output$statsOf.731VBW <- renderPrint({
        QC731VBW <- read.csv(Regelk.SAA731.VBW,header = TRUE)
        summary(QC731VBW$Concentration)
        
      })
      output$resultsTable.731VBW <- renderDataTable({
        data <- read.csv(Regelk.SAA731.VBW,header = TRUE)
        data$Intensity <- signif(data$Intensity,4)
        data$Concentration <- signif(data$Concentration,4)
        data})
      
      # Generierung des Plots
      myBlindwertkartenPlot2(datapath_df = Regelk.SAA731.VBW,datapath_Value = Zielwert.SAA731.VBW)
    })
    output$resultsPlot.731MM2 <- renderPlot({

      output$statsOf.731MM2 <- renderPrint({
        QC731MM2 <- read.csv(Regelk.SAA731.MM2,header = TRUE)
        summary(QC731MM2$WFR)
      })
      output$resultsTable.731MM2 <- renderDataTable({
        data <- read.csv(Regelk.SAA731.MM2,header = TRUE)
        data$WFR <- signif(data$WFR,4)
        data$Concentration <- signif(data$Concentration,4)
        data})
      ### Generierung des Plots
      myZielwertkartenPlot2(datapath_df = Regelk.SAA731.MM2,datapath_Value = Zielwert.SAA731.MM2)
      ### Generierung der Daten für die BW-Regelkarte

    })
    output$resultsPlot.731MM3 <- renderPlot({

      ### Generierung der Daten für die BW-Regelkarte
      output$statsOf.731MM3 <- renderPrint({
        QC731MM3 <- read.csv(Regelk.SAA731.MM3)
        summary(QC731MM3$WFR)
      })
      output$resultsTable.731MM3 <- renderDataTable({
        data <- read.csv(Regelk.SAA731.MM3,header = TRUE)
        data$WFR <- signif(data$WFR,4)
        data$Concentration <- signif(data$Concentration,4)
        data})
      ### Generierung des Plots
      myZielwertkartenPlot2(datapath_df = Regelk.SAA731.MM3,datapath_Value = Zielwert.SAA731.MM3)


    })
    
  
    # #### SAA 732 ######
    output$resultsPlot.Cis.VBW <- renderPlot({

      ### Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
      output$statsOf.Cis.VBW <- renderPrint({
        QC.Cis.VBW <- read.csv(Regelk.SAA732.CisPt.VBW,header = TRUE)
        summary(QC.Cis.VBW$Concentration)
      })
      output$resultsTable.Cis.VBW <- renderDataTable({
        data <- read.csv(Regelk.SAA732.CisPt.VBW,header = TRUE)
        data$Area <- signif(data$Area,4)
        data$Concentration <- signif(data$Concentration,4)
        data}) 

      # Generierung des Plots
      myBlindwertkartenPlot2(datapath_df = Regelk.SAA732.CisPt.VBW,datapath_Value = Zielwert.SAA732.CisPt)

    })
    output$resultsPlot.Carbo.VBW <- renderPlot({
      
       
      ### Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
      output$statsOf.Carbo.VBW <- renderPrint({
        QC.Carbo.VBW <- read.csv(Regelk.SAA732.CarboPt.VBW,header = TRUE)
        summary(QC.Carbo.VBW$Concentration)
      })
      output$resultsTable.Carbo.VBW <- renderDataTable({
        data <- read.csv(Regelk.SAA732.CarboPt.VBW,header = TRUE)
        data$Area <- signif(data$Area,4)
        data$Concentration <- signif(data$Concentration,4)
        data}) 
      
      # Generierung des Plots
      myBlindwertkartenPlot2(datapath_df = Regelk.SAA732.CarboPt.VBW,datapath_Value = Zielwert.SAA732.CarboPt)
      
    })
    output$resultsPlot.Oxali.VBW <- renderPlot({
      
      
      ### Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
      output$statsOf.Oxali.VBW <- renderPrint({
        QC.Oxali.VBW <- read.csv(Regelk.SAA732.OxaliPt.VBW,header = TRUE)
        summary(QC.Oxali.VBW$Concentration)
      })
      output$resultsTable.Oxali.VBW <- renderDataTable({
        data <- read.csv(Regelk.SAA732.OxaliPt.VBW,header = TRUE)
        data$Area <- signif(data$Area,4)
        data$Concentration <- signif(data$Concentration,4)
        data}) 
      
      # Generierung des Plots
      myBlindwertkartenPlot2(datapath_df = Regelk.SAA732.OxaliPt.VBW,datapath_Value = Zielwert.SAA732.OxaliPt)
      
    })
    
    output$resultsPlot.Cis.MM3 <- renderPlot({
      
      
      ### Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
      output$statsOf.Cis.MM3 <- renderPrint({
        QC.Cis.MM3 <- read.csv(Regelk.SAA732.CisPt.QC,header = TRUE)
        summary(QC.Cis.MM3$WFR)
      })
      output$resultsTable.Cis.MM3 <- renderDataTable({
        data <- read.csv(Regelk.SAA732.CisPt.QC,header = TRUE)
        data$WFR <- signif(data$WFR,4)
        data$Concentration <- signif(data$Concentration,4)
        data}) 
      
      # Generierung des Plots
      myZielwertkartenPlot2(datapath_df = Regelk.SAA732.CisPt.QC,datapath_Value = Zielwert.SAA732.CisPt)
      
    })
    output$resultsPlot.Carbo.MM3 <- renderPlot({
      
      
      ### Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
      output$statsOf.Carbo.MM3 <- renderPrint({
        QC.Carbo.MM3 <- read.csv(Regelk.SAA732.CarboPt.QC,header = TRUE)
        summary(QC.Carbo.MM3$WFR)
      })
      output$resultsTable.Carbo.MM3 <- renderDataTable({
        data <- read.csv(Regelk.SAA732.CarboPt.QC,header = TRUE)
        data$WFR <- signif(data$WFR,4)
        data$Concentration <- signif(data$Concentration,4)
        data}) 
      
      # Generierung des Plots
      myZielwertkartenPlot2(datapath_df = Regelk.SAA732.CarboPt.QC,datapath_Value = Zielwert.SAA732.CarboPt)
      
    })
    output$resultsPlot.Oxali.MM3 <- renderPlot({
      
      
      ### Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
      output$statsOf.Oxali.MM3 <- renderPrint({
        QC.Oxali.MM3 <- read.csv(Regelk.SAA732.OxaliPt.QC,header = TRUE)
        summary(QC.Oxali.MM3$WFR)
      })
      output$resultsTable.Oxali.MM3 <- renderDataTable({
        data <- read.csv(Regelk.SAA732.OxaliPt.QC,header = TRUE)
        data$WFR <- signif(data$WFR,4)
        data$Concentration <- signif(data$Concentration,4)
        data}) 
      
      # Generierung des Plots
      myZielwertkartenPlot2(datapath_df = Regelk.SAA732.OxaliPt.QC,datapath_Value = Zielwert.SAA732.OxaliPt)
      
    })
    
    
    
    # # Download Buttons #######################################################
    # # #### Generate Report Button
    output$ICapQ.731.report <- downloadHandler(
      filename = function() { # Benennung der erzeugten PDF Datei anhand der eingelesenen CSV Datei
        
        if(!is.null(input$FileInput1)){ # Generierung eines Namens für den Report auf Basis der eingelesenen Datei
          paste(str_split_fixed(input$FileInput1$name,"[.]",2)[1], sep = '.', "pdf")
        } else{
          "report.pdf"
        }
        
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        # Code kann im Netz extrahiert werden.
        
        rmdFile <- file.path(tempdir(),"Pt_Gesamt.rmd")
        file.copy("Pt_Gesamt.rmd",rmdFile,overwrite = TRUE)
        
        # Dieser Code funktioniert nicht, da FileInput1 bei Shiny ein Temp-Ordner ist 
        # if(!is.null(input$FileInput1)){
        #   myStaticFilePath <- paste0("\\\\10.1.8.210\\gc-ms-fid\\Analysen\\2018\\Berichte\\iCAPQ Messdaten\\Pt Gesamt\\",input$FileInput1$name)
        # } else{myStaticFilePath <- "NA"}
        myStaticFilePath <- "NA"
        
        
        
        # Set up parameters to pass to Rmd document
        params <- list(inFile = myStaticFilePath,
                       Operator = input$ICapQ.731.Operator,
                       BGMethod = input$ICapQ.731.BG,
                       PipettenList = c(input$ICapQ.731.Pipette1,
                                        input$ICapQ.731.Pipette2,
                                        input$ICapQ.731.Pipette3,
                                        input$ICapQ.731.Pipette4,
                                        input$ICapQ.731.Pipette5),
                       QCSoll_731_MM2 = input$ICapQ.731.QCInput1,
                       QCSoll_731_MM3 = input$ICapQ.731.QCInput2,
                       UseMeans = input$ICapQ.731.checkUseMeans,
                       UseIntCorr = input$ICapQ.731.checkUseIntCorr)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        out <- rmarkdown::render(rmdFile, output_file = file,
                                 params = params,
                                 encoding="UTF-8",
                                 envir = new.env(parent = globalenv()
                                 )
        )
        file.rename(out, file)
        
        
        ###### Update Plots
        # #### SAA 731 ######
        
        output$resultsPlot.731VBW <- renderPlot({
          
           
          ## Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
          output$statsOf.731VBW <- renderPrint({
            QC731VBW <- read.csv(Regelk.SAA731.VBW,header = TRUE)
            summary(QC731VBW$WFR)
            
          })
          output$resultsTable.731VBW <- renderDataTable({
            data <- read.csv(Regelk.SAA731.VBW,header = TRUE)
            data$Intensity <- signif(data$Intensity,4)
            data$Concentration <- signif(data$Concentration,4)
            data})
          
          # Generierung des Plots
          myBlindwertkartenPlot2(datapath_df = Regelk.SAA731.VBW,datapath_Value = Zielwert.SAA731.VBW)
        })
        output$resultsPlot.731MM2 <- renderPlot({
          
          output$statsOf.731MM2 <- renderPrint({
            QC731MM2 <- read.csv(Regelk.SAA731.MM2,header = TRUE)
            summary(QC731MM2$WFR)
          })
          output$resultsTable.731MM2 <- renderDataTable({
            data <- read.csv(Regelk.SAA731.MM2,header = TRUE)
            data$WFR <- signif(data$WFR,4)
            data$Concentration <- signif(data$Concentration,4)
            data})
          ### Generierung des Plots
          myZielwertkartenPlot2(datapath_df = Regelk.SAA731.MM2,datapath_Value = Zielwert.SAA731.MM2)
          ### Generierung der Daten für die BW-Regelkarte
          
        })
        output$resultsPlot.731MM3 <- renderPlot({
          
          ### Generierung der Daten für die BW-Regelkarte
          output$statsOf.731MM3 <- renderPrint({
            QC731MM3 <- read.csv(Regelk.SAA731.MM3)
            summary(QC731MM3$WFR)
          })
          output$resultsTable.731MM3 <- renderDataTable({
            data <- read.csv(Regelk.SAA731.MM3,header = TRUE)
            data$WFR <- signif(data$WFR,4)
            data$Concentration <- signif(data$Concentration,4)
            data})
          ### Generierung des Plots
          myZielwertkartenPlot2(datapath_df = Regelk.SAA731.MM3,datapath_Value = Zielwert.SAA731.MM3)
          
          
        })
        
      }
    )
    output$ICapQ.732.report <- downloadHandler(
      filename = function() { # Benennung der erzeugten PDF Datei anhand der eingelesenen CSV Datei
        
        if(!is.null(input$FileInput1)){ # Generierung eines Namens für den Report auf Basis der eingelesenen Datei
          paste(str_split_fixed(input$FileInput1$name,"[.]",2)[1], sep = '.', "pdf")
        } else{
          "report.pdf"
        }
        
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        # Code kann im Netz extrahiert werden.
        
        rmdFile <- file.path(tempdir(),"Pt_Spezies.rmd")
        file.copy("Pt_Spezies.rmd",rmdFile,overwrite = TRUE)
        
        # Dieser Code funktioniert nicht, da FileInput1 bei Shiny ein Temp-Ordner ist 
        # if(!is.null(input$FileInput1)){
        #   myStaticFilePath <- paste0("\\\\10.1.8.210\\gc-ms-fid\\Analysen\\2018\\Berichte\\iCAPQ Messdaten\\Pt Gesamt\\",input$FileInput1$name)
        # } else{myStaticFilePath <- "NA"}
        myStaticFilePath <- "NA"
        
        
        
        # Set up parameters to pass to Rmd document
        params <- list(inFile = myStaticFilePath,
                       Operator = input$ICapQ.732.Operator,
                       BGMethod = input$ICapQ.732.BG,
                       PipettenList = c(input$ICapQ.732.Pipette1,
                                        input$ICapQ.732.Pipette2,
                                        input$ICapQ.732.Pipette3,
                                        input$ICapQ.732.Pipette4,
                                        input$ICapQ.732.Pipette5),
                       QCSoll.Cis = input$ICapQ.732.QCSoll.Cis,
                       QCSoll.Carbo = input$ICapQ.732.QCSoll.Carbo,
                       QCSoll.Oxali = input$ICapQ.732.QCSoll.Oxali)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        out <- rmarkdown::render(rmdFile, output_file = file,
                                 params = params,
                                 encoding="UTF-8",
                                 envir = new.env(parent = globalenv()
                                 )
        )
        file.rename(out, file)
        
        
        ###### Update Plots
        # #### SAA 732 ######
        output$resultsPlot.Cis.VBW <- renderPlot({
          
          
          ### Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
          output$statsOf.Cis.VBW <- renderPrint({
            QC.Cis.VBW <- read.csv(Regelk.SAA732.CisPt.VBW,header = TRUE)
            summary(QC.Cis.VBW$Concentration)
          })
          output$resultsTable.Cis.VBW <- renderDataTable({
            data <- read.csv(Regelk.SAA732.CisPt.VBW,header = TRUE)
            data$Area <- signif(data$Area,4)
            data$Concentration <- signif(data$Concentration,4)
            data}) 
          
          # Generierung des Plots
          myBlindwertkartenPlot2(datapath_df = Regelk.SAA732.CisPt.VBW,datapath_Value = Zielwert.SAA732.CisPt.VBW)
          
        })
        output$resultsPlot.Carbo.VBW <- renderPlot({
          
          
          ### Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
          output$statsOf.Carbo.VBW <- renderPrint({
            QC.Carbo.VBW <- read.csv(Regelk.SAA732.CarboPt.VBW,header = TRUE)
            summary(QC.Carbo.VBW$Concentration)
          })
          output$resultsTable.Carbo.VBW <- renderDataTable({
            data <- read.csv(Regelk.SAA732.CarboPt.VBW,header = TRUE)
            data$Area <- signif(data$Area,4)
            data$Concentration <- signif(data$Concentration,4)
            data}) 
          
          # Generierung des Plots
          myBlindwertkartenPlot2(datapath_df = Regelk.SAA732.CarboPt.VBW,datapath_Value = Zielwert.SAA732.CarboPt.VBW)
          
        })
        output$resultsPlot.Oxali.VBW <- renderPlot({
          
           
          ### Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
          output$statsOf.Oxali.VBW <- renderPrint({
            QC.Oxali.VBW <- read.csv(Regelk.SAA732.OxaliPt.VBW,header = TRUE)
            summary(QC.Oxali.VBW$Concentration)
          })
          output$resultsTable.Oxali.VBW <- renderDataTable({
            data <- read.csv(Regelk.SAA732.OxaliPt.VBW,header = TRUE)
            data$Area <- signif(data$Area,4)
            data$Concentration <- signif(data$Concentration,4)
            data}) 
          
          # Generierung des Plots
          myBlindwertkartenPlot2(datapath_df = Regelk.SAA732.OxaliPt.VBW,datapath_Value = Zielwert.SAA732.OxaliPt.VBW)
          
        })
        
        output$resultsPlot.Cis.MM3 <- renderPlot({
          
          
          ### Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
          output$statsOf.Cis.MM3 <- renderPrint({
            QC.Cis.MM3 <- read.csv(Regelk.SAA732.CisPt.QC,header = TRUE)
            summary(QC.Cis.MM3$WFR)
          })
          output$resultsTable.Cis.MM3 <- renderDataTable({
            data <- read.csv(Regelk.SAA732.CisPt.QC,header = TRUE)
            data$WFR <- signif(data$WFR,4)
            data$Concentration <- signif(data$Concentration,4)
            data}) 
          
          # Generierung des Plots
          myZielwertkartenPlot2(datapath_df = Regelk.SAA732.CisPt.QC,datapath_Value = Zielwert.SAA732.CisPt)
          
        })
        output$resultsPlot.Carbo.MM3 <- renderPlot({
          
          
          ### Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
          output$statsOf.Carbo.MM3 <- renderPrint({
            QC.Carbo.MM3 <- read.csv(Regelk.SAA732.CarboPt.QC,header = TRUE)
            summary(QC.Carbo.MM3$WFR)
          })
          output$resultsTable.Carbo.MM3 <- renderDataTable({
            data <- read.csv(Regelk.SAA732.CarboPt.QC,header = TRUE)
            data$WFR <- signif(data$WFR,4)
            data$Concentration <- signif(data$Concentration,4)
            data}) 
          
          # Generierung des Plots
          myZielwertkartenPlot2(datapath_df = Regelk.SAA732.CarboPt.QC,datapath_Value = Zielwert.SAA732.CarboPt)
          
        })
        output$resultsPlot.Oxali.MM3 <- renderPlot({
          
          
          ### Generierung der Daten für die BW-Regelkartec("Labels","StartTime","Intensity","Concentration")
          output$statsOf.Oxali.MM3 <- renderPrint({
            QC.Oxali.MM3 <- read.csv(Regelk.SAA732.OxaliPt.QC,header = TRUE)
            summary(QC.Oxali.MM3$WFR)
          })
          output$resultsTable.Oxali.MM3 <- renderDataTable({
            data <- read.csv(Regelk.SAA732.OxaliPt.QC,header = TRUE)
            data$WFR <- signif(data$WFR,4)
            data$Concentration <- signif(data$Concentration,4)
            data}) 
          
          # Generierung des Plots
          myZielwertkartenPlot2(datapath_df = Regelk.SAA732.OxaliPt.QC,datapath_Value = Zielwert.SAA732.OxaliPt)
          
        })
      }
    )
    # #######################################################
  }


# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(DT)
mymtcars = mtcars
mymtcars$id = 1:nrow(mtcars)
runApp(
  list(ui = pageWithSidebar(
    headerPanel('Examples of DataTables'),
    sidebarPanel(
      checkboxGroupInput('show_vars', 'Columns to show:', names(mymtcars),
                         selected = names(mymtcars))
      ,textInput("collection_txt",label="Foo")
    ),
    mainPanel(
      DT::dataTableOutput("mytable")
    )
  )
  , server = function(input, output, session) {
    
    shinyInput <- function(FUN,id,num,...) {
      inputs <- character(num)
      for (i in seq_len(num)) {
        inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
      }
      inputs
    }
    
    rowSelect <- reactive({
      
      rows=names(input)[grepl(pattern = "srows_",names(input))]
      paste(unlist(lapply(rows,function(i){
        if(input[[i]]==T){
          return(substr(i,gregexpr(pattern = "_",i)[[1]]+1,nchar(i)))
        }
      })))
      
    })
    
    observe({
      updateTextInput(session, "collection_txt", value = rowSelect() ,label = "Foo:" )
    })
    output$mytable = DT::renderDataTable({
      #Display table with checkbox buttons
      DT::datatable(cbind(Pick=shinyInput(checkboxInput,"srows_",nrow(mymtcars),value=NULL,width=1), mymtcars[, input$show_vars, drop=FALSE]),
                    options = list(orderClasses = TRUE,
                                   lengthMenu = c(5, 25, 50),
                                   pageLength = 25 ,
                                   
                                   drawCallback= JS(
                                     'function(settings) {
                                     Shiny.bindAll(this.api().table().node());}')
                    ),selection='none',escape=F)
      
      
    } 
    )
    
    
  })
)
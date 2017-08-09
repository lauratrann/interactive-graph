library(shiny)
library(plotly, warn.conflicts = FALSE)
library(rhandsontable)


shinyUI(fluidPage(
  
  headerPanel("R data reader"),
  
  # Input in sidepanel:
  sidebarPanel(
    tags$style(type='text/css', ".well { max-width: 20em; }"),
    # Tags:
    tags$head(
      tags$style(type="text/css", "select[multiple] { width: 100%; height:10em}"),
      tags$style(type="text/css", "select { width: 100%}"),
      tags$style(type="text/css", "input { width: 19em; max-width:100%}")
    ),
    
    # Select filetype:
    selectInput("readFunction", "Function to read data:", 
                c(
                  "read.table",
                  "read.csv",
                  "read.csv2",
                  "read.delim",
                  "read.delim2"
                )),
    
    # Argument selecter:
    htmlOutput("ArgSelect"),
    
    # Argument field:
    htmlOutput("ArgText"),
    
    # Upload data:
    fileInput("file", "Upload data-file:"),
    
    # Variable selection:
    htmlOutput("varselect"),
    
    
    actionButton("do", "Click Me"),
    actionButton("recalc", "recalc"),
    br()
    
  ),
  
  # Main:
  mainPanel(
    rHandsontableOutput("hot"),
    rHandsontableOutput("logicTbl"),
    tableOutput("summaryTable"),
    plotOutput( "plot" )
    
  )
))
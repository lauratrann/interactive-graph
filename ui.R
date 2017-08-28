library(shiny)
library(plotly, warn.conflicts = FALSE)
library(rhandsontable)

shinyUI(fluidPage(
  
  headerPanel("R data reader"),
  
  # Input in sidepanel:
fluidRow( 
  column(2,
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
                  "read.csv"
                )),
    
    # Argument selecter:
    htmlOutput("ArgSelect"),
    
    # Argument field:
    htmlOutput("ArgText"),
    
    # Upload data:
    fileInput("file", "Upload data-file:"),
    
    # Variable selection:
    htmlOutput("varselect"),
    
    actionButton("do", "Click Me")
  ),
  column(2,
    rHandsontableOutput("hot")
  ), 
  column(2,
    rHandsontableOutput("logicTbl")
  ), 
  column(2,
  
   textInput("lEC50", "Lower EC50"),
   verbatimTextOutput("lowerEC50"),
   textInput("lTop", "Lower Top "),
   verbatimTextOutput("lowerTop"),
   textInput("lBot", "Lower Bottom"),
   verbatimTextOutput("lowerBot"),
   textInput("lHill", "Lower Hill"),
   verbatimTextOutput("lowerHill")
 
  ),

  column(2,
       textInput("uEC50", "Upper EC50"),
       verbatimTextOutput("upperEC50"),
       textInput("uTop", "Upper Top"),
       verbatimTextOutput("upperTop"),
       textInput("uBot", "Upper Bottom"),
       verbatimTextOutput("upperBot"),
       textInput("uHill", "Upper Hill"),
       verbatimTextOutput("upperHill")
       ),
  column(2,
       textInput("seedL", "Seed Lower"),
       verbatimTextOutput("lowerSeed"),
       textInput("seedU", "Seed Upper"),
       verbatimTextOutput("upperSeed"),
       textInput("seedHill", "Seed Hill"),
       verbatimTextOutput("hillSeed"),
       textInput("seedEC50", "Seed EC50"),
       verbatimTextOutput("ec50Seed")
)

), 
fluidRow(
  column(8, align="center",
         tableOutput("summaryTable"),
         plotOutput( "plot" )
         ),
  column(4, 
         actionButton("recalc", "Recalculate"),
         actionButton("resetLogical", "Reset the Logic Table")
         
  )
)

))
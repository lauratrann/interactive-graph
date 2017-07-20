library(shiny)
library(plotly, warn.conflicts = FALSE)


shinyUI(fluidPage(
  titlePanel("DR Graph Fit"),
  
  #start of sidebar layout
  sidebarLayout(
    sidebarPanel(
      
      #tags$head(tags$script(src = "message-handler.js")),
      actionButton("do", "Click Me"),
      
      
      
      
      # Copy the line below to make a file upload manager
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
    ),
    
    mainPanel(
      tableOutput(
        'contents'
      ),
      plotOutput(
        "plot" 
      )
      
    )
    
  )
))
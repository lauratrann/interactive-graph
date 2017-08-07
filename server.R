library("drc")
library("reshape2")
library("ggplot2")
library("formattable")
library(rhandsontable)
shinyServer(function(input, output) {
  
  #Argument names:
  ArgNames <- reactive({
    Names <- names(formals(input$readFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  # Argument selector:
  output$ArgSelect <- renderUI({
    if(length(ArgNames())==0) return(NULL)
    selectInput("arg","Argument:",ArgNames())
  })
  
  
  ## Arg text field:
  output$ArgText <- renderUI({
    fun__arg <- paste0(input$readFunction,"__",input$arg)
    if (is.null(input$arg)) return(NULL)
    Defaults <- formals(input$readFunction)
    if (is.null(input[[fun__arg]]))
    {
      textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]]))
    } 
    else 
    {
      textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]])
    }
    
  })
  
  
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    argList <- argList[names(argList) %in% ArgNames()]
    Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
    return(Dataset)
  })
  
  
  
  # Select variables:
  output$varselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    # Variable selection:    
    selectInput("vars", "Variables to use:",
                names(Dataset()), names(Dataset()), multiple =TRUE)            
  })
  
  
  
  dataTbl <- function(){
    output$hot <- renderRHandsontable({
      if (is.null(input$vars) || length(input$vars)==0) return(NULL)
      fitDR <<-Dataset()
      values <<- reactiveValues(data = as.data.frame(fitDR))
      g <<- rhandsontable(values$data)
      return(g)
    })
  }
  
  redoDataTbl <- function(){ 
    output$hot <- renderRHandsontable({
    g <- rhandsontable(values$data)
    return(g)
 
    })
  }
  
  
  observeEvent( input$recalc,{
    values$data <- hot_to_r(input$hot)
    redoDataTbl()
    plotData()

    
  })
  
  
  
  
  removeTFCol <- function(){
    newFitDR[,Keep_Values:=NULL]
  }
  
  
  
  sumTbl <- function(){
    #Show table:
    output$summaryTable <- renderTable(digits = -3, {
      if (is.null(input$vars) || length(input$vars)==0) return(NULL)
      drmHillSlope <- -(coefficientDRM[1])
      drmBottom <- coefficientDRM[2]
      drmTop <- coefficientDRM[3]
      drmEC50 <- coefficientDRM[4]
      drmLogEC50 <- log10(drmEC50)
      drmSpan <- ( drmTop - drmBottom )
      coefficientTable <-list(HillSlope = c(drmHillSlope), ec50 = c(drmEC50), logEC50 = c(drmLogEC50), Bottom = c(drmBottom), Top = c(drmTop), Span = (drmSpan)) 
    })
  }
  
  plotData <- function(){
    output$plot <- renderPlot({
      reshapedX <- reshape2::melt(values$data,id.vars = "X") # get numbers ready for use.
      fitDR.LL.4 <- drm(data = reshapedX,value~X,fct=LL.4(),na.action = na.omit) # run model.
      coefficientDRM <<- coef(fitDR.LL.4)
      # predictions and confidence intervals.
      fitDR.fits <- expand.grid(conc=exp(seq(log(1.00e-04), log(1.00e-09), length=100))) 
      # new data with predictions
      pm <- predict(fitDR.LL.4, newdata=fitDR.fits, interval="confidence") 
      fitDR.fits$p <- pm[,1]
      fitDR.fits$pmin <- pm[,2]
      fitDR.fits$pmax <- pm[,3]
      reshapedX$XX <- reshapedX$X
      reshapedX$XX[reshapedX$XX == 0] <- 1.00e-09  
      p <- ggplot(reshapedX, aes(x = XX, y = value)) +
        geom_point() +
        geom_ribbon(data=fitDR.fits, aes(x=conc, y=p, ymin=pmin, ymax=pmax), alpha=0.2) +
        geom_line(data=fitDR.fits, aes(x=conc, y=p)) +
        coord_trans(x="log")
      print(p) 
      print("this is the plot")
      
    })
  }
  
  observeEvent(input$do, {
    dataTbl()
    plotData()
    sumTbl()
  })
  
})
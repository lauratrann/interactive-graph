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
  
  
  
  printLogicTbl <- function(){
    output$logicTbl <- renderRHandsontable({
    numrows <<- nrow(fitDR)
    logic <<- reactiveValues( data = data.frame(logical = rep(TRUE, numrows) , Y1 = rep(TRUE, numrows)))
    l <<- rhandsontable(logic$data)
    return(l)
    })
  }

  redoLogicTbl <- function(){
      output$logicTbl <- renderRHandsontable({
      logic$data <- hot_to_r(input$logicTbl)
      l <- rhandsontable(logic$data)
      return(l)
    })
  }

  dataTbl <- function(){
    output$hot <- renderRHandsontable({
      if (is.null(input$vars) || length(input$vars)==0) return(NULL)
      fitDR <<-Dataset()
      values <<- reactiveValues(data = as.data.frame(fitDR))
      g <<- rhandsontable(values$data , readOnly = TRUE)
      return(g)
    })
  }
  
  
  
  handleLogical <- function(){
    logic$data <- hot_to_r(input$logicTbl)
    savedLogic <<- logic$data
    savedDF <<- values$data
    for( row in 1:nrow(logic$data)){
      if( !isTRUE(logic$data[row,])){
        values$data[row,] <- NA
      }
    }
    newDF <<- values$data
  }
  
  redoDataTbl <- function(){
    output$hot <- renderRHandsontable({
      values$data <<- newDF
      g <- rhandsontable(values$data, readOnly = TRUE)
      return(g)
    })
  }
  
  getOrigLogic <- function(){
    output$logicTbl <- renderRHandsontable({
      l <- rhandsontable(logic$data)
      return(l)
    })
  }
  
  getOrigData<- function(){
    output$hot<- renderRHandsontable({
      values$data <- savedDF
      print(values$data)
      g <- rhandsontable(values$data, readOnly = TRUE)
      return(g)
    })
  }

  
  # sepData <- function(){
  #   
  #   fitDR <-Dataset()
  #   sep <- split(fitDR, rep(1:2, each = 11))
  #   for( i  in sep){
  #     print(i)
  #   }
  # }

  observeEvent( input$recalc,{
    setLowerL()
    setUpperL()
    setSeed()
    handleLogical()
    redoDataTbl()
    redoLogicTbl()
    plotData()
    sumTbl()
  })
  
  observeEvent(input$resetLogical,{
    printLogicTbl()
    getOrigData()
    plotData()
    sumTbl()
  })
  
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
  
  
  setLowerL<- function(){
    setLowerEC50L()
    setLowerTopL()
    setLowerBotL()
    setLowerHillL()
  }
  
  setUpperL<- function(){
    setUpperEC50L()
    setUpperTopL()
    setUpperBotL()
    setUpperHillL()
    
  }
  
  setSeed <- function(){
    setLowerSeed()
    setUpperSeed()
    setHillSeed()
    setEC50Seed()
  }
  
  setLowerEC50L <- function(){
    if(input$lEC50 != "NULL" || input$lEC50 != ""){ lowerEC50 <<- NULL}
    output$lowerEC50 <- renderPrint({
      lowerEC50 <<- as.numeric(input$lEC50)
    })
  }
  
  setUpperEC50L <- function(){
    if(input$uEC50 != "NULL" || input$uEC50 != ""){ upperEC50 <<- NULL}
    output$upperEC50 <- renderPrint({
      upperEC50 <<- as.numeric(input$uEC50)
    })
  }
  
  setLowerTopL<- function(){
    if(input$lTop != "NULL" || input$lTop != ""){ lowerTop <<- NULL}
    output$lowerTop <- renderPrint({
      lowerTop <<- as.numeric(input$lTop)
    })
  }
  
  setUpperTopL<- function(){
    if(input$uTop != "NULL" || input$uTop != ""){ upperTop <<- NULL}
    output$upperTop <- renderPrint({
      upperTop <<- as.numeric(input$uTop)
    })
  }
  
  setLowerBotL<- function(){
    if(input$lBot != "NULL" || input$lBot != ""){ lowerBot <<- NULL}
    output$lowerBot <- renderPrint({
      lowerBot <<- as.numeric(input$lBot)
    })
  }
  
  setUpperBotL<- function(){
    if(input$uBot != "NULL" || input$uBot != ""){ upperBot <<- NULL}
    output$upperBot <- renderPrint({
      upperBot <<- as.numeric(input$uBot)
    })
  }
  
  setLowerHillL<- function(){
    if(input$lHill != "NULL" || input$lHill != ""){ lowerHill <<- NULL}
    output$lowerHill <- renderPrint({
      lowerHill <<- as.numeric(input$lHill)
    })
  }
  
  setUpperHillL<- function(){
    if(input$uHill != "NULL" || input$uHill != ""){ upperHill <<- NULL}
    output$upperHill <- renderPrint({
      upperHill <<- as.numeric(input$uHill)
    })
  }
  
  setLowerSeed<- function(){
    if(input$seedL != "NULL" || input$seedL != ""){ lowerSeed <<- 0}
    output$lowerSeed <- renderPrint({
      lowerSeed <<- as.numeric(input$seedL)
    })
  }
  
  setUpperSeed<- function(){
    if(input$seedU != "NULL" || input$seedU != ""){ upperSeed <<- 0}
    output$upperSeed <- renderPrint({
      upperSeed <<- as.numeric(input$seedU)
    })
  }
  
  setHillSeed <-function(){
    if(input$seedHill != "NULL" || input$seedHill != ""){ hillSeed <<- 0}
    output$hillSeed <- renderPrint({
      hillSeed <<- as.numeric(input$seedHill)
    })
  }
  
  setEC50Seed <- function(){
    if(input$seedEC50 != "NULL" || input$seedEC50 != ""){ ec50Seed <<- 0}
    output$ec50Seed <- renderPrint({
      ec50Seed <<- as.numeric(input$seedEC50)
    })
  }
  
 
  
  plotData <- function(){
    output$plot <- renderPlot({
      if(isTRUE(noSeed)){
        reshapedX <- reshape2::melt(values$data,id.vars = "X") # get numbers ready for use.g
        fitDR.LL.4 <- drm(data = reshapedX,value~X,fct=LL.4(), na.action = na.omit, lowerl=c(lowerHill, lowerBot, lowerTop, lowerEC50), upperl=c(upperHill, upperBot, upperTop, upperEC50)) # run model.
        print(summary(fitDR.LL.4))
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
      }
      else{
        reshapedX <- reshape2::melt(values$data,id.vars = "X") # get numbers ready for use.g
        fitDR.LL.4 <- drm(data = reshapedX,value~X,fct=LL.4(), start = c(hillSeed, lowerSeed, upperSeed, ec50Seed), na.action = na.omit, lowerl=c(lowerHill, lowerBot, lowerTop, lowerEC50), upperl=c(upperHill, upperBot, upperTop, upperEC50)) # run model.
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
      }
      
    })
  }
  
  observeEvent(input$do, {
    noSeed <<- TRUE
    setLowerL()
    setUpperL()
    setSeed()
    dataTbl()
    printLogicTbl()
    plotData()
    sumTbl()
  })
  
  
})
library("drc")
library("reshape2")
library("ggplot2")
library("formattable")
shinyServer(function(input, output){
  demo=structure(list(X = c(0,0,0,0,0,0,0,0,0,0),
                      Y1 = c(0,0,0,0,0,0,0,0,0,0),
                      Y2 = c(0,0,0,0,0,0,0,0,0,0)),
                 .Names = c("X", "Y1", "Y2"), class = "data.frame", row.names = c(NA, -10L ))
  
  
  
  output$contents <- renderTable(digits = -2,{
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
      fileExists <- TRUE 
    }
    
    Data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                     quote=input$quote)
    
    print(Data)
    demo <<- structure(list(X = Data[,1], Y1 = Data[,2], Y2 = Data[,3]),  .Names = c("X", "Y1", "Y2"), class = "data.frame", row.names = c(NA, -10L ))
    
    print(demo)
  })
  
  
  observeEvent(input$do, {
    output$plot <- renderPlot({
      
      
      demo1 <- reshape2::melt(demo,id.vars = "X") # get numbers ready for use.
      demo.LL.4 <- drm(data = demo1,value~X,fct=LL.4(),na.action = na.omit) # run model.
      
      # predictions and confidence intervals.
      demo.fits <- expand.grid(conc=exp(seq(log(1.00e-04), log(1.00e-09), length=100))) 
      # new data with predictions
      pm <- predict(demo.LL.4, newdata=demo.fits, interval="confidence") 
      demo.fits$p <- pm[,1]
      demo.fits$pmin <- pm[,2]
      demo.fits$pmax <- pm[,3]
      
      demo1$XX <- demo1$X
      demo1$XX[demo1$XX == 0] <- 1.00e-09  
      
      ggplot(demo1, aes(x = XX, y = value)) +
        geom_point() +
        geom_ribbon(data=demo.fits, aes(x=conc, y=p, ymin=pmin, ymax=pmax), alpha=0.2) +
        geom_line(data=demo.fits, aes(x=conc, y=p)) +
        coord_trans(x="log")
    })
  })
})
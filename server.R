library("shiny")
library("Rglpk")
library("XLConnect")
library("modopt.matlab")

shinyServer(function(input, output) {

  data <- reactive({  
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  output$summary <- renderPrint({
    summary(data())
  })
  
  output$excelTable <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    rawExcelmainMatrix <- readWorksheet(loadWorkbook(inFile$datapath), sheet = 2)
    rawExcelmainMatrix <- rawExcelmainMatrix[-1,] # remove line with variable names
  })
})

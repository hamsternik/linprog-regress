library("shiny")
library("Rglpk")
library("XLConnect")
library("modopt.matlab")

source("dataFrameFromTwoLists.R")

shinyServer(function(input, output, session) {
  
  rawExcelDataInput <- reactive({
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    rawExcelmainMatrix <- readWorksheet(loadWorkbook(inFile$datapath), sheet = 2)
    rawExcelmainMatrix <- rawExcelmainMatrix[-1,] # remove line with variable names
    
    return(rawExcelmainMatrix)
  })
  
  mapOfNamesDataInput <- reactive({
    
    rawExcelmainMatrix <- rawExcelDataInput()
    if (is.null(rawExcelmainMatrix))
      return(NULL)
    
    ### get column names from excel mainMatrixbase
    columnNames <- colnames(rawExcelmainMatrix)
        
    ### create variable names
    variableNames <- c()
    for (i in 1:length(rawExcelmainMatrix)) {
      variableNames <- append(variableNames,  paste("X", toString(i), sep = ""))
    }
        
    ### hash-map of column names and variable names
    mapOfNames <- list()
    for (i in 1:length(rawExcelmainMatrix)) {
      mapOfNames[[variableNames[i]]] <- columnNames[i]
    }
        
    mapOfNames.df <- dataFrameFromTwoLists(columnNames, variableNames)
    return(mapOfNames.df)
  })
  
  ################################################################################################
  
  output$excelTable <- renderTable({
    rawExcelDataInput()
  })
  
  output$criterion <- renderUI({
    selectizeInput('cn', 'Criterior Variables', choices = mapOfNamesDataInput(), multiple = TRUE)
  })
  output$state <- renderUI({
    selectizeInput('st', 'State Variables', choices = mapOfNamesDataInput(), multiple = TRUE)
  })
  output$control <- renderUI({
    selectizeInput('cl', 'Control Variables', choices = mapOfNamesDataInput(), multiple = TRUE)
  })
  
})

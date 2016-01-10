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
    
    return(mapOfNames)
  })
  
  mapOfNamesAsDataFrameDataInput <- reactive({
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
  
  criterionVariablesNumberDataInput <- reactive({
    criterionVariablesNumber <- length(input$cn)
    return(criterionVariablesNumber)
  })
  
  criterionVariablesDataInput <- reactive({
    criterionVariables <- as.vector(input$cn)
    return(criterionVariables)
  })
  
  stateVariablesNumberDataInput <- reactive({
    stateVariablesNumber <- length(input$st)
    return(stateVariablesNumber)
  })
  
  stateVariablesDataInput <- reactive({
    stateVariables <- as.vector(input$st)
    return(stateVariables)
  })
  
  controlVariablesNumberDataInput <- reactive({
    controlVariablesNumber <- length(input$cl)
    return(controlVariablesNumber)
  })
  
  controlVariablesDataInput <- reactive({
    controlVariables <- as.vector(input$cl)
    return(controlVariables)
  })
  
  mainMatrixDataInput <- reactive({
    mainMatrix <- matrix()
    rawExcelmainMatrix <- rawExcelDataInput()
    mapOfNames <- mapOfNamesDataInput()
    
    for (i in 1:(criterionVariablesNumberDataInput() + stateVariablesNumberDataInput() + controlVariablesNumberDataInput())) {
      if ((i > criterionVariablesNumberDataInput()) && (i <= (criterionVariablesNumberDataInput() + stateVariablesNumberDataInput()))) {
        j <- stateVariablesDataInput()[i - criterionVariablesNumberDataInput()]
        jj <- mapOfNames[[j]]
        mainMatrix <- cbind(mainMatrix, as.numeric(rawExcelmainMatrix[[jj]]))
      }
      else if (i > (criterionVariablesNumberDataInput() + stateVariablesNumberDataInput())) {
        j <- controlVariablesDataInput()[i - (criterionVariablesNumberDataInput() + stateVariablesNumberDataInput())]
        jj <- mapOfNames[[j]]
        mainMatrix <- cbind(mainMatrix, as.numeric(rawExcelmainMatrix[[jj]]))
      }
      else {
        j <- criterionVariablesDataInput()[i]
        jj <- mapOfNames[[j]]
        mainMatrix <- cbind(as.numeric(rawExcelmainMatrix[[jj]]))
      }
    }
    
    return(mainMatrix)
  })
  
  ################################################################################################
  
  output$excelTable <- renderTable({
    rawExcelDataInput()
  })
  
  output$criterion <- renderUI({
    selectizeInput('cn', 'Criterior Variables', choices = mapOfNamesAsDataFrameDataInput(), multiple = TRUE)
  })
  output$state <- renderUI({
    selectizeInput('st', 'State Variables', choices = mapOfNamesAsDataFrameDataInput(), multiple = TRUE)
  })
  output$control <- renderUI({
    selectizeInput('cl', 'Control Variables', choices = mapOfNamesAsDataFrameDataInput(), multiple = TRUE)
  })
  
  output$optimumResult <- renderPrint({
    ## TODO
  })
  
})

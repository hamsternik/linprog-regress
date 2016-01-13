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
    return(mapOfNames.df$parameters)
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
    rawExcelmainMatrix <- rawExcelDataInput()
    mapOfNames <- mapOfNamesDataInput()
    
    mainMatrix <- matrix()
    
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
  
  constrainsDataInput <- reactive({
    stateAndControlVecOfConstrains <- c()
    for (i in (criterionVariablesNumberDataInput() + 1)
         :(criterionVariablesNumberDataInput() + stateVariablesNumberDataInput() + controlVariablesNumberDataInput())) {
      stateAndControlVecOfConstrains <- rbind(stateAndControlVecOfConstrains, c(min(mainMatrixDataInput()[,i]), max(mainMatrixDataInput()[,i])))
    }
    
    colnames(stateAndControlVecOfConstrains) <- c("min", "max")
    return(stateAndControlVecOfConstrains)
  })
  
  additionMatrixDataInput <- reactive({
    rawExcelmainMatrix <- rawExcelDataInput()
    mapOfNames <- mapOfNamesDataInput()
    mainMatrix <- mainMatrixDataInput()
    
    additionMatrix <- c()
    
    for (i in 1:(criterionVariablesNumberDataInput() + stateVariablesNumberDataInput()))
      additionMatrix <- cbind(additionMatrix, mainMatrix[,i])
    additionMatrix <- additionMatrix[-(dim(additionMatrix)[1]),]
    additionMatrix <- rbind(rep(1, (dim(additionMatrix)[2])), additionMatrix)
    
    return(additionMatrix)
  })
  
  extendedMatrixDataInput <- reactive({
    extendedMatrix <- cbind(additionMatrixDataInput(), mainMatrixDataInput())
    extendedMatrix <- extendedMatrix[-1, ]
    return(extendedMatrix)
  })
  
  criterionRegressDataInput <- reactive({
    criterionRegress <- c()
    for (i in 1:criterionVariablesNumberDataInput()) {
      X <- c()
      
      for (j in 1:controlVariablesNumberDataInput())
        X <- cbind(X, mainMatrixDataInput()[,criterionVariablesNumberDataInput() + stateVariablesNumberDataInput() + j])
      
      X <- cbind(X, additionMatrixDataInput())
      
      yCriterion <- t(t(mainMatrixDataInput()[, i]))
      
      criterionRegressFitFunction <- lm.fit(X, yCriterion)
      criterionRegress <- rbind(criterionRegress, criterionRegressFitFunction$coefficients)
    }
    
    criterionRegressConstants <- c()
    for (i in 1:((dim(criterionRegress))[1])) {
      f = criterionRegress[i,]
      criterionRegressConstants <- append(criterionRegressConstants, mean(yCriterion) - t(apply(X, 2, mean))%*%t(t(f)))
    }
    
    answer <- list(first = criterionRegress, second = criterionRegressConstants)
    return(answer)
  })
  
  stateRegressDataInput <- reactive({
    stateRegress <- c()
    for (i in (criterionVariablesNumberDataInput() + 1):(criterionVariablesNumberDataInput() + stateVariablesNumberDataInput())) {
      X <- c()
      
      for (j in (criterionVariablesNumberDataInput() + 1):(criterionVariablesNumberDataInput() + stateVariablesNumberDataInput())) {
        if (j != i)
          X <- cbind(X, mainMatrixDataInput()[,j])
        else
          X <- cbind(X, rep(0, dim(mainMatrixDataInput())[1])) 
      }
      
      for (j in (criterionVariablesNumberDataInput() + stateVariablesNumberDataInput() + 1)
           :(criterionVariablesNumberDataInput() + stateVariablesNumberDataInput() + controlVariablesNumberDataInput())) {
        X <- cbind(X, mainMatrixDataInput()[, j])
      }
      
      X <- cbind(X, additionMatrixDataInput())
      
      yState <- t(t(mainMatrixDataInput()[, i]))
      
      stateRegressFitFunction <- lm.fit(X, yState)
      stateRegress <- rbind(stateRegress, stateRegressFitFunction$coefficients)
    }
    
    for (i in 1:dim(stateRegress)[1])
      for (j in 1:dim(stateRegress)[2])
        if (is.na(stateRegress[i,j]))
          stateRegress[i,j] <- 0
    
    stateRegressConstants <- c()
    for (i in 1:((dim(stateRegress))[1])) {
      f = stateRegress[i,]
      stateRegressConstants <- append(stateRegressConstants, mean(yState) - t(apply(X, 2, mean))%*%t(t(f)))
    }
    
    answer <- list(first = stateRegress, second = stateRegressConstants)
    return(answer)
  })
  
  timePointDataInput <- reactive({
    timeLine <- input$timeLine1
    additionMatrix <- additionMatrixDataInput()
    answer <- additionMatrix[timeLine, ]
    
    return(answer)
  })
  
  
  modifiedCriterionRegressConstantsDataInput <- reactive({
    parametrsBefore <- (criterionRegressDataInput()$first)[(controlVariablesNumberDataInput() + 1):length(criterionRegressDataInput()$first)]
    criterionRegressConstants <- (timePointDataInput() %*% parametrsBefore) + (criterionRegressDataInput()$second)
    
    return(criterionRegressConstants)
  })
  
  criterionRegressForLP <- reactive({
    criterionRegressLP <- (criterionRegressDataInput()$first)[,1:controlVariablesNumberDataInput()]
    criterionRegressLP <- append(criterionRegressLP, rep(0, stateVariablesNumberDataInput()))
    
    return(criterionRegressLP)
  })
  
  modifiedStateRegressConstantsDataInput <- reactive({
    lastStateConstantVec <- c()
    stateRegressPartOfConstantValues <- (stateRegressDataInput()$first)[,( ((dim((stateRegressDataInput()$first)))[2]) - (dim(additionMatrixDataInput())[2]) + 1 ):(dim((stateRegressDataInput()$first)))[2]]
    for (i in 1:dim(stateRegressPartOfConstantValues)[1]) {
      lastStateConstantVec <- append(lastStateConstantVec, ((timePointDataInput() %*% stateRegressPartOfConstantValues[i,]) + stateRegressConstants[i]))
    }
    
    return(lastStateConstantVec)
  })
  
  stateRegressForLP <- reactive({
    return((stateRegressDataInput()$first)[,1:(stateVariablesNumberDataInput() + controlVariablesNumberDataInput())])
  })
  
  linprogDataInput <- reactive({
    lp <- linprog(f = criterionRegressForLP(),
                   A = stateRegressForLP(),
                   b = -modifiedStateRegressConstantsDataInput(),
                   Aeq = c(), 
                   beq = c(), 
                   lb = constrainsDataInput()[,"min"], 
                   ub = constrainsDataInput()[,"max"])
    return(lp$x)
  })
  
  optimumCriterionDataInput <- reactive({
    return(as.numeric(linprogDataInput() %*% criterionRegressForLP() + modifiedCriterionRegressConstantsDataInput()))
  })
  
  ################################################################################################
  
  ### Table Render
  output$excelTable <- renderTable({
    rawExcelDataInput()
  })
  output$extendedMatrixTable <- renderTable({
    extendedMatrixDataInput()
  })
  
  ### Data Table Render
  output$criterionCoefTable <- renderDataTable({
    as.matrix(criterionRegressDataInput()$first)
  })
  output$stateCoefTable <- renderDataTable({
    stateRegressDataInput()$first
  })
  
  ### UI Render
  output$criterion <- renderUI({
    selectizeInput('cn', 'Criterior Variables', choices = mapOfNamesAsDataFrameDataInput(), multiple = TRUE)
  })
  output$state <- renderUI({
    selectizeInput('st', 'State Variables', choices = mapOfNamesAsDataFrameDataInput(), multiple = TRUE)
  })
  output$control <- renderUI({
    selectizeInput('cl', 'Control Variables', choices = mapOfNamesAsDataFrameDataInput(), multiple = TRUE)
  })
  output$timeLine <- renderUI({
    if (is.null(rawExcelDataInput()))
      return(NULL)
    else
      numericInput('timeLine1', '', 2, 2, (dim(rawExcelDataInput()))[1])
  })
  
  ### Text Render Through UI Render
  output$criterionCoefTitle <- renderUI({
    HTML(paste("<b>", "Regression coefficients of criterion variables", "</b>"))
  })
  output$stateCoefTitle <- renderUI({
    HTML(paste("<b>", "Regression coefficients of state variables", "</b>"))
  })
  timeLineTitleReactive <- reactive({
    if (is.null(rawExcelDataInput()) || is.null(input$cn))
      HTML(paste("<b>", "Waiting...", "</b>"))
    else
      HTML(paste("<b>", "Enter number of concrete time from ", ((dim(mainMatrixDataInput())[1]) - 1), " time points: ", "</b>"))
  })
  output$timeLineTitle <- renderUI({
    timeLineTitleReactive()
  })
  
  ### Text Render
  output$optimumResult <- renderText({
    optimumCriterionDataInput()
  })
  
})

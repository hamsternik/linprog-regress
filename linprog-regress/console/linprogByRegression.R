library("Rglpk")
library("XLConnect")
library("modopt.matlab")

source("/home/hamsternik/1. University/5 semestr/operation research/kursach/code/linprog-regress/dataFrameFromTwoLists.R")

#### get Excel worksheet
filePath <- "/home/hamsternik/1. University/5 semestr/operation research/kursach/code/linprog-regress/data/Ukraine.xlsx"
rawExcelmainMatrix <- readWorksheet(loadWorkbook(filePath), sheet = 2)
rawExcelmainMatrix <- rawExcelmainMatrix[-1,] # remove line with variable names

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

#### criterion variables
criterionVariablesNumber <- as.numeric(readline("Enter number of criterion variables: "))
criterionVariables <- c()
for (i in 1:criterionVariablesNumber) {
  criterionVar <- readline(paste0("Enter ", i, " criterion variable: "))
  criterionVariables <- append(criterionVariables, criterionVar)
}

#### state variables
stateVariablesNumber <- as.numeric(readline("Enter number of state variables: "))
stateVariables <- c()
for (i in 1:stateVariablesNumber) {
  stateVar <- readline(paste0("Enter ", i, " state variable: "))
  stateVariables <- append(stateVariables, stateVar)
}

#### control variables
controlVariablesNumber <- as.numeric(readline("Enter number of control variables: "))
controlVariables <- c()
for (i in 1:controlVariablesNumber) {
  controlVar <- readline(paste0("Enter ", i, " control variable: "))
  controlVariables <- append(controlVariables, controlVar)
}

### bind all excel mainMatrix matrix columns in new matrix
mainMatrix <- matrix()
for (i in 1:(criterionVariablesNumber + stateVariablesNumber + controlVariablesNumber)) {
  if ((i > criterionVariablesNumber) && (i <= (criterionVariablesNumber + stateVariablesNumber))) {
     j <- stateVariables[i - criterionVariablesNumber]
     jj <- mapOfNames[[j]]
     mainMatrix <- cbind(mainMatrix, as.numeric(rawExcelmainMatrix[[jj]]))
  }
  else if (i > (criterionVariablesNumber + stateVariablesNumber)) {
    j <- controlVariables[i - (criterionVariablesNumber + stateVariablesNumber)]
    jj <- mapOfNames[[j]]
    mainMatrix <- cbind(mainMatrix, as.numeric(rawExcelmainMatrix[[jj]]))
  }
  else {
    j <- criterionVariables[i]
    jj <- mapOfNames[[j]]
    mainMatrix <- cbind(as.numeric(rawExcelmainMatrix[[jj]]))
  }
}
###

### constrains for state variables
stateAndControlVecOfConstrains <- c()
for (i in (criterionVariablesNumber + 1)
     :(criterionVariablesNumber + stateVariablesNumber + controlVariablesNumber)) {
  stateAndControlVecOfConstrains <- rbind(stateAndControlVecOfConstrains, c(min(mainMatrix[,i]), max(mainMatrix[,i])))
}
colnames(stateAndControlVecOfConstrains) <- c("min", "max")
#print(stateAndControlVecOfConstrains)

# change 'stateAndControlVecOfConstrains' constrains!
# stateAndControlVecOfConstrains[1,"min"] <-  10.0
# stateAndControlVecOfConstrains[8,"min"] <-  456.0
# stateAndControlVecOfConstrains[6,"max"] <- 3000.0
###

### create addition matrix
additionMatrix <- matrix()
additionMatrix <- cbind(as.numeric(rawExcelmainMatrix[[mapOfNames[[criterionVariables[1]]]]]))
additionMatrix <- cbind(additionMatrix, as.numeric(rawExcelmainMatrix[[mapOfNames[[stateVariables[1]]]]]))
additionMatrix <- cbind(additionMatrix, as.numeric(rawExcelmainMatrix[[mapOfNames[[stateVariables[2]]]]]))
additionMatrix <- cbind(additionMatrix, as.numeric(rawExcelmainMatrix[[mapOfNames[[stateVariables[3]]]]]))
additionMatrix <- cbind(additionMatrix, as.numeric(rawExcelmainMatrix[[mapOfNames[[stateVariables[4]]]]]))
additionMatrix <- cbind(additionMatrix, as.numeric(rawExcelmainMatrix[[mapOfNames[[stateVariables[5]]]]]))

additionMatrix <- additionMatrix[-(dim(additionMatrix)[1]),]
additionMatrix <- rbind(rep(1, (dim(additionMatrix)[2])), additionMatrix)
### 

### create extended matrix with criterion var. and state variables before
extendedMatrix <- cbind(additionMatrix, mainMatrix)
#print(extendedMatrix)

### count regress for criterion variable
# first parametr in linprog: 'criterionRegress'
criterionRegress <- c()
for (i in 1:criterionVariablesNumber) {
  X <- c()
  
  for (j in 1:controlVariablesNumber)
    X <- cbind(X, mainMatrix[,criterionVariablesNumber+stateVariablesNumber+j])
  
  X <- cbind(X, additionMatrix)
  
  yCriterion <- t(t(mainMatrix[, i]))
  
  criterionRegressFitFunction <- lm.fit(X, yCriterion)
  criterionRegress <- rbind(criterionRegress, criterionRegressFitFunction$coefficients)
}
#print(criterionRegress)
###

### calculate constant 'c' for Xkr cost function
criterionRegressConstants <- c()
for (i in 1:((dim(criterionRegress))[1])) {
  f = criterionRegress[i,]
  criterionRegressConstants <- append(criterionRegressConstants, mean(yCriterion) - t(apply(X, 2, mean))%*%t(t(f)))
}
#print(criterionRegressConstants)
###

### count regress for state variables
stateRegress <- c()
for (i in (criterionVariablesNumber + 1):(criterionVariablesNumber + stateVariablesNumber)) {
  X <- c()

  for (j in (criterionVariablesNumber + 1):(criterionVariablesNumber + stateVariablesNumber)) {
    if (j != i)
      X <- cbind(X, mainMatrix[,j])
    else
      X <- cbind(X, rep(0, dim(mainMatrix)[1])) 
  }
  
  for (j in (criterionVariablesNumber + stateVariablesNumber + 1)
       :(criterionVariablesNumber + stateVariablesNumber + controlVariablesNumber)) {
    X <- cbind(X, mainMatrix[, j])
  }
  
  X <- cbind(X, additionMatrix)
  
  yState <- t(t(mainMatrix[, i]))
  
  stateRegressFitFunction <- lm.fit(X, yState)
  stateRegress <- rbind(stateRegress, stateRegressFitFunction$coefficients)
}
#print(stateRegress)

for (i in 1:dim(stateRegress)[1])
  for (j in 1:dim(stateRegress)[2])
    if (is.na(stateRegress[i,j]))
      stateRegress[i,j] <- 0
#print(stateRegress)

A = stateRegress
###

### calculate constants 'Ci' for Xci cost functions
stateRegressConstants <- c()
for (i in 1:((dim(stateRegress))[1])) {
  f = stateRegress[i,]
  stateRegressConstants <- append(stateRegressConstants, mean(yState) - t(apply(X, 2, mean))%*%t(t(f)))
}
#print(stateRegressConstants)
###

### ask about specific step in time
timeLine <- as.numeric(readline(paste0("Enter number of concrete time from ", (dim(mainMatrix)[1]), " all time: ")))
concretePartOfTime <- additionMatrix[timeLine,]
###

### get modified constrains for criterion variable
# criterion constant: 'criterionRegressConstants'
parametrsFromTimeBefore <- criterionRegress[(controlVariablesNumber + 1):length(criterionRegress)]
criterionRegressConstants <- (concretePartOfTime %*% parametrsFromTimeBefore) + criterionRegressConstants
###

### leave criterion variable values in 'criterionRegress' 
criterionRegress <- criterionRegress[,1:controlVariablesNumber]
criterionRegress <- append(criterionRegress, rep(0, stateVariablesNumber))
###

### get modified constrains for state variables
# state constant: 'stateRegressConstants'
lastStateConstantVec <- c()
stateRegressPartOfConstantValues <- stateRegress[,( ((dim(A))[2]) - (dim(additionMatrix)[2]) + 1 ):(dim(A))[2]]
for (i in 1:dim(stateRegressPartOfConstantValues)[1]) {
  lastStateConstantVec <- append(lastStateConstantVec, ((concretePartOfTime %*% stateRegressPartOfConstantValues[i,]) + stateRegressConstants[i]))
}
#print("Print coeficients from state variables regression")
#print(lastStateConstantVec)
###

### leave state and control variable values in 'stateRegress'
stateRegress <- stateRegress[,1:(stateVariablesNumber + controlVariablesNumber)]
#print(stateRegress)
###

### coefficients of criterion and state variables
sol <- linprog(f = criterionRegress, 
               A = stateRegress, 
               b = -lastStateConstantVec, 
               Aeq = c(), 
               beq = c(), 
               lb = stateAndControlVecOfConstrains[,"min"], 
               ub = stateAndControlVecOfConstrains[,"max"])

stateAndControlVecOfCoefficients <- sol$x

print("Show all state and control variables")
print(stateAndControlVecOfCoefficients)
###


### Optimized criterion variable
optimizedCriterion <- as.numeric(stateAndControlVecOfCoefficients %*% criterionRegress + criterionRegressConstants)

print("Show optimized criterial variable as X11: ")
print(optimizedCriterion)
###


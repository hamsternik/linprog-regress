dataFrameFromTwoLists <- function(columnNames, variableNames) {
  bindedDataFrame <- as.data.frame(cbind(labels = columnNames, parameters = variableNames))
}
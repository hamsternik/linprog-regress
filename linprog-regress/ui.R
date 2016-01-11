library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  headerPanel("Linear programming problem using regression models"),
  
  sidebarPanel(
    fileInput('file1', 'Choose Excel file', 
              accept = c('application/vnd.ms-excel',
                         'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                         '.xlsx',
                         '.xls')),
    tags$hr()
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Table", tableOutput("excelTable")),
      tabPanel("Variable Control", 
               fluidRow(
                 column(5, uiOutput("criterion"), uiOutput("state"), uiOutput("control")),
                 column(4, textOutput("timeLineTitle"), uiOutput("timeLine"))
      )),
      tabPanel("Result", uiOutput("optimumResult")),
      tabPanel("Plot", uiOutput("optimumPlot"))
    )
  )
))
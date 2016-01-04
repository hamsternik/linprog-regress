library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  headerPanel("Linear programming problem using regression models"),
  
  sidebarPanel(
    fileInput('file1', 'Choose Excel file', 
              accept = c('application/vnd.ms-excel',
                         'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                         '.xls',
                         '.xlsx')),
    
    tags$hr()
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Table", tableOutput("excelTable"))
    )
  )
))
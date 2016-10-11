library(shiny)
library(DT)

shinyUI(
  fluidPage(
  headerPanel('Air Quality Data'),
  sidebarPanel(
    title = 'Air Quality Data',
    selectInput('xcol', 'X Variable', names(airquality)[-c(5,6)],
                selected=names(airquality)[[1]]),
    selectInput('ycol', 'Y Variable', names(airquality)[-c(5,6)],
                selected=names(airquality)[[2]]),
    # actionButton('select2', 'Select the above variables.'),
    sliderInput("subsample", label = "Size of random samples",
                min = 5, max = 50, value = 10, step = 5),
    actionButton('resetSelection',
                 label = "Click to reset row selection"
    ) # end of action button
  ),
  mainPanel(
    
    fluidRow(
      column(6,  
             h1('select rows'), 
             DT::dataTableOutput('x1')),
      column(6, 
             plotOutput('x2', height = 400))
    ),
    verbatimTextOutput('info')
  )
))
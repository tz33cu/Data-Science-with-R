library(shiny)
library(DT)

shinyUI(
  fluidPage(
  headerPanel('2015 NYC Air Quality Data'),
  sidebarPanel(
    title = 'Air Quality Data',
    sliderInput("nbins", label = "Number of bins for histogram",
                min = 5, max = 50, value = 10, step = 5)
    ),
  mainPanel(
    plotOutput('xplot', height = 400),
    verbatimTextOutput('info')
  )
))
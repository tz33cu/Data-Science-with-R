library(shiny)

shinyUI(
  fluidPage(
    headerPanel('Gradient boosting machine'),
    sidebarPanel(width=3,
      sliderInput("theta", label = "Persp: theta",
                  min = -180, max = 180, value = 0, step = 5),
      sliderInput("phi", label = "Persp: phi",
                  min = -90, max = 90, value = 0, step = 5),
      fluidRow(
        column(5, selectInput('interact', 'Interaction', c(1,2,3),
                              selected=1)),
        column(7, sliderInput("shrinkage", label = "Shrinkage",
                              min = 0.001, max = 1, value = 0.011, 
                              step = 0.05))),
      fluidRow(
        column(6, sliderInput("ntree", label = "Number of trees",
                              min = 50, max = 3000, value = 1000, step = 50)),
        column(6, sliderInput("showstep", label = "Show step k",
                              min = 1, max = 3000, value = 2, step = 1)))
    ),
    mainPanel(width=9,
      fluidRow(column(12, plotOutput('GBMfit', height=400))),
      fluidRow(column(12, plotOutput('errorcurve', height=300)))
    )
  )
)
library(shiny)
library(DT)
shinyApp(
  ui=fluidPage(
    headerPanel('Air Quality Data'),
    sidebarPanel(
      title = 'Air Quality Data',
      selectInput('xcol', 'X Variable', names(airquality)[-c(5,6)],
                  selected=names(airquality)[[1]]),
      selectInput('ycol', 'Y Variable', names(airquality)[-c(5,6)],
                  selected=names(airquality)[[2]]),
      # actionButton('select2', 'Select the above variables.'),
      sliderInput("subsample", label = "Size of random samples",
                  min = 5, max = 50, value = 10, step = 1),
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
  ),
  server<-function(input, output) {
    
    selectedData <- 
      #airquality[,c(input$xcol,input$ycol)]
      reactive({
        airquality[, c(input$xcol, input$ycol)]
      })
    nn <- nrow(airquality)
    
    output$x1 = DT::renderDataTable(airquality[,-c(5,6)], 
                                    options = list(
                                      lengthMenu = list(c(3, 5, 10), c('3', '5', '10')),
                                      pageLength = 5
                                    ),
                                    server = FALSE,
                                    selection = list(target = 'row+column'))
    
    proxy = dataTableProxy('x1')
    
    observeEvent(input$resetSelection, {
      proxy %>% selectRows(sample(1:nn, input$subsample, replace=F))
    })
    
    
    # highlight selected rows in the scatterplot
    output$x2 = renderPlot(height = 400, {
      par(mar = c(4, 4, 1, .1))
      plot(airquality[, c(input$xcol, input$ycol)])
      s = input$x1_rows_selected
      if (length(s)) {
        points(airquality[s, c(input$xcol, input$ycol), drop = FALSE], 
               pch = 19, cex = 2)
        abline(lsfit(airquality[s,input$xcol], 
                     airquality[s,input$ycol])$coef, col=2)
      }
    })
    
    output$info = renderPrint({
      s = input$x1_rows_selected
      cor.sel=NA
      if(length(s)) cor.sel=cor(airquality[s,input$xcol], 
                                airquality[s,input$ycol],
                                use="pairwise.complete.obs")
      list(xcol=input$xcol, ycol=input$ycol, 
           cor.all=cor(airquality[,input$xcol], 
                       airquality[,input$ycol],
                       use="pairwise.complete.obs"),
           cor.sel=cor.sel)
    })
    
  }
)
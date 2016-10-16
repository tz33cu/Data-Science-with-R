library(shiny)
library(DT)
shinyServer(function(input, output) {
  
  #selectedData <- 
    #airquality[,c(input$xcol,input$ycol)]
  #  reactive({
  #    airquality[, c(input$xcol, input$ycol)]
  #  })
  nn <- nrow(airquality)
  
  output$x1 = DT::renderDataTable(airquality[,-c(5,6)], 
                                  options = list(
                                    lengthMenu = list(c(3, 5, 10), c('3', '5', '10')),
                                    pageLength = 5
                                  ),
                                  server = FALSE,
                                  selection = list(target = 'row'))
  
  proxy = dataTableProxy('x1')
  
  observeEvent(input$resetSelection, {
    proxy %>% selectRows(sample(1:nn, input$subsample, replace=F))
  })
  
  
  # highlight selected rows in the scatterplot
  output$x2 = renderPlot(height = 400, {
    par(mar = c(4, 4, 2, .1), font.main=1)
    options(digits = 2)
    cor.all=cor(airquality[,input$xcol], 
                airquality[,input$ycol],
                use="pairwise.complete.obs")
    plot(airquality[, c(input$xcol, input$ycol)])
    s = input$x1_rows_selected
    abline(lsfit(airquality[,input$xcol], 
                 airquality[,input$ycol])$coef, col=1, lty=2)
    cor.sel=NA
    if (length(s)>=2) {
      points(airquality[s, c(input$xcol, input$ycol), drop = FALSE], 
             pch = 21, bg=2, cex = 1.5, col=4)
      abline(lsfit(airquality[s,input$xcol], 
                   airquality[s,input$ycol])$coef, col=2)
      cor.sel=cor(airquality[s,input$xcol], 
                  airquality[s,input$ycol],
                  use="pairwise.complete.obs")
    }
    title(main=paste("correlation (all)=", format(cor.all),
                    ", correlation (sel)=", format(cor.sel)))
  })
  
  output$info = renderPrint({
    s = input$x1_rows_selected
    cor.sel=NA
    if(length(s)) cor.sel=cor(airquality[s,input$xcol], 
                              airquality[s,input$ycol],
                              use="pairwise.complete.obs")
    list(selected=s, 
         xcol=input$xcol, ycol=input$ycol, 
         cor.all=cor(airquality[,input$xcol], 
                     airquality[,input$ycol],
                     use="pairwise.complete.obs"),
         cor.sel=cor.sel)
  })
  
}
)
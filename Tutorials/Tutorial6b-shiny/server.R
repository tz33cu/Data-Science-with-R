library(shiny)
library(DT)
shinyServer(function(input, output) {
  
  ozonedata=read.csv(file="data/ozone.csv")
  names(ozonedata)[4]="ozone"
  pm25data=read.csv(file="data/pm25.csv")
  names(pm25data)[4]="pm25"
  
  output$xplot = renderPlot(height=400, {
    par(mfrow=c(2,1))
    hist(ozonedata$ozone,
         breaks=seq(min(ozonedata$ozone)*0.99, 
                    max(ozonedata$ozone)*1.01,
                    len=input$nbins+1))
    hist(pm25data$pm25,
         breaks=seq(min(pm25data$pm25)*0.99, 
                    max(pm25data$pm25)*1.01,
                    len=input$nbins+1))}
  )
  
  
  output$info = renderPrint({
    list(mean.ozone=mean(ozonedata$ozone),
         mean.pm25=mean(pm25data$pm25))
  })
  
}
)
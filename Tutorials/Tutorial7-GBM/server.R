library(shiny)
library(graphics)
library(gbm)

shinyServer(function(input, output) {
  
  load("spam.RData")
  spamdata.train$logChar4=log(spamdata.train$char4+0.01)
  spamdata.train$logcapsum=log(spamdata.train$cap.run.sum+0.01)
  spamdata=spamdata.train
  
  output$GBMfit=renderPlot(height=400, {
    # fitting the model
    gbm.model1=gbm(spam~logChar4+logcapsum, data=spamdata, 
                   train.fraction=0.8, 
                   n.trees=input$ntree, 
                   shrinkage = input$shrinkage,
                   interaction.depth=input$interact)
    
    # visualize the fitted model
    x1=seq(min(spamdata$logChar4, na.rm=T), 
           max(spamdata$logChar4, na.rm=T), 0.25)
    x2=seq(min(spamdata$logcapsum, na.rm=T), 
           max(spamdata$logcapsum, na.rm=T), 0.25)
    zz.input=expand.grid(x1, x2)
    colnames(zz.input)=c("logChar4", "logcapsum")
    zz.output.1=predict.gbm(gbm.model1, zz.input, 
                            n.trees=input$showstep, single.tree=T)
    zz.output.all=predict.gbm(gbm.model1, zz.input, 
                              n.trees=input$showstep, single.tree=F)
    zz.mat1=matrix(zz.output.1, length(x1), length(x2), byrow=F)
    zz.mat2=matrix(zz.output.all, length(x1), length(x2), byrow=F)
    par(mfrow=c(1,2))
    par(mar = c(4, 4, 2, .1), font.main=1)
    persp(x1, x2, zz.mat1, 
          main=paste("one tree at step", input$showstep), 
          col="lightblue",
          phi=input$phi, theta=input$theta)
    persp(x1, x2, zz.mat2,
          main=paste("all trees at step", input$showstep), 
          col="lightsalmon",
          phi=input$phi, theta=input$theta)
  })
  
  output$errorcurve=renderPlot(height=200, {
    gbm.model1=gbm(spam~logChar4+logcapsum, data=spamdata, 
                   train.fraction=0.8, 
                   n.trees=input$ntree, 
                   shrinkage = input$shrinkage,
                   interaction.depth=input$interact)
    par(mar = c(4, 4, 2, .1), font.main=1)
    
    plot(c(1, input$ntree), c(0,max(gbm.model1$valid.error)), 
         type="n", xlab="trees", ylab="error rate",
         main="Loss fucntions")
    lines(gbm.model1$train.error, col=1, lty=2, lwd=1.5)
    lines(gbm.model1$valid.error, col=1, lty=1, lwd=2)
    legend(input$ntree*0.05, max(gbm.model1$valid.error)*0.4, 
           c("training loss", "validation loss"),
           lty=2:1)
  })
}
)
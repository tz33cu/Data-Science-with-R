library(shiny)
library(graphics)
library(gbm)
library(KernSmooth)

shinyServer(function(input, output) {
  
  load("data/spam.RData")
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
           max(spamdata$logChar4, na.rm=T), 0.5)
    x2=seq(min(spamdata$logcapsum, na.rm=T), 
           max(spamdata$logcapsum, na.rm=T), 0.5)
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
          main=paste("The fitted tree at step", input$showstep), 
          col="lightblue",
          xlab="Log char4",
          ylab="Log cap sum",
          zlab="estimated probability of spam",
          phi=input$phi, theta=input$theta)
    persp(x1, x2, zz.mat2,
          main=paste("Model incld. all trees up to step", input$showstep), 
          xlab="Log char4",
          ylab="Log cap sum",
          zlab="estimated probability of spam",
          col="lightpink",
          phi=input$phi, theta=input$theta)
  })
  
  output$errorcurve=renderPlot(height=300, {
    gbm.model1=gbm(spam~logChar4+logcapsum, data=spamdata, 
                   train.fraction=0.8, 
                   n.trees=input$ntree, 
                   shrinkage = input$shrinkage,
                   interaction.depth=input$interact)
    par(mfrow=c(1,2),
        mar = c(4, 4, 2, .1), font.main=1)
     
    # panel 1: observed data
    
    smoothScatter(spamdata$logChar4, 
                  spamdata$logcapsum,
                  xlab="Log char4", ylab="Log cap sum",
                  main="Training data",
                  colramp = colorRampPalette(c("white", 
                                               gray.colors(10, start = 0.8, end = 0.2))))
    points(spamdata$logChar4[spamdata$spam==1], 
                  spamdata$logcapsum[spamdata$spam==1],
                  col=rgb(1,0,0,alpha=0.4))
    points(spamdata$logChar4[spamdata$spam==0], 
                  spamdata$logcapsum[spamdata$spam==0],
                  col=rgb(0,0,1,alpha=0.8))
    legend(1.5, 8.5, c("spam", "not spam"), pch=1, col=c(2,4))
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
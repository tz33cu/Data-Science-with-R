library(gbm)

spamdata.train$logChar4=log(spamdata.train$char4+0.01)
spamdata.train$logcapsum=log(spamdata.train$cap.run.sum+0.01)

gbm.model1=gbm(spam~logChar4+logcapsum, data=spamdata.train, train.fraction=0.8, n.trees=3000, interaction.depth=3)
gbm.model2=gbm(spam~logChar4+logcapsum, data=spamdata.train, 
shrinkage=0.01, train.fraction=0.8, n.trees=3000, interaction.depth=3)
gbm.model3=gbm(spam~logChar4+logcapsum, data=spamdata.train, shrinkage=0.1, train.fraction=0.8, n.trees=3000, interaction.depth=3)

plot(c(1, 3000), c(0,1.5), type="n", xlab="trees", ylab="error rate")
lines(gbm.model1$train.error, col=1, lty=2, lwd=1.5)
lines(gbm.model2$train.error, col=2, lty=2, lwd=1.5)
lines(gbm.model3$train.error, col=3, lty=2, lwd=1.5)
lines(gbm.model1$valid.error, col=1, lty=1, lwd=2)
lines(gbm.model2$valid.error, col=2, lty=1, lwd=2)
lines(gbm.model3$valid.error, col=3, lty=1, lwd=2)


x1=seq(min(spamdata.train$logChar4, na.rm=T), max(spamdata.train$logChar4, na.rm=T), 0.25)
x2=seq(min(spamdata.train$logcapsum, na.rm=T), max(spamdata.train$logcapsum, na.rm=T), 0.25)
zz.input=expand.grid(x1, x2)
colnames(zz.input)=c("logChar4", "logcapsum")
pdf("results.pdf")
plot(spamdata.train$logChar4, spamdata.train$logcapsum, col=spamdata.train$spam+1)
#par(mfrow=c(1,2))
for(i in c(1, 2, 3, 10, 50, 100, 200, 201, 500, 501, 502, 700, 1000, 2000, 3000)){

zz.output.1=predict.gbm(gbm.model1, zz.input, n.trees=i, single.tree=T)
zz.output.all=predict.gbm(gbm.model1, zz.input, n.trees=i, single.tree=F)
zz.mat1=matrix(zz.output.1, length(x1), length(x2), byrow=F)
zz.mat2=matrix(zz.output.all, length(x1), length(x2), byrow=F)
persp(x1, x2, zz.mat1, main=paste("one tree at step", i), phi=40, theta=-20)
persp(x1, x2, zz.mat2, main=paste("all trees at step", i), phi=40, theta=-20)
}


persp(x1, x2, zz.mat2, main=paste("all trees at step", i), col="lightblue")

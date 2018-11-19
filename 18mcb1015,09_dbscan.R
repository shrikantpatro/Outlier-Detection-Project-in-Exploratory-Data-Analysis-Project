library("dbscan")
library(caret)
library(utils)
data=clean1[,3:169]
#data
classi<-clean1[,169]
#classi
#par(mfrow=c(1,2))
#View(data)
#c1=cor(data)
#c1
meanx8=mean(data$X8)
meanx23=mean(data$X23)
sdx8=sd(data$X8)
sdx23=sd(data$X23)
#x <- rnorm(475,meanx8,sdx8)
#y <- rnorm(475,meanx23,sdx23)
#plot(x,y)
#findCorrelation(c1,cutoff = 0.99,names = TRUE)
#c2=cor(data1)
#data_bind <- cbind(data$X8,data$X23)
dat <- data.frame(data$X8,data$X23)






par(mfcol=c(2,2))
data_std <- apply(dat,2,function(x)x/(max(x)-min(x)))

kNNdistplot(data_std, k =  10)
abline(h=0.1,lwd=2)
dbcl1_1<-dbscan(data_std,eps=0.1,minPts=10)
plot(as.data.frame(dat),pch=18,col=ifelse(dbcl1_1$cluster==0,2,1))

### abalone ###
kNNdistplot(data_std, k =  20)
abline(h=0.05,lwd=2)
dbcl1_2 <- dbscan(data_std,eps=0.05,minPts=10)
plot(as.data.frame(dat),pch=18,col=ifelse(dbcl1_2$cluster==0,2,1))

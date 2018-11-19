library(caret)
library(utils)
data=clean1[,3:169]
#data
classi<-clean1[,169]
#classi
par(mfrow=c(1,2))
#View(data)
c1=cor(data)
c1
meanx8=mean(data$X8)
meanx23=mean(data$X23)
sdx8=sd(data$X8)
sdx23=sd(data$X23)
x <- rnorm(475,meanx8,sdx8)
y <- rnorm(475,meanx23,sdx23)
plot(x,y)
findCorrelation(c1,cutoff = 0.99,names = TRUE)
dat <- data.frame(data$X8,data$X23)
#typeof(data_bind)
#typeof(dat)
par(mfcol= c(3,1))
dat_std <- apply(dat,2,function(x)x/(max(x)-min(x)))
cl1_2 <- kmeans(dat_std,200)
ind <- as.vector(which(table(cl1_2$cluster)< 5))
out <- ifelse(cl1_2$cluster %in% ind,1,2)
plot(as.data.frame(dat),pch=18,col=out, main="B1) MUSK:=200 , minpoints=5")

cl3_2 <- kmeans(dat_std,300)
ind <- as.vector(which(table(cl3_2$cluster)< 5))
out <- ifelse(cl3_2$cluster %in% ind,1,2)
plot(as.data.frame(dat),pch=18,col=out, main="B2) MUSK:=300 , minpoints=5")

cl2_2 <- kmeans(dat_std,200)
ind <- as.vector(which(table(cl2_2$cluster)< 10))
out <- ifelse(cl2_2$cluster %in% ind,1,2)
plot(as.data.frame(dat),pch=18,col=out, main="B3) MUSK:=200 , minpoints=10")


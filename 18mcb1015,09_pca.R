library(caret)
library(utils)
data=clean1[,3:169]
data
classi<-clean1[,169]
classi
par(mfrow=c(1,2))
View(data)
c1=cor(data)
c1
meanx8=mean(data$X8)
meanx23=mean(data$X23)
sdx8=sd(data$X8)
sdx23=sd(data$X23)
x <- rnorm(475,meanx8,sdx8)
y <- rnorm(475,meanx23,sdx23)
plot(x,y)
for (i in 1:nrow(c1)){
  correlations <-  which((c1[i,] > 0.98) & (c1[i,] != 1))
  
  if(length(correlations)> 0){
    print(colnames(c1)[i])
    print(correlations)
  }
}
data_bind <- cbind(data$X8,data$X23)
data_bind1 <- cbind(data$X40,data$X76,data$X6)

bp <- function(X,fac){
  med <-sapply(X,median) 
  q25 <-sapply(X,function(x)quantile(x,prob=0.25)) 
  q75 <-sapply(X,function(x)quantile(x,prob=0.75)) 
  erg <- t(apply(X, 1, function(x) abs(med-x)-fac*(q75-q25)))
  return(as.vector(which(rowSums(erg>0)>0)))}

bp_app <-function(X,a){
  outliers <- rep(1,length(X[,1]))
  outliers[bp(X,a)] <- 2
  outliers<- as.factor(outliers)
  levels(outliers) <- c("No Outlier","Outlier")
  print(table(outliers))
  if(table(outliers)[2] > 0)plot(X,col=outliers,pch=18)
  return(outliers)}


dat_std <- apply(dat,2,function(x)x/(max(x)-min(x)))

par(mfcol=c(2,2))
print("Outlier detection using PCA with Factor 1.5")                 
PCA1.5_1 <-bp_app(as.data.frame(princomp(data_bind)$scores)[,1:2],1.5)
PCA1.5_1
plot(data_bind,col=PCA1.5_1,pch=18)
print("Outlier detection using PCA with Factor 3")                 
PCA3_1 <-bp_app(as.data.frame(princomp(data_bind)$scores)[,1:2],3)
plot(data_bind,col=PCA3_1,pch=18)


print("Outlier detection using PCA with Factor 1.5")                 
PCA1.5_2 <-bp_app(as.data.frame(princomp(data_bind1)$scores)[,1:2],1.5)
PCA1.5_2
plot(data_bind1,col=PCA1.5_2,pch=18)
print("Outlier detection using PCA with Factor 3")                 
PCA3_2 <-bp_app(as.data.frame(princomp(data_bind1)$scores)[,1:2],3)
plot(data_bind1,col=PCA3_2,pch=18)

library(caret)
library(utils)
data=clean1[,3:169]
par(mfrow=c(1,2))
View(data)
c1=cor(data)
c1
findCorrelation(c1,cutoff = 0.99,names = TRUE)
c2=cor(data1)
data_bind <- cbind(data$X8,data$X23)
dat <- data.frame(data$X8,data$X23)

euclid <- function(X,fac){
  med <-sapply(X,median) 
  erg <- t(apply(X, 1, function(x) (med-x)^2))
  dist <- sqrt(rowSums(erg))
  #   print(plot(dist))
  return(dist > fac*median(dist))}

euclid_app <-function(X,a){
  outliers <- rep(1,length(X[,1]))
  outliers[euclid(X,a)] <- 2
  outliers<- as.factor(outliers)
  levels(outliers) <- c("No Outlier","Outlier")
  print(table(outliers))
  if(table(outliers)[2] > 0)plot(X,col=outliers,pch=18)
  #return(outliers)
}
### musk dataset ###
par(mfrow=c(1,2))
dat_std <- apply(dat,2,function(x)x/(max(x)-min(x)))
print("Euclid methode on musk data")   
euclid_app(as.data.frame(dat_std),5)
euclid_app(as.data.frame(dat_std),3)

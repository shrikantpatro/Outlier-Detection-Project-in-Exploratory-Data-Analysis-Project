library(caret)
library(utils)
data=clean1[,3:169]
data
c1=cor(data)
c1
for (i in 1:nrow(c1)){
  correlations <-  which((c1[i,] > 0.98) & (c1[i,] != 1))
  
  if(length(correlations)> 0){
    print(colnames(c1)[i])
    print(correlations)
  }
}

par(mfrow=c(4,2))
data_bind23<-cbind(data$X8,data$X11.1,data$X6)
mod23 <- lm(data$X23 ~ data_bind23, data=data)
cooksd23 <- cooks.distance(mod23)
plot(cooksd23, pch="*", cex=1.2, main="")
abline(h = 4*mean(cooksd23, na.rm=T), col="red")
text(x=1:length(cooksd23)+1, y=cooksd23, labels=ifelse(cooksd23>4*mean(cooksd23, na.rm=T),names(cooksd23),""), col="red")
influential <- as.numeric(names(cooksd23)[(cooksd23 > 4*mean(cooksd23, na.rm=T))])  # influential row numbers

data_bind.28<-cbind(data$X.32)
mod.28 <- lm(data$X.28 ~ data_bind.28, data=data)
cooksd.28 <- cooks.distance(mod.28)
plot(cooksd.28, pch="*", cex=1.2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd.28, na.rm=T), col="red")  
text(x=1:length(cooksd.28)+1, y=cooksd.28, labels=ifelse(cooksd.28>4*mean(cooksd.28, na.rm=T),names(cooksd.28),""), col="red")
influential <- as.numeric(names(cooksd.28)[(cooksd.28 > 4*mean(cooksd.28, na.rm=T))])  # influential row numbers

data_bind63<-cbind(data$X80,data$X51)
mod63 <- lm(data$X63 ~ data_bind63, data=data)
cooksd63 <- cooks.distance(mod63)
plot(cooksd63, pch="*", cex=1.2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd63, na.rm=T), col="red")
text(x=1:length(cooksd63)+1, y=cooksd63, labels=ifelse(cooksd63>4*mean(cooksd63, na.rm=T),names(cooksd63),""), col="red")
influential <- as.numeric(names(cooksd63)[(cooksd63 > 4*mean(cooksd63, na.rm=T))])  # influential row numbers

data_bind.177<-cbind(data$X.146,data$X.206)
mod.177 <- lm(data$X.177 ~ data_bind.177, data=data)
cooksd.177 <- cooks.distance(mod.177)
plot(cooksd.177, pch="*", cex=1.2, main="")
abline(h = 4*mean(cooksd63, na.rm=T), col="red") 
text(x=1:length(cooksd.177)+1, y=cooksd.177, labels=ifelse(cooksd.177>4*mean(cooksd.177, na.rm=T),names(cooksd63),""), col="red")
influential <- as.numeric(names(cooksd.177)[(cooksd.177 > 4*mean(cooksd.177, na.rm=T))])

data_bind32<-cbind(data$X.28)
mod32 <- lm(data$X32 ~ data_bind32, data=data)
cooksd32 <- cooks.distance(mod32)
plot(cooksd32, pch="*", cex=1.2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd32, na.rm=T), col="red") 
text(x=1:length(cooksd32)+1, y=cooksd32, labels=ifelse(cooksd32>4*mean(cooksd32, na.rm=T),names(cooksd63),""), col="red")
influential <- as.numeric(names(cooksd32)[(cooksd32 > 4*mean(cooksd32, na.rm=T))])

data_bind40<-cbind(data$X76,data$X6)
mod40 <- lm(data$X40 ~ data_bind40, data=data)
cooksd40 <- cooks.distance(mod40)
plot(cooksd40, pch="*", cex=1.2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd40, na.rm=T), col="red")
text(x=1:length(cooksd40)+1, y=cooksd40, labels=ifelse(cooksd40>4*mean(cooksd40, na.rm=T),names(cooksd40),""), col="red")
influential <- as.numeric(names(cooksd40)[(cooksd40 > 4*mean(cooksd40, na.rm=T))])

data_bind6<-cbind(data$X23,data$X40)
mod6 <- lm(data$X6 ~ data_bind6, data=data)
cooksd6 <- cooks.distance(mod6)
plot(cooksd6, pch="*", cex=1.2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd6, na.rm=T), col="red")
text(x=1:length(cooksd6)+1, y=cooksd6, labels=ifelse(cooksd6>4*mean(cooksd6, na.rm=T),names(cooksd6),""), col="red")
influential <- as.numeric(names(cooksd6)[(cooksd6 > 4*mean(cooksd6, na.rm=T))])





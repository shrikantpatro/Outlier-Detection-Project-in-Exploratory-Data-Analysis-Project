library(caret)
library(utils)
library(car) 
data2=clean1[,3:169]
classi<-clean1[,169]
data8=data$X8
data23=data$X23
data40=data$X40
data76=cbind(data$X76,data$X6)

scatterplot(data8 ~ data23 | classi , data=data2,main="",xlab="",ylab="",col = "red")
scatterplotMatrix(~ data8 + data23 + data40 | classi,data=data2, main="",legend.pos="bottomright")

scatterplot(data40 ~ data$X76 | classi , data=data2,main="",xlab="",ylab="",col = "red")
scatterplotMatrix(~ data40 + data$X76 + data$X6 | classi,data=data2, main="",legend.pos="bottomright")

networkMeasuresAverage <- function(mygraphList,nCols) {
  ave<-as.data.frame(t(colMeans(data.matrix(as.data.frame(mygraphList[,1:nCols])),na.rm=TRUE)))
  #stddev<-colSds(data.matrix(as.data.frame(mygraphList[,1:nCols])),na.rm=TRUE)
  stddev<-sapply(1:10,function(x) sd(as.numeric(mygraphList[,x]),na.rm=TRUE))
  dataOut<-rbind(ave,stddev)
  rownames(dataOut)<-c("Mean","StdDev")
  networkMeasuresAverage<-as.data.frame(dataOut) 
}
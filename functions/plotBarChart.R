plotBarChart <- function(average,engaged,instrumental,title,ylimits) {
  means<-c(average[1],engaged[1],instrumental[1])
  mp <- barplot(means, axes=FALSE, axisnames=FALSE, ylim=c(ylimits[1], ylimits[2]),col=c("red2", "blue","cyan"), main=title, xlab="Group", ylab="Homophilly")
  axis(1, labels=c("Ave", "Engaged","Instrumental"), at = mp)
  interval<-(ylimits[2]-ylimits[1])/10
  axis(2, at=seq(ylimits[1] , ylimits[2], by=interval))
  box()
  stDevs <- matrix(c(average[2],engaged[2],instrumental[2]),3)
  segments(mp, means - stDevs, mp, means + stDevs, lwd=2)
  segments(mp - 0.1, means - stDevs, mp + 0.1, means - stDevs, lwd=2)
  segments(mp - 0.1, means + stDevs, mp + 0.1, means + stDevs, lwd=2)
}
hphyCliques <- function(mygraph, characteristic,cliqueSize) {
  a<-cliques(mygraph,cliqueSize)
  if(length(a)>0){
    print("length OK")
      hphy<-lapply(1:length(a),function(x){tempFun(a,x,mygraph)})
  }
  if(length(a)==0){hphy<-'NaN'}
  return(hphy)
  


}

#Append ! to string, otherwise increment
tempFun <- function(a,x,mygraph){
  clique1<-a[[x]]
  g1 <- induced.subgraph(graph=mygraph,vids=clique1)
  h<- homophilly(g1,'engagement')
  tt<-0
  if(is.nan(h)){
    tt<-length(unique(V(g1)$engagement))
  }
  if(tt==1){h<-1}
  return(h)
}


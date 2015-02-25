networkMeasuresPersonal <- function(mygraphList,length) {
  h<-list(NULL)
  hEngage<-list(NULL)
  triadicClos<-list(NULL)
  density<-list(NULL)
  cohesion<-list(NULL)
  size<-list(NULL)
  nComponents<-list(NULL)
  nCliques<-list(NULL)
  averageTieStrength<-list(NULL)
  homophillyCliqueEngagement<-list(NULL)
  engagement<-list(NULL)
  egoID<-list(NULL)
  
  for(i in 1:length){  
    g2<-mygraphList[[i]]
    if(length(V(g2))!=0){
      characteristic<-"ethnicity"
      if(length(V(g2))!=0){
        #mixing matrix
        # now calculate the assortativity coefficient
        ##assortcoeff(m)
        # now calculate the assortativity coefficient which is hopefully the homophily
        nHomophilous<-length(which(V(g2)$engagement==V(g2)[1]$engagement))
        hEngage[i]<- nHomophilous/length(V(g2))
        nHomophilous<-length(which(V(g2)$ethnicity==V(g2)[1]$ethnicity))
        h[i]<- nHomophilous/length(V(g2))
        triadicClos[i]<-triadicClosure(g2)
        density[i]<-graph.density(g2)
        cohesion[i]<-graph.cohesion(g2)
        size[i]<-length(V(g2))
        nComponents[i]<-no.clusters(g2)
        nCliques[i]<-clique.number(g2)
        averageTieStrength[i]<-mean(E(g2)$Strength)
        homophillyCliqueEngagement[i]<-mean(as.numeric(hphyCliques(g2,'engagement',3)))
        engagement[i]<-V(g2)$engagement[1]
        egoID[i]<-V(g2)$egoid[1]
      }
    }
  }
  print("here")
  ##bundles into a dataframe- these are averages for the entire network including Ego
  dataOut<-data.frame(cbind(h,hEngage,triadicClos,size,density,cohesion,nCliques,averageTieStrength,homophillyCliqueEngagement,nComponents,engagement,egoID))
  
  print("here")
  # dataOut<-rbind(dataOut,c(h,triadicClos,size,density,cohesion,nCliques,averageTieStrength,homophillyCliqueEngagement))
  colnames(dataOut)<-c("HomophillyEthnicity","HomophillyEngagement","Triadic","Size","Density","Cohesion","nCliques","AverageTieStrength","HomophillyCliqueEngagement","nComponents","Engagement","egoID")
  
  
  dataOut <- sapply(dataOut, function(x) ifelse(x == "NULL", NA, x)) 
  networkMeasures<-as.data.frame(dataOut)
  
  
}
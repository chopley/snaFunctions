#Load libraries, set wd
rm(list=ls())
setwd("/home/charles/ownCloud/charles/work/2014/eliFinalPaper/snaData/charlesDataReduction/")  
library(igraph)     #social network analysis (for network plots)
library(intergraph) #for creating and changing igraph network objects
library(plyr)
source('./functions/assortcoeff.R')
source('./functions/mixmat.R')
source('./functions/hphyCJC.R')
#Load functions and data
source("./functions/sna_functions.R")      #load sna functions
source('./functions//hphyCliques.R')
require('fBasics')
source('./functions//networkMeasures.R')
source('./functions/networkMeasuresPersonal.R')
source('./functions//networkMeasuresAveraging.R')
source('./functions//plotBarChart.R')
library(xlsx)
##-size
#-number of components
#-% of components that are homo on engagement
#-number of cliques
#-number of cliques that are 100% homo on engagement; and that are 100% homo on ethnicity
#-Newman's homo for ethnicity and engagement
#-cohesion
#-density
#-triadic closure
#-average tie strength
###

##Egocentric Network Only (ie ties directly with ego only):
#-degree
#-average tie strength
#-% homo on engagement
#-% homo on ethnicity
#-% ties =1
#-% ties=2
#-%ties=3
#-% ties with direct prior connection
#-% ties with indirect prior connection
##




#Load datasets
egos   <- read.csv2(file="/home/charles/ownCloud/charles/work/2014/eliFinalPaper/snaData/egos5.csv", header=TRUE,sep=',') 
alters <- read.csv2(file="/home/charles/ownCloud/charles/work/2014/eliFinalPaper/snaData/alters5.csv", header=TRUE,sep=',') 
ties   <- read.csv2(file="/home/charles/ownCloud/charles/work/2014/eliFinalPaper/snaData/adjacency5.csv", header=TRUE,sep=',')

#ties contains all the data that is required to construct the network.
#alters contains all the data that is required to compute the homophily.



##how many ego networks do we have in all the data?
nEgoNetworks<-length(unique(alters$egoid))
egoNetworkVector<-unique(alters$egoid)

ties$friends[ties$egoid==1]

##inititiate the list structures to store the resulting graphs
iGraphList_t0<-list(NULL)
iGraphList_t2<-list(NULL)
iGraphList_egoID<-list(NULL)
iGraphList_egoNetwork_egoRemovedID<-list(NULL)
tt<-list(NULL)
##create the network graphs
for(nEgo in egoNetworkVector) 
{
  print(nEgo)
  ##lets get the alter id network that describes all the people in the EgoID network at t2
  tt2<-ties[ties$egoid==nEgo,]
  nodest2<-unique(c(tt2$n1,tt2$n2)) ##lets get the node names in the network structure at t2
  #this creates the vertices with links as specified in the ties data file
  temp<-rbind(tt2$n1,tt2$n2)
  #and now we create a graph of the object at t2
  g2<-graph(c(temp),length(nodest2),directed=F)
  ##assign tie strength based on Elis values
  g2<-set.edge.attribute(g2,"Strength",E(g2),tt2$tieStrength) 
  
  
  ##lets colour the edges by the type of tie
  #  E(g2)$width[tt2$friends==1]=1 ##associate
  #  E(g2)$width[tt2$friends==2]=3 ##weak friend
  #  E(g2)$width[tt2$friends==3]=9 ##mentor
  #  E(g2)$width[tt2$friends==4]=6 ##strong friend
  #  E(g2)$color<-"black"  
  
  ##next step is to populate the networks with the features specified in the alters data set
  ##first we extract the features using the egoid feature value.
  ttAlters<-alters[alters$egoid==nEgo,]
  #this vector will contain the alterID values in the network. These will not necessarily be in consecutive order
  alteridVector<-ttAlters$alterid 
  #now we can assign the various attributes
  
  listofVals<-as.list(colnames(ttAlters)[1:14])
  #populate all the characteristics in the network. By the end we will have the indexing of the two graph vertices matching the
  # alter ID values. Ego should always appear as virtex number 1.
  for (i in 1:length(listofVals)){
    ##use this to get whether the value requires as.character or not
    ascharacter<-eval(parse(text=paste("is.numeric(ttAlters$",listofVals[i],"[1])",sep="")))
    eval(parse(text=paste("V(g2)[alteridVector]$",listofVals[i],"<-as.character(ttAlters$",listofVals[i],")",sep="")))
    
  }
  
  V(g2)$color[V(g2)$ethnicity=="Black"]="deepskyblue2"
  V(g2)$color[V(g2)$ethnicity=="Asians"]="mediumorchid4"
  V(g2)$color[V(g2)$ethnicity=="White"]="white"
  V(g2)$color[V(g2)$ethnicity=="Traveller"]="magenta"
  V(g2)$color[V(g2)$ethnicity=="Mixed"]="orange"
  
  ##remove the edges with strength==0
  g2<-delete.edges(g2,E(g2)[E(g2)$Strength==0])
  ##here we define how we will see the difference between the different edges
  ##these one define the widths using width of line
 # E(g2)$width[E(g2)$Strength==1]=1 ##associate
#  E(g2)$width[E(g2)$Strength==2]=2 ##weak friend
#  E(g2)$width[E(g2)$Strength==3]=3 ##mentor
#  E(g2)$width[E(g2)$Strength==4]=4 ##strong friend
#  E(g2)$color<-"black"
 ## and these use colouts 
  E(g2)$color[E(g2)$Strength==1]="blue" ##associate
  E(g2)$color[E(g2)$Strength==2]="red" ##weak friend
  E(g2)$color[E(g2)$Strength==3]="green" ##mentor
  E(g2)$color[E(g2)$Strength==4]="black" ##strong friend
 
## and these line types
#E(g2)$lty[E(g2)$Strength==1]=1 #"solid" ##associate
#E(g2)$lty[E(g2)$Strength==2]=2 #"dashed" ##weak friend
#E(g2)$lty[E(g2)$Strength==3]=3 #"dotted" ##mentor
#E(g2)$lty[E(g2)$Strength==4]=4 #"dotdash" ##strong friend
#E(g2)$color<-"black"

  if(length(V(g2))!=0){
    ##We assign labels based on status of engagement
    V(g2)$engagement[is.na(V(g2)$engagement)]<-"Unknown" ##first remove annoying NAs and replace with zeros
    idLabels<-lapply(1:length(V(g2)),function(x){if(V(g2)[x]$engagement=="engaged"){tt[x]=paste("E",x)}
                                                 else if(V(g2)[x]$engagement=="instrumental"){tt[x]=paste("D",x)}
                                                 else{tt[x]=paste("Unknown",x)}})
    idLabels[1]<-paste("EGO",idLabels[1])
    
    
    
    V(g2)$label<-idLabels
    ##and we assign different shapes based on status of engagement
    V(g2)$shape[V(g2)$engagement=="engaged"]="circle"
    V(g2)$shape[V(g2)$engagement=="instrumental"]="square"
    V(g2)$shape[V(g2)$engagement==""]="square"
    V(g2)$shape[V(g2)$engagement==""]="square"
    V(g2)$shape[is.na(V(g2)$shape)]<-"square"
  }
  
  
  
  if(iGraphList_t2=="NULL"){
    iGraphList_t2<-list(g2)
  }else{    
    iGraphList_t2<-append(iGraphList_t2,list(g2))    
  }
  
  
}


##Next we can remove the ego network from the full network
#first we copy the full network to a new name
iGraphList_egoRemoved<-iGraphList_t2
for(i in 1:26){
  #   ##create the egoIDs
  print(i)
  g2<-iGraphList_egoRemoved[[i]]
  if(length(V(g2))!=0){
    #then remove vertex ID==1 which is the ego 
    g2<-delete.vertices(g2,1)
    } 
    iGraphList_egoRemoved[[i]]<-g2
}

## next we do egoNetworkss
iGraphList_egoID<-iGraphList_t2
for(i in 1:26){
  #   ##create the egoIDs
  print(i)
  g2<-iGraphList_egoID[[i]]
  if(length(V(g2))!=0){
    ##get the neighbours of vertex ID==1
    tt<-as.list(neighborhood(g2,1,1))
    ##create the subgraph that only includes the neighbours of Ego
    g2 <- induced.subgraph(graph=g2,vids=as.numeric(unlist(tt)))
  } 
  iGraphList_egoID[[i]]<-g2
}

## next we do egoNetworkss with ego removed
iGraphList_egoID2<-iGraphList_t2
for(i in 1:26){
  #   ##create the egoIDs
  print(i)
  g2<-iGraphList_egoID2[[i]]
  if(length(V(g2))!=0){
    ##get the neighbours of vertex ID==1
    tt<-as.list(neighborhood(g2,1,1))
    ##create the subgraph that only includes the neighbours of Ego
    g2 <- induced.subgraph(graph=g2,vids=as.numeric(unlist(tt)))
    g2<-delete.vertices(g2,1)
  } 
  iGraphList_egoNetwork_egoRemovedID[[i]]<-g2
}



## next we do personal Networks
##we copy the Ego ID and then get ready to remove the ones that non Ego ties
iGraphList_personalNets<-iGraphList_egoID
for(i in 1:26){
  #   ##create the egoIDs
  print(i)
  g2<-iGraphList_personalNets[[i]]
  if(length(V(g2))!=0){
    tt<-get.edgelist(g2)
    ##get the index where the edge does not connect on either side to ID==1
    edgeIDsNotOne<-which((tt[,1]!=1)&(tt[,2]!=1))
    g2<-delete.edges(g2,edgeIDsNotOne)
  } 
  iGraphList_personalNets[[i]]<-g2
}





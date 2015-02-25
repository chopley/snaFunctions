pdf(file="barPlots.pdf",paper="a4",width=8,height=10) 
par(mfrow=c(2,2),ps=10)
plotBarChart(avet2$HomophillyEngagement,avet2Engaged$HomophillyEngagement,avet2Instrumental$HomophillyEngagement,"Homophilly on Engagement",c(0,2))
plotBarChart(avet2$HomophillyEthnicity,avet2Engaged$HomophillyEthnicity,avet2Instrumental$HomophillyEthnicity,"Homophilly on Ethnicity",c(0,2))
plotBarChart(avet2$Triadic,avet2Engaged$Triadic,avet2Instrumental$Triadic,"Triadic Closure",c(0,80))
plotBarChart(avet2$Size,avet2Engaged$Size,avet2Instrumental$Size,"Size",c(0,30))
plotBarChart(avet2$Density,avet2Engaged$Density,avet2Instrumental$Density,"Density",c(0,1))
plotBarChart(avet2$Cohesion,avet2Engaged$Cohesion,avet2Instrumental$Cohesion,"Cohesion",c(-0.5,2))
plotBarChart(avet2$nCliques,avet2Engaged$nCliques,avet2Instrumental$nCliques,"Cliques",c(0,10))
plotBarChart(avet2$AverageTieStrength,avet2Engaged$AverageTieStrength,avet2Instrumental$AverageTieStrength,"Average Tie Strength",c(0,5))
plotBarChart(avet2$HomophillyCliqueEngagement,avet2Engaged$HomophillyCliqueEngagement,avet2Instrumental$HomophillyCliqueEngagement,"Homophilly in Cliques on Engagement",c(0,5))
plotBarChart(avet2$nComponents,avet2Engaged$nComponents,avet2Instrumental$nComponents,"Components",c(0,3))
dev.off()

pdf(file="snaPaper_t2.pdf",paper="a4",width=8,height=10) 
par(mfrow=c(2,3),ps=10)
for(i in 1:26){
  print(i)
  EgoID<-i
  #complete networks
  g2<-iGraphList_t2[[i]]
  if(length(V(g2))!=0){
    print('plotting')
    # plot(iGraphList_t0[[i]],main=paste0("Start-EgoID ",V(g0)$egoid[1]))
    #  g0String1<-paste("homophily",round(as.numeric(h_g0[i]),4))
    #  g0String2<-paste("Triadic Closure",round(as.numeric(triadicClos_g0[i]),4))
    #  g0String3<-paste("Density",round(as.numeric(density_g0[i]),4))
    #  mtext(g0String1,side=1,line=1,at=-1,adj=0)
    #  mtext(g0String2,side=1,line=2,at=-1,adj=0)
    #  mtext(g0String3,side=1,line=3,at=-1,adj=0)
    V(g2)$shape[is.na(V(g2)$shape)]<-"square"
    plot(g2,main=paste0("End-EgoID ",V(g2)$egoid[1]),layout=layout.kamada.kawai)
#     g2String1<-paste("homophily",round(as.numeric(t2$Homophilly[i]),2))
#     g2String2<-paste("Triadic Closure",round(as.numeric(t2$Triadic[i]),2))
#     g2String3<-paste("Density",round(as.numeric(t2$Density[i]),2))
#     g2String4<-paste("Cohesion",round(as.numeric(t2$Density[i]),2))
#     g2String5<-paste("Size",round(as.numeric(t2$Size[i]),2))
#     g2String6<-paste("nComponents",round(as.numeric(t2$nComponents[i]),2)) 
#     g2String7<-paste("nCliques",round(as.numeric(t2$nCliques[i]),2)) 
#     g2String8<-paste("mean Tie Strength",round(as.numeric(t2$AverageTieStrength[i]),2)) 
#     g2String9<-paste("clique Hphly Engagement",round(as.numeric(t2$HomophillyCliqueEngagement_g2[i]),3)) 
#     
#     
#     
#     mtext(g2String1,side=1,line=-4,at=-1,adj=0)
#     mtext(g2String2,side=1,line=-3,at=-1,adj=0)
#     mtext(g2String3,side=1,line=-2,at=-1,adj=0)
#     mtext(g2String4,side=1,line=-1,at=-1,adj=0)
#     mtext(g2String5,side=1,line=0,at=-1,adj=0)
#     mtext(g2String6,side=1,line=1,at=-1,adj=0)
#     mtext(g2String7,side=1,line=2,at=-1,adj=0)
#     mtext(g2String8,side=1,line=3,at=-1,adj=0)
#     mtext(g2String9,side=1,line=4,at=-1,adj=0)
  }
}
dev.off()

pdf(file="snaPaper_t2_egoNetwork.pdf",paper="a4",width=8,height=10) 
par(mfrow=c(2,3),ps=10)
for(i in 1:26){
  print(i)
  EgoID<-i
  g2<-iGraphList_egoID[[i]]
  if(length(V(g2))!=0){
    print('plotting')
    V(g2)$shape[is.na(V(g2)$shape)]<-"square"
    plot(g2,main=paste0("End-EgoID ",V(g2)$egoid[1]),layout=layout.kamada.kawai)

  }
}
dev.off()

#plot the personal networks
pdf(file="snaPaper_t2_personalNetwork.pdf",paper="a4",width=8,height=10) 
par(mfrow=c(2,3),ps=10)
for(i in 1:26){
  print(i)
  EgoID<-i
  #personal networks are stored in iGraphList_personalNets
  g2<-iGraphList_personalNets[[i]]
  if(length(V(g2))!=0){
    print('plotting')
    V(g2)$shape[is.na(V(g2)$shape)]<-"square"
    plot(g2,main=paste0("End-EgoID ",V(g2)$egoid[1]),layout=layout.kamada.kawai)
    #plot(g2, paste0("End-EgoID",V(g2)$egoid[1],".svg"), layout=layout.kamada.kawai)
  }
}
dev.off()

#plot the personal networks


for(i in 1:26){
  print(i)
  EgoID<-i
  #complete networks are stored in iGraphList_t2[[i]]
  g2<-iGraphList_t2[[i]]
  if(length(V(g2))!=0){
    print('plotting')
    V(g2)$shape[is.na(V(g2)$shape)]<-"square"
    #plot(g2,main=paste0("End-EgoID ",V(g2)$egoid[1]),layout=layout.kamada.kawai)
    filenameOut<-paste0("CompleteEgoID",V(g2)$egoid[1],".svg")
    print(filenameOut)
    svg(filenameOut)
    plot(g2,main=paste0("CompleteNetwork-EgoID ",V(g2)$egoid[1]) , layout=layout.kamada.kawai)
    dev.off()
  }
}


for(i in 1:26){
  print(i)
  EgoID<-i
  #personal networks are stored in iGraphList_personalNets
  g2<-iGraphList_personalNets[[i]]
  if(length(V(g2))!=0){
    print('plotting')
    V(g2)$shape[is.na(V(g2)$shape)]<-"square"
    #plot(g2,main=paste0("End-EgoID ",V(g2)$egoid[1]),layout=layout.kamada.kawai)
    filenameOut<-paste0("PersonalEgoID",V(g2)$egoid[1],".svg")
    print(filenameOut)
    svg(filenameOut)
    plot(g2,main=paste0("PersonalNetwork-EgoID ",V(g2)$egoid[1]) , layout=layout.kamada.kawai)
    dev.off()
  }
}


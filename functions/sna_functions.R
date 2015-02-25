
#whichSubset:  extract df subset based on group, time &/or network type
#directedTies: adds all edges in reverse direction to ties.df


#
#Shared ties
#
sharedTies <- function(undirected.df){
#undirected.df <- u.ties
  #Count each type of pre-existing tie for all ties
  none <-length(which(undirected.df$ties=="None"))
  ps   <-length(which(undirected.df$ties=="Prison-shared"))
  os   <-length(which(undirected.df$ties=="Out-shared"))
  pd   <-length(which(undirected.df$ties=="Prison-direct"))
  od   <-length(which(undirected.df$ties=="Out-direct"))
  un   <-length(which(undirected.df$ties=="Unknown"))

  #Count each type of pre-existing tie for friendship
  f.none <-length(which(undirected.df$friend==1 & undirected.df$ties=="None"))
  f.ps   <-length(which(undirected.df$friend==1 & undirected.df$ties=="Prison-shared"))
  f.os   <-length(which(undirected.df$friend==1 & undirected.df$ties=="Out-shared"))
  f.pd   <-length(which(undirected.df$friend==1 & undirected.df$ties=="Prison-direct"))
  f.od   <-length(which(undirected.df$friend==1 & undirected.df$ties=="Out-direct"))
  f.un   <-length(which(undirected.df$friend==1 & undirected.df$ties=="Unknown"))
  
  #Count each type of pre-existing tie for associates
  a.none <-length(which(undirected.df$associate==1 & undirected.df$ties=="None"))
  a.ps   <-length(which(undirected.df$associate==1 & undirected.df$ties=="Prison-shared"))
  a.os   <-length(which(undirected.df$associate==1 & undirected.df$ties=="Out-shared"))
  a.pd   <-length(which(undirected.df$associate==1 & undirected.df$ties=="Prison-direct"))
  a.od   <-length(which(undirected.df$associate==1 & undirected.df$ties=="Out-direct"))
  a.un   <-length(which(undirected.df$associate==1 & undirected.df$ties=="Unknown"))


  #Count each type of pre-existing tie for mentors
  m.none <-length(which(undirected.df$mentors==1 & undirected.df$ties=="None"))
  m.ps   <-length(which(undirected.df$mentors==1 & undirected.df$ties=="Prison-shared"))
  m.os   <-length(which(undirected.df$mentors==1 & undirected.df$ties=="Out-shared"))
  m.pd   <-length(which(undirected.df$mentors==1 & undirected.df$ties=="Prison-direct"))
  m.od   <-length(which(undirected.df$mentors==1 & undirected.df$ties=="Out-direct"))
  m.un   <-length(which(undirected.df$mentors==1 & undirected.df$ties=="Unknown"))
  
  #Count each type of pre-existing tie for mentees
  me.none <-length(which(undirected.df$mentees==1 & undirected.df$ties=="None"))
  me.ps   <-length(which(undirected.df$mentees==1 & undirected.df$ties=="Prison-shared"))
  me.os   <-length(which(undirected.df$mentees==1 & undirected.df$ties=="Out-shared"))
  me.pd   <-length(which(undirected.df$mentees==1 & undirected.df$ties=="Prison-direct"))
  me.od   <-length(which(undirected.df$mentees==1 & undirected.df$ties=="Out-direct"))
  me.un   <-length(which(undirected.df$mentees==1 & undirected.df$ties=="Unknown"))
  
  #Compute percentages
  all.ties <- length(undirected.df$ties)
  if(none!=0){none.perc <-round(none/all.ties,2)}else{none.perc <-0}
  if(ps!=0)  {ps.perc   <-round(ps/all.ties,2)  }else{ps.perc <-0}
  if(os!=0)  {os.perc   <-round(os/all.ties,2)  }else{os.perc <-0}
  if(pd!=0)  {pd.perc   <-round(pd/all.ties,2)  }else{pd.perc <-0}
  if(od!=0)  {od.perc   <-round(od/all.ties,2)  }else{od.perc <-0}
  if(un!=0)  {un.perc   <-round(un/all.ties,2)  }else{un.perc <-0}
  all.ties <- length(which(undirected.df$friend==1))
  if(f.none!=0){f.none.perc <-round(f.none/all.ties,2)}else{f.none.perc <-0}
  if(f.ps!=0)  {f.ps.perc   <-round(f.ps/all.ties,2)  }else{f.ps.perc <-0}
  if(f.os!=0)  {f.os.perc   <-round(f.os/all.ties,2)  }else{f.os.perc <-0}
  if(f.pd!=0)  {f.pd.perc   <-round(f.pd/all.ties,2)  }else{f.pd.perc <-0}
  if(f.od!=0)  {f.od.perc   <-round(f.od/all.ties,2)  }else{f.od.perc <-0}
  if(f.un!=0)  {f.un.perc   <-round(f.un/all.ties,2)  }else{f.un.perc <-0}
  all.ties <- length(which(undirected.df$associate==1))
  if(a.none!=0){a.none.perc <-round(a.none/all.ties,2)}else{a.none.perc <-0}
  if(a.ps!=0)  {a.ps.perc   <-round(a.ps/all.ties,2)  }else{a.ps.perc <-0}
  if(a.os!=0)  {a.os.perc   <-round(a.os/all.ties,2)  }else{a.os.perc <-0}
  if(a.pd!=0)  {a.pd.perc   <-round(a.pd/all.ties,2)  }else{a.pd.perc <-0}
  if(a.od!=0)  {a.od.perc   <-round(a.od/all.ties,2)  }else{a.od.perc <-0}
  if(a.un!=0)  {a.un.perc   <-round(a.un/all.ties,2)  }else{a.un.perc <-0}
  all.ties <- length(which(undirected.df$mentors==1))
  if(m.none!=0){m.none.perc <-round(m.none/all.ties,2)}else{m.none.perc <-0}
  if(m.ps!=0)  {m.ps.perc   <-round(m.ps/all.ties,2)  }else{m.ps.perc <-0}
  if(m.os!=0)  {m.os.perc   <-round(m.os/all.ties,2)  }else{m.os.perc <-0}
  if(m.pd!=0)  {m.pd.perc   <-round(m.pd/all.ties,2)  }else{m.pd.perc <-0}
  if(m.od!=0)  {m.od.perc   <-round(m.od/all.ties,2)  }else{m.od.perc <-0}
  if(m.un!=0)  {m.un.perc   <-round(m.un/all.ties,2)  }else{m.un.perc <-0}
  all.ties <- length(which(undirected.df$mentees==1))
  if(me.none!=0){me.none.perc <-round(me.none/all.ties,2)}else{me.none.perc <-0}
  if(me.ps!=0)  {me.ps.perc   <-round(me.ps/all.ties,2)  }else{me.ps.perc <-0}
  if(me.os!=0)  {me.os.perc   <-round(me.os/all.ties,2)  }else{me.os.perc <-0}
  if(me.pd!=0)  {me.pd.perc   <-round(me.pd/all.ties,2)  }else{me.pd.perc <-0}
  if(me.od!=0)  {me.od.perc   <-round(me.od/all.ties,2)  }else{me.od.perc <-0}
  if(me.un!=0)  {me.un.perc   <-round(me.un/all.ties,2)  }else{me.un.perc <-0}
  
  #Bind into single dataframe
  df    <- data.frame(tie=NULL, count=NULL, percentage=NULL)
  none  <- cbind("all-none", none,none.perc) # ps, ps.perc,ps,ps.perc,os,os.perc,od, od.perc,un,un.perc)
  ps    <- cbind("all-ps", ps,ps.perc)   ;pd  <- cbind("all-pd", pd,pd.perc)
  os    <- cbind("all-os", os,os.perc)   ;od  <- cbind("all-od", od,od.perc)
  un    <- cbind("all-un", un,un.perc)   
  all   <- rbind(none,pd,ps,od,os,un)    ; all <- data.frame(all)
  colnames(all) <- colnames(df)
  df    <- rbind(df,all)
  none  <- cbind("friend-none", f.none,f.none.perc) # ps, ps.perc,ps,ps.perc,os,os.perc,od, od.perc,un,un.perc)
  ps    <- cbind("friend-ps", f.ps,f.ps.perc)   ;pd     <- cbind("friend-pd", f.pd,f.pd.perc)
  os    <- cbind("friend-os", f.os,f.os.perc)   ;od     <- cbind("friend-od", f.od,f.od.perc)
  un    <- cbind("friend-un", f.un,f.un.perc)   ;
  friend   <- rbind(none,pd,ps,od,os,un)    ; friend <- data.frame(friend)
  colnames(friend) <- colnames(df)
  df    <- rbind(df,friend)
  none  <- cbind("associate-none", a.none,a.none.perc) # ps, ps.perc,ps,ps.perc,os,os.perc,od, od.perc,un,un.perc)
  ps    <- cbind("associate-ps", a.ps,a.ps.perc)   ;pd     <- cbind("associate-pd", a.pd,a.pd.perc)
  os    <- cbind("associate-os", a.os,a.os.perc)   ;od     <- cbind("associate-od", a.od,a.od.perc)
  un    <- cbind("associate-un", a.un,a.un.perc)  
  associate   <- rbind(none,pd,ps,od,os,un)    ; associate <- data.frame(associate)
  colnames(associate) <- colnames(df)
  df    <- rbind(df,associate)
  none  <- cbind("mentor-none", m.none,m.none.perc) # ps, ps.perc,ps,ps.perc,os,os.perc,od, od.perc,un,un.perc)
  ps    <- cbind("mentor-ps", m.ps,m.ps.perc)   ;pd     <- cbind("mentor-pd", m.pd,m.pd.perc)
  os    <- cbind("mentor-os", m.os,m.os.perc)   ;od     <- cbind("mentor-od", m.od,m.od.perc)
  un    <- cbind("mentor-un", m.un,m.un.perc)   
  mentor   <- rbind(none,pd,ps,od,os,un)    ; mentor <- data.frame(mentor)
  colnames(mentor) <- colnames(df)
  df    <- rbind(df,mentor)
  none  <- cbind("mentee-none", me.none,me.none.perc) # ps, ps.perc,ps,ps.perc,os,os.perc,od, od.perc,un,un.perc)
  ps    <- cbind("mentee-ps", me.ps,me.ps.perc)   ;pd     <- cbind("mentee-pd", me.pd,me.pd.perc)
  os    <- cbind("mentee-os", me.os,me.os.perc)   ;od     <- cbind("mentee-od", me.od,me.od.perc)
  un    <- cbind("mentee-un", me.un,me.un.perc)   
  mentee   <- rbind(none,pd,ps,od,os,un)    ; mentee <- data.frame(mentee)
  colnames(mentee) <- colnames(df)
  df    <- rbind(df,mentee)
  
  return(df)
}

#triadicClosure (CJC): calculates the number of closed triads
#takes an igraph object as input. This should be a directed igraph as undirected causes problems with the 
#algorithm
triadicClosure <- function(igrph){
  triads<-triad.census(as.directed(igrph))
  nTriads<-triads[9]+triads[10]+triads[12]+triads[13]+triads[14]+triads[15]+triads[16]
  return(nTriads)
}



#_____________________________________________________________________________________________
#whichSubset: extract df subset based on group, time &/or network type 
#_____________________________________________________________________________________________
#to specify specific ego, enter egoid number, e.g. 1
#to specify a network, enter column name for that network marker, e.g. "friend" or "mentor"
#to specify that only ego.edges or alter.egdes are wanted, enter "TRUE" (for one or the other, not both)
#for ego.only or alter.only, enter "true" or "TRUE"
#group argument refers to group AD/SY/EH etc



whichSubset <- function(df,group,time,network, friends,
                        ego.edges, alter.edges,ego.only,alter.only,specific.ego){
  if(missing(time))        {time         <- "all"}
  if(missing(group))       {group        <- "all"}
  if(missing(network))     {network      <- "all"}
  if(missing(friends))     {friends      <- "all"}
  if(missing(ego.edges))   {ego.edges    <- "all"}
  if(missing(alter.edges))  {alter.edges <- "all"}
  if(missing(ego.only))        {ego.only <- "false"}
  if(missing(alter.only))    {alter.only <- "false"}
  if(missing(specific.ego)){specific.ego <- "false"}

      if(time!="all"){
        #select ties in that time period
        t.rows    <- which(df$time==time)
        df        <- df[t.rows,]
      }
  
  if(friends!="all"){
    f.rows <- which(df$friends==friends)
    df     <- df[f.rows,]
  }
      
      if(group!="all"){
            if(group=="engaged"){
              g.rows <- which(df$e.engagement=="engaged")
              df     <- df[g.rows,]
            }else{
                  if(group=="instrumental"){
                    g.rows <- which(df$e.engagement=="instrumental")
                    df     <- df[g.rows,]
                  }else{
                          g.rows <- which(df$group==group)
                          df     <- df[g.rows,]
                        }
            }
      }

      if(network!="all"){
        col    <- which(names(df)==network) #find index for column which = that network identifier
        n.rows <- which(df[,col]==1)        #select only those with 1 in that column
        df     <- df[n.rows,]
      }
      
      #where edges are subset by whether they include ego (ego.edges) or include only alters(alter.edges)
      #subset based on n1 and n2, where if either==1 then the edge includes ego
      if(ego.edges!="all"){
        e.rows <- which(df$n1==1 | df$n2==1)  
        df     <- df[e.rows,]               
      }
     
      if(alter.edges!="all"){
        a.rows <- which(df$n1!=1 & df$n2!=1)#subsets alter edges in both directions
        df     <- df[a.rows,]
      }
      
      #where node info required for egos or alters only, subset based only on n1 because all egos=1
      #and alter information in df corresponds to n1
      if(ego.only!="false"){
        e.rows <- which(df$n1==1)
        df     <- df[e.rows,]
      }
  
      if(alter.only!="false"){
        a.rows <- which(df$n1!=1)
        df     <- df[a.rows,]
      }
  
      if(specific.ego!="false"){
        rows   <- which(df$egoid==specific.ego)
        df     <- df[rows,]
      }
  
  

  return(df)
}
#_____________________________________________________________________________________________
#density.fun: Compute density for each network in the set 
#_____________________________________________________________________________________________
#note that df should be dataset with full set of ties in BOTH directions (ie all ties, assuming directional)
#ties= number 1-4 defining which type of tie (ie network) to count
#where 1=associates,2=prison friends, 3=treatment friends, 4=mentorships
#df<- temp.df; ties=2
density.fun <- function(df,ties){
  if(missing(ties)){ties <- "all"}
  #df<- temp.df; ties="1"
  
  #count number of alters in the network
  #unique.nodes       <- ddply(df, c("egoid"), summarise, alters=length(unique(c(nodeid.x,nodeid.y))))
  #unique.nodes[,2]  <- unique.nodes[,2] -1   #subtract ego from total number of nodes
  
  #count number of ties not involving ego in the network for each egoid
#This ddply approach does not work, i'm not sure why - counts ties as 0 for all egos, which is incorrect
  #even if n1/n2 values are numeric or if friends==ties
#     specific.ties  <- ddply(df, c("egoid"), summarise, 
#                             nonego.ties=length(which(n1!="1" & n2!="1" & friends!=ties)))
    
    index      <- unique(df$egoid)
    density.df <- data.frame(egoid=NULL,all.nodes=NULL,net.size=NULL,
                             degree=NULL,density=NULL)

    for(i in index){
      egoid <- i
      #Subset a single ego's network
      sub.df          <- which(df$egoid==i)
      sub.df          <- df[sub.df,]
      
      #SIZE: Count number of alters in ego's entire network
      #Do not need to divide in two because only unique node IDs are counted
      all.nodes       <- length(unique(c(sub.df$n1,sub.df$n2)))
      if(all.nodes > 0){all.nodes <- all.nodes-1} #subtract ego from tally
      
      #Compile indices of which ties include ego (ego.index) and which ties
      #do not include ego (alter.index)
      #with if condition used to identify only those ties that are specified
      #unless`ties' argument == "all"
      if(ties!="all"){        
        #Count only ties of relevant type (sub.df$friends==ties)
        ego.index      <- which(sub.df$friends==ties & (sub.df$n1==1 | sub.df$n2==1))        
        alter.index    <- which(sub.df$friends==ties & sub.df$n1!=1 & sub.df$n2!=1)
      }else{ego.index  <- which(sub.df$n1==1 | sub.df$n2==1)
            alter.index<- which(sub.df$n1!=1 & sub.df$n2!=1)
      }
      #i=5

      #DEGREE
      #Use ego & alter indices to count number of ties ego had (degree)
        degree              <- length(ego.index)
        if(degree>0){degree <- degree/2}

      #DENSITY
      #where n(n-1)/2 = max # ties betw alters, with n= number of alters
      #L= number of ties that exist between alters
      #density = L/(n(n-1)/2)
#note: in Prell p. 121, n is defined as alters connected to ego but here all alters
#that ego identified are included in the calculation
        L        <- length(alter.index) #ties between alters
        if(L>0){L<- L/2                 #since ties are listed in both directions
                density  <- L/(all.nodes*(all.nodes-1)/2)   
                }else{density <- 0}                
        density <- 100* density

      #ALTERS IN THAT NETWORK
      if(ties!="all"){
        index    <- which(sub.df$friends==ties)
        net.size <- length(unique(c(sub.df$n1[index],sub.df$n2[index])))
      }else{net.size <- all.nodes}
      
      #End loop by combining measures into a row and attaching to density.df
      row        <- cbind(egoid,all.nodes,net.size,degree,density) 
      density.df <- rbind(density.df,row)

} #end loop




  return(density.df)
  
}
#_____________________________________________________________________________________________
#HOMOPHILY
#_____________________________________________________________________________________________

homophily <- function(df, ties, group){
  
  if(missing(ties)){ties <- "all"}
  
  #Create index and empty dataset for loop
  index      <- unique(df$egoid)
  h.df       <- data.frame(egoid=NULL,h.engE0=NULL,h.engI0=NULL,h.engE2=NULL,h.engI2=NULL,
                           h.ethW0=NULL,h.ethB0=NULL,h.ethOth0=NULL,h.ethW2=NULL,h.ethB2=NULL,h.ethOth2=NULL)
  
  
  #START LOOP through each ego's network
  for(i in index){
    
    #Record ego id
    egoid <- i
    
    #Subset a single ego's network
    sub.df          <- which(df$egoid==i)
    sub.df          <- df[sub.df,]
    
    #Divide into t0 and t2 subsets of ego's dataset
    t0 <- which(sub.df$time==0)
    t0 <- sub.df[t0,]
    t2 <- which(sub.df$time==2)
    t2 <- sub.df[t2,]
    
    #Count total number of ego ties in network being assessed       
    if(ties=="friends"){
      ego.index0      <- which((t0$friends==1 | t0$friends==2 |t0$friends==3) & (t0$n1==1 | t0$n2==1))
      ego.index2      <- which((t2$friends==1 | t2$friends==2 |t2$friends==3) & (t2$n1==1 | t2$n2==1))
      
    }else{
      if(ties!="all"){
        #Count only ties of relevant type (sub.df$friends==ties)
        ego.index0      <- which(t0$friends==ties & (t0$n1==1 | t0$n2==1))        
        ego.index2      <- which(t2$friends==ties & (t2$n1==1 | t2$n2==1))    
      }else{
        ego.index0  <- which(t0$n1==1 | t0$n2==1)
        ego.index2  <- which(t2$n1==1 | t2$n2==1)
      }
    }
    
    n.alters0                 <- length(ego.index0)
    if(n.alters0>0){n.alters0 <- n.alters0/2}
    n.alters2                 <- length(ego.index2)
    if(n.alters2>0){n.alters2 <- n.alters2/2}
    
    #Subset to remove non-ego ties
    t0 <- t0[ego.index0,]
    t2 <- t2[ego.index2,]
    
    #Now subset data on alters (connected to ego)
    alters0 <- which(t0$n2==1)
    alters0 <- t0[alters0,]
    alters2 <- which(t2$n2==1)
    alters2 <- t2[alters2,]    
    
    #Engagement
    engE0 <- length(which(alters0$engagement=="engaged"))
    if(engE0>0){h.engE0<-round(engE0/n.alters0,2)}else{h.engE0 <- 0}
    engI0 <- length(which(alters0$engagement=="instrumental"))
    if(engI0>0){h.engI0<-round(engI0/n.alters0,2)}else{h.engI0 <- 0} 
    
    engE2 <- length(which(alters2$engagement=="engaged"))
    if(engE2>0){h.engE2<-round(engE2/n.alters2,2)}else{h.engE2 <- 0}
    engI2 <- length(which(alters2$engagement=="instrumental"))
    if(engI2>0){h.engI2<-round(engI2/n.alters2,2)}else{h.engI2 <- 0} 
    
    #Ethnicity
    ethW0 <- length(which(alters0$ethnicity=="White"))
    if(ethW0>0){h.ethW0<-round(ethW0/n.alters0,2)}else{h.ethW0 <- 0}
    ethB0 <- length(which(alters0$ethnicity=="Black"))
    if(ethB0>0){h.ethB0<-round(ethB0/n.alters0,2)}else{h.ethB0 <- 0} 
    ethOth0 <- length(which(alters2$ethnicity!="Black" & alters2$ethnicity!="White" ))
    if(ethOth0>0){h.ethOth0<-round(ethOth0/n.alters2,2)}else{h.ethOth0 <- 0} 
    
    ethW2 <- length(which(alters2$ethnicity=="White"))
    if(ethW2>0){h.ethW2<-round(ethW2/n.alters2,2)}else{h.ethW2 <- 0}
    ethB2 <- length(which(alters2$ethnicity=="Black"))
    if(ethB2>0){h.ethB2<-round(ethB2/n.alters2,2)}else{h.ethB2 <- 0} 
    ethOth2 <- length(which(alters2$ethnicity!="Black" & alters2$ethnicity!="White" ))
    if(ethOth2>0){h.ethOth2<-round(ethOth2/n.alters2,2)}else{h.ethOth2 <- 0} 
    
    #End loop by combining measures into a row and attaching to density.df
    row        <- cbind(egoid,h.engE0,h.engI0,h.engE2,h.engI2,
                        h.ethW0,h.ethB0,h.ethOth0,h.ethW2,h.ethB2,h.ethOth2)
    
    h.df <- rbind(h.df,row)
  }
  
  names <- c("egoid","t0.engaged","t0.instrumental","t0.white",
             "t0.black", "t0.other","t2.engaged",
             "t2.instrumental","t2.white",
             "t2.black", "t2.other")
  
  colnames(h.df) <- names
  
  if(missing(group)){group <- "none"}
  if(group!="none"){
    if(group==1){h.df$motivation <- rep("treatment", length(h.df$egoid))
                 h.df$engagement <- rep("engaged", length(h.df$egoid))}
    if(group==2){h.df$motivation <- rep("both", length(h.df$egoid))
                 h.df$engagement <- rep("engaged", length(h.df$egoid))}
    if(group==3){h.df$motivation <- rep("instrumental", length(h.df$egoid))
                 h.df$engagement <- rep("engaged", length(h.df$egoid))}
    if(group==4){h.df$motivation <- rep("treatment", length(h.df$egoid))
                 h.df$engagement <- rep("instrumental", length(h.df$egoid))}
    if(group==5){h.df$motivation <- rep("both", length(h.df$egoid))
                 h.df$engagement <- rep("instrumental", length(h.df$egoid))}
    if(group==6){h.df$motivation <- rep("instrumental", length(h.df$egoid))
                 h.df$engagement <- rep("instrumental", length(h.df$egoid))}
    if(group=="Black"){h.df$motivation <- rep("ethnicity", length(h.df$egoid))
                       h.df$engagement <- rep("Black", length(h.df$egoid))}
    if(group=="White"){h.df$motivation <- rep("ethnicity", length(h.df$egoid))
                       h.df$engagement <- rep("White", length(h.df$egoid))}    
  }
  
  return(h.df)
}
#_____________________________________________________________________________________________
#summariseNodes: Table of summary stats for any group of nodes 
#_____________________________________________________________________________________________
#df<-ad.summary
#if just.egos="TRUE", then includes measures just available for egos like post-prison outcome

summaryNodes <- function(df, just.egos, just.alters){
  if(missing(just.egos)){just.egos     <- "FALSE"}
  if(missing(just.alters)){just.alters <- "FALSE"}
  
  #If just.egos is true, subset df to extract only ties that involve ego. Since the df includes
  #ties in both directions (ie. two rows for each tie), each tie that involves ego will have at
  #least one row where n1==1 (nodes numbered 1 = egos)
  #-----------------------------------------------------------------------
  if(just.egos=="TRUE"){
        ego.rows  <- which(df$n1==1)
        df        <- df[ego.rows,]
        ego.index <- unique(df$egoid)
        ego.df    <- df[0,]
            for(i in ego.index){
              rows      <- which(df$egoid==i)
              subset    <- df[rows,]
              ego.row   <- subset[1,]
              ego.df    <- rbind(ego.df,ego.row)
            }
        df<- ego.df
    
  }else{

      if(just.alters=="TRUE"){
        alter.rows  <- which(df$n1!=1)
        df        <- df[alter.rows,]
        alter.index <- unique(df$nodeid)
        alter.df    <- df[0,]
            for(i in alter.index){
              rows      <- which(df$nodeid==i)
              subset    <- df[rows,]
              alter.row   <- subset[1,]
              alter.df    <- rbind(alter.df,alter.row)
            }
        df<- alter.df
      }
  }

 df.out <- data.frame(var=NULL, value=NULL)
       #N -------------------------------------------------
       var    <- "n"
       value  <- length(df$age)
       n      <- cbind(var,value)
       df.out <- rbind(df.out, n)
  
       #Age -------------------------------------------------
       var      <- "Age: Mean (SD)"
       df$age   <- as.numeric(as.character(df$age)) 
       mean.age <- round(mean(df$age, na.rm=TRUE),2)
       sd.age   <- round(sd(df$age, na.rm=TRUE),2)
       value    <- paste(mean.age," (", sd.age,")", sep="")
       age      <- cbind(var,value)
       df.out   <- rbind(df.out,age)
       
       var      <- "Age: Min, Max"
       min      <- min(df$age, na.rm=TRUE)
       max      <- max(df$age, na.rm=TRUE)
       value    <- paste(min,"-", max, sep="")
       age2     <- cbind(var,value)
       df.out   <- rbind(df.out,age2)
       
       #Ethnicity-------------------------------------------
       var        <- "Ethnicity: n(%)"; value      <- ""
       ethnicity  <- cbind(var,value)
       df.out     <- rbind(df.out,ethnicity)
       
       eth.counts <- table(df$ethnicity)
       categories <- names(eth.counts)
       ethnicity  <- data.frame(var=NULL, value=NULL)
       total.n    <- length(all.df$ethnicity)
       
        #Use loop to count number and compute % for each category of that variable
       for(i in categories){
         var       <- names(eth.counts[i])
         index     <- which(names(eth.counts)==i)
         count     <- eth.counts[[i]]
         if(count>0){perc      <- round(count/total.n,2); perc<- 100*perc}else{perc = 0}
         value     <- paste(count, " (",perc,"%)", sep="")
         row       <- cbind(var,value)
         ethnicity <- rbind(ethnicity,row) 
         }
       df.out   <- rbind(df.out,ethnicity)
      
       #offence-------------------------------------------
       var        <- "offence: n(%)"; value      <- ""
       offence    <- cbind(var,value)
       df.out     <- rbind(df.out,offence)
       off.counts <- table(df$offence)
       categories <- names(off.counts)
       offence  <- data.frame(var=NULL, value=NULL)
       total.n    <- length(all.df$offence)
      
       for(i in categories){
         var       <- names(off.counts[i])
         var       <- paste("off-", var)
         index     <- which(names(off.counts)==i)
         count     <- off.counts[[i]]
         if(count>0){perc      <- round(count/total.n,2); perc<- 100*perc}else{perc = 0}
         value     <- paste(count, " (",perc,"%)", sep="")
         row       <- cbind(var,value)
         offence   <- rbind(offence,row) 
       }
       df.out <- rbind(df.out,offence)
       
       #drug-------------------------------------------
       var        <- "drug: n(%)"; value      <- ""
       drug       <- cbind(var,value)
       df.out     <- rbind(df.out,drug)
       
       drug.counts<- table(df$drug)
       categories <- names(drug.counts)
       drug       <- data.frame(var=NULL, value=NULL)
       total.n    <- length(all.df$drug)
      
       for(i in seq(length(categories))){
         var       <- names(drug.counts[i])
         var       <- paste("drug-", var)
         index     <- which(names(drug.counts)==i)
         count     <- drug.counts[[i]]
         if(count>0){perc      <- round(count/total.n,2); perc<- 100*perc}else{perc = 0}
         value     <- paste(count, " (",perc,"%)", sep="")
         row       <- cbind(var,value)
         drug      <- rbind(drug,row) 
       }
       df.out <- rbind(df.out,drug)
       
       #engagement-------------------------------------------
       var        <- "Engagement: n(%)"; value      <- ""
       eng        <- cbind(var,value)
       df.out     <- rbind(df.out,eng)
       total.n    <- length(df$engaged)
       
       var        <- "e-Engaged"
       count      <- length(which(df$engaged=="engaged"))
       if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
       value      <- paste(count, " (", perc,"%)", sep="")
       engaged    <- cbind(var,value)
       
       var        <- "e-Instrumental"
       count      <- length(which(df$engaged=="instrumental"))
       if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
       value      <- paste(count, " (", perc,"%)", sep="")
       instrument <- cbind(var,value)
       
       var        <- "e-Unknown/not applicable"
       count      <- length(which(df$engaged!="instrumental" & df$engaged!="engaged"))
       if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
       value      <- paste(count, " (", perc,"%)", sep="")
       unknown    <- cbind(var,value)
        #----------------------------------------  
       df.out     <- rbind(df.out,engaged,instrument,unknown)
        
        
        #prison history-------------------------------------------
        var        <- "Prison history binary: n(%)"; value      <- ""
        ph         <- cbind(var,value)      ; df.out     <- rbind(df.out,ph)
        var        <- "phb-Yes"#----------------------------------------  
        count      <- length(which(df$prison.any=="yes"))
        if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
        value      <- paste(count, " (", perc,"%)", sep="")
        yes        <- cbind(var,value)
        var        <- "phb-No"#----------------------------------------  
        count      <- length(which(df$prison.any=="no"))
        if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
        value      <- paste(count, " (", perc,"%)", sep="")
        no         <- cbind(var,value)
        var        <- "phb-Unknown/not applicable"#-------------------- 
        count      <- length(which(df$prison.any=="unknown"))
        if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
        value      <- paste(count, " (", perc,"%)", sep="")
        unknown    <- cbind(var,value)
        #----------------------------------------  
        df.out     <- rbind(df.out,yes,no,unknown)
        
        #sentence duration-------------------------------------------
        var        <- "Sentence: n(%)"; value      <- ""
        ph         <- cbind(var,value)      ; df.out     <- rbind(df.out,ph)
        var        <- "s-Short" #----------------------------------------                        
        count      <- length(which(df$sentence=="short"))
        if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
        value      <- paste(count, " (", perc,"%)", sep="")
        short      <- cbind(var,value)
        var        <- "s-Medium" #----------------------------------------
        count      <- length(which(df$sentence=="medium"))
        if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
        value      <- paste(count, " (", perc,"%)", sep="")
        medium     <- cbind(var,value)
        var        <- "s-Long" #----------------------------------------
        count      <- length(which(df$sentence=="long"))
        if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
        value      <- paste(count, " (", perc,"%)", sep="")
        long       <- cbind(var,value)
        var        <- "s-Life" #----------------------------------------
        count      <- length(which(df$sentence=="life"))
        if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
        value      <- paste(count, " (", perc,"%)", sep="")
        life       <- cbind(var,value)
        var        <- "s-Unknown/not applicable"#---------------------
        count      <- length(which(df$sentence=="unknown" | df$sentence==""))
        if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
        value      <- paste(count, " (", perc,"%)", sep="")
        unknown    <- cbind(var,value)
        #----------------------------------------
        df.out     <- rbind(df.out,short,medium,long,life,unknown) #**** Attach to df.out
   
 
 if(just.egos=="TRUE"){
         #Treatment year-------------------------------------------
         var      <- "Tx Year: Mean (SD)"
         df$tx.year   <- as.numeric(as.character(df$tx.year)) 
         mean.tx.year <- round(mean(df$tx.year, na.rm=TRUE),2)
         sd.tx.year   <- round(sd(df$tx.year, na.rm=TRUE),2)
         value    <- paste(mean.tx.year," (", sd.tx.year,")", sep="")
         tx.year  <- cbind(var,value)
         df.out   <- rbind(df.out,tx.year)  #**** Attach to df.out
         
         var      <- "Tx.year: Min, Max"
         min      <- min(df$tx.year, na.rm=TRUE)
         max      <- max(df$tx.year, na.rm=TRUE)
         value    <- paste(min,"-", max, sep="")
         tx.year2 <- cbind(var,value)
         df.out   <- rbind(df.out,tx.year2)#**** Attach to df.out
         
      
         #Treatment duration---------------------------------------
         #tx.length
         var            <- "Tx.length: Mean (SD)"
         df$tx.length   <- as.numeric(as.character(df$tx.length)) 
         mean.tx.length <- round(mean(df$tx.length, na.rm=TRUE),2)
         sd.tx.length   <- round(sd(df$tx.length, na.rm=TRUE),2)
         value          <- paste(mean.tx.length," (", sd.tx.length,")", sep="")
         tx.length      <- cbind(var,value)
         df.out         <- rbind(df.out,tx.length)
         
         var      <- "Tx.length: Min, Max"
         min      <- min(df$tx.length, na.rm=TRUE)
         max      <- max(df$tx.length, na.rm=TRUE)
         value    <- paste(min,"-", max, sep="")
         tx.length2     <- cbind(var,value)
         df.out   <- rbind(df.out,tx.length2) # ***  ATTACH  ***
         
         #Treatment completion-------------------------------------
         #tx.outcome
         var        <- "Treatment Completion: n (%)"; value <- ""
         tx.comp    <- cbind(var,value)
         df.out     <- rbind(df.out,tx.comp)  # ***  ATTACH  ***
         var        <- "Completed"#---------------------
         count      <- length(which(df$sentence=="Completed"))
         if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
         value      <- paste(count, " (", perc,"%)", sep="")
         comp       <- cbind(var,value)
         var        <- "Did not complete"#---------------------
         count      <- length(which(df$sentence=="Noncomplete"))
         if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
         value      <- paste(count, " (", perc,"%)", sep="")
         noncomp    <- cbind(var,value)
         df.out     <- rbind(df.out,noncomp,comp)  # ***  ATTACH  ***
         
         #Drug outcome---------------------------------------------
         #postprison.drug
         var        <- "Post-Treatment Drug Use (detailed): n (%)"; value <- ""
         du         <- cbind(var,value)
         df.out     <- rbind(df.out,du)  # ***  ATTACH  ***
         var        <- "du-Initial"#---------------------
         count      <- length(which(df$postprison.drug=="Initial"))
         if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
         value      <- paste(count, " (", perc,"%)", sep="")
         initial    <- cbind(var,value)
         var        <- "du-Ongoing"#---------------------
         count      <- length(which(df$postprison.drug=="Ongoing"))
         if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
         value      <- paste(count, " (", perc,"%)", sep="")
         ongoing    <- cbind(var,value)
         var        <- "du-None"#---------------------
         count      <- length(which(df$postprison.drug=="None"))
         if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
         value      <- paste(count, " (", perc,"%)", sep="")
         none       <- cbind(var,value)
         df.out     <- rbind(df.out,initial,ongoing, none) # ***  ATTACH  ***
         
         var        <- "Post-Treatment Drug Use (binary): n (%)"; value <- ""
         du         <- cbind(var,value)
         df.out     <- rbind(df.out,du)  # ***  ATTACH  *** 
         var        <- "dub-Yes"#---------------------
         count      <- length(which(df$postprison.drug=="Initial"|df$postprison.drug=="Ongoing"))
         if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
         value      <- paste(count, " (", perc,"%)", sep="")
         yes        <- cbind(var,value)
         var        <- "dub-No"#---------------------
         count      <- length(which(df$postprison.drug=="No"))
         if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
         value      <- paste(count, " (", perc,"%)", sep="")
         no         <- cbind(var,value)
         df.out     <- rbind(df.out,yes,no) # ***  ATTACH  ***
         
         #Recid outcome--------------------------------------------
         #postprison.recid
         var        <- "Post-Treatment Recidivism: n (%)"; value <- ""
         recid      <- cbind(var,value)
         df.out     <- rbind(df.out,recid)  # ***  ATTACH  *** 
         var        <- "r-Yes"#---------------------
         count      <- length(which(df$postprison.recid=="Conviction"|df$postprison.recid=="No conviction"))
         if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
         value      <- paste(count, " (", perc,"%)", sep="")
         yes        <- cbind(var,value)
         var        <- "r-No"#---------------------
         count      <- length(which(df$postprison.recid=="None"))
         if(count!=0){perc <- round(count/total.n, 2); perc<- 100*perc}else{perc <- 0}
         value      <- paste(count, " (", perc,"%)", sep="")
         no         <- cbind(var,value)
         df.out     <- rbind(df.out,yes,no) # ***  ATTACH  ***
 } # END OF EGO ONLY
 
 return(df.out)
}

#_____________________________________________________________________________________________
#networkSizeDensity Get density and size of single network from df (not network object)
#_____________________________________________________________________________________________

networkSizeDensity <- function(df, network){
#df<- net1_t0

  if(missing(network)){network <- "FALSE"}
  
  #count number of unique nodeids to get total size of this network
  size  <- length(unique(df$nodeid))
  size  <- size-1  #subtract ego
  
  #DENSITY: actual ties without ego / all possible ties without ego
  #L divided by n(n-1)/2, where L= number of ties in network that do not involve ego and n= number of alters
  #not counting ego  
  if(network!="FALSE"){
    col.index   <- which(names(df)==network)
    total.edges <- length(which(df$n1!=1 & df$n2!=1 & df[,col.index]==1))
  }else{total.edges <- length(which(df$n1!=1 & df$n2!=1))}
  
  if(total.edges !=0){
    if(total.edges!="mentor" & total.edges!="mentee"){
    total.edges   <- total.edges/2} #since friend/assoc etc. networks=undirected but df=directed
  }

  
  max.edges   <- (size*(size-1))/2 
  
  if(total.edges != 0){
    density     <- total.edges/max.edges
  }else{density <- 0}
  
  df.out <- data.frame(size=size, density=density)

return(df.out)  
}


#__________________________________________________________________________________________
#computeHomophily
#__________________________________________________________________________________________
#Compute homophily for a given variable (specified in var.name) for a single network, which has
#already been converted to an igraph object using the make_iNet function

computeHomophily <- function(net,var.name){
  #var.name="age.cat"
  #net.x<- net
  #net <- inet
  
  #extract edge data.frame and vertices data.frame from igraph object
  edgelist  <- asDF(net, "edgelist") 
  edges     <- edgelist$edges            #extract edge list
  verts     <- edgelist$vertexes         #extract vertex list
  edges     <- as.data.frame(edges)
  verts     <- as.data.frame(verts)
  var.col   <- which(names(verts)==var.name)
  
  #match verts data on attribute (verts[, var.col]), using edge-verts matching with
  #variables: verts$name (vertex name, ie original alter id)
  #edges$n1.1; n1.1.2 (n1 and n2 ids, added to edgelist when df converted to i net object)
  edges$n1.var   <- as.character(verts[,var.col][match(edges$n1.1,verts$name)])
  edges$n2.var   <- as.character(verts[,var.col][match(edges$n2.1,verts$name)])
  matches        <- length(which(edges$n1.var==edges$n2.var))
  total          <- length(edges$n1.var)
  hphy           <- matches/total
 
  
  return(hphy)
}

#_____________________________________________________________________________________________
#networkSNA Get network measures using igraph, from network object and df
#_____________________________________________________________________________________________

networkSNA <- function(net){
  #net<- inet
  
  i.homophily.age        <- computeHomophily(net,var.name="age.cat")
  i.homophily.ethnicity  <- computeHomophily(net,var.name="ethnicity")
  i.homophily.offence    <- computeHomophily(net,var.name="offence")
  i.homophily.sentence   <- computeHomophily(net,var.name="sentence")
  i.homophily.violence   <- computeHomophily(net,var.name="violence")
  i.homophily.status     <- computeHomophily(net,var.name="status")
  i.homophily.drug       <- computeHomophily(net,var.name="drug")
  i.homophily.engagement <- computeHomophily(net,var.name="engagement")
  
  i.density   <- graph.density(net, loops=FALSE)
  i.between   <- betweenness(net, v=V(net)$name=="1", directed=TRUE, weights=NULL)
  if(length(i.between)!=0){i.between <- i.between[[1]]}else{i.between    <- 0}
  
  i.df <- data.frame(i.homophily.age,i.homophily.ethnicity,i.homophily.offence,
                     i.homophily.sentence,i.homophily.violence,i.homophily.status,
                     i.homophily.drug,i.homophily.engagement,
                     i.density,i.between)
  
  return(i.df)
  
}

#_____________________________________________________________________________________________
#multipleSNA: computes SNA measures for multiple ego networks
#_____________________________________________________________________________________________
#Ego networks must be in df, with directed ties (ie both directions); not multiplex, with a single
#type of ties specified (e.g. network.type="friend"), and a single time point
#output is a data frame, where rows= egoids for each network and columns are the various sna measures
#including homophily for each alter variable, using the computeHomphile function written here

multipleSNA <- function(df,network.type){
  #  df<- ad; network.type="friend"
  
  network.index <- unique(df$egoid)
  
  sna.df <- data.frame(egoid=NULL,initial=NULL,group=NULL,sizeD=NULL,net_sna=NULL)
  
  for(i in network.index){
    subset    <- which(df$egoid==i)
    subset    <- df[subset,]
    sizeD     <- networkSizeDensity(subset, network=network.type)
    net_i     <- make_iNet(subset, network.type)
    net_sna   <- networkSNA(net=net_i)
    egoid     <- subset$egoid[1] 
    initial   <- subset$initial[1]
    group     <- as.character(subset$group[1])
    sna.stats <- cbind(egoid,initial,group,sizeD,net_sna)
    sna.df    <- rbind(sna.df,sna.stats)
  }
  
  sna.df$type <- rep(network.type, length(sna.df$egoid))
  
  return(sna.df)
}

#_____________________________________________________________________________________________
#summarySNA: summarise SNA measures from multiple ego networks
#_____________________________________________________________________________________________
#sna.df = output from multipleSNA, containing SNA measures for multiple ego networks identified
#by egoid in the sna.df
#output is data.frame with summary stats for each measure

summarySNA <- function(sna.df){
  #  sna.df <- ad.stats
  
  mean.size      <- round(mean(sna.df$size,na.rm=TRUE),2)
  sd.size        <- round(sd(sna.df$size, na.rm=TRUE), 2)
  size           <- paste(mean.size, " (", sd.size, ")", sep="")
  min            <- round(min(sna.df$size, na.rm=TRUE),2)
  max            <- round(max(sna.df$size, na.rm=TRUE),2)
  size.range     <- paste(min,max,sep="-")
  
  inf.rows       <- which(sna.df$density==Inf)
  sna.df$density[inf.rows] <- rep(NA, length(inf.rows))
  mean.density   <- round(mean(sna.df$density,na.rm=TRUE),2)
  sd.density     <- round(sd(sna.df$density, na.rm=TRUE), 2)
  density        <- paste(mean.density, " (", sd.density, ")", sep="")
  min            <- round(min(sna.df$density, na.rm=TRUE),2)
  max            <- round(max(sna.df$density, na.rm=TRUE),2)
  density.range  <- paste(min,max,sep="-")
  
  mean.i.density <- round(mean(sna.df$i.density,na.rm=TRUE),2)
  sd.i.density   <- round(sd(sna.df$i.density, na.rm=TRUE), 2)
  i.density      <- paste(mean.i.density, " (", sd.i.density, ")", sep="")
  min            <- round(min(sna.df$i.density, na.rm=TRUE),2)
  max            <- round(max(sna.df$i.density, na.rm=TRUE),2)
  i.density.range  <- paste(min,max,sep="-")
  
  mean.between <- round(mean(sna.df$i.between,na.rm=TRUE),2)
  sd.between   <- round(sd(sna.df$i.between, na.rm=TRUE), 2)
  between      <- paste(mean.between, " (", sd.between, ")", sep="")
  
  mean.i.homophily.age <- round(mean(sna.df$i.homophily.age,na.rm=TRUE),2)
  sd.i.homophily.age   <- round(sd(sna.df$i.homophily.age, na.rm=TRUE), 2)
  hphy.age             <- paste(mean.i.homophily.age, " (", sd.i.homophily.age, ")", sep="")
  
  mean.i.homophily.ethnicity <- round(mean(sna.df$i.homophily.ethnicity,na.rm=TRUE),2)
  sd.i.homophily.ethnicity   <- round(sd(sna.df$i.homophily.ethnicity, na.rm=TRUE), 2)
  hphy.ethnicity             <- paste(mean.i.homophily.ethnicity, " (", sd.i.homophily.ethnicity, ")", sep="")
  
  mean.i.homophily.offence <- round(mean(sna.df$i.homophily.offence,na.rm=TRUE),2)
  sd.i.homophily.offence   <- round(sd(sna.df$i.homophily.offence, na.rm=TRUE), 2)
  hphy.offence             <- paste(mean.i.homophily.offence, " (", sd.i.homophily.offence, ")", sep="")
  
  mean.i.homophily.sentence <- round(mean(sna.df$i.homophily.sentence,na.rm=TRUE),2)
  sd.i.homophily.sentence   <- round(sd(sna.df$i.homophily.sentence, na.rm=TRUE), 2)
  hphy.sentence             <- paste(mean.i.homophily.sentence, " (", sd.i.homophily.sentence, ")", sep="")
  
  mean.i.homophily.violence <- round(mean(sna.df$i.homophily.violence,na.rm=TRUE),2)
  sd.i.homophily.violence   <- round(sd(sna.df$i.homophily.violence, na.rm=TRUE), 2)
  hphy.violence             <- paste(mean.i.homophily.violence, " (", sd.i.homophily.violence, ")", sep="")
  
  mean.i.homophily.status <- round(mean(sna.df$i.homophily.status,na.rm=TRUE),2)
  sd.i.homophily.status   <- round(sd(sna.df$i.homophily.status, na.rm=TRUE), 2)
  hphy.status             <- paste(mean.i.homophily.status, " (", sd.i.homophily.status, ")", sep="")
  
  mean.i.homophily.drug <- round(mean(sna.df$i.homophily.drug,na.rm=TRUE),2)
  sd.i.homophily.drug   <- round(sd(sna.df$i.homophily.drug, na.rm=TRUE), 2)
  hphy.drug             <- paste(mean.i.homophily.drug, " (", sd.i.homophily.drug, ")", sep="")
  
  mean.i.homophily.engagement <- round(mean(sna.df$i.homophily.engagement,na.rm=TRUE),2)
  sd.i.homophily.engagement   <- round(sd(sna.df$i.homophily.engagement, na.rm=TRUE), 2)
  hphy.engagement             <- paste(mean.i.homophily.engagement, " (", sd.i.homophily.engagement, ")", sep="")
  
  network.summary <- data.frame(size,size.range,density,density.range,i.density,i.density.range,between,
                                hphy.age,hphy.ethnicity,hphy.offence,hphy.sentence,hphy.violence,
                                hphy.status,hphy.drug,hphy.engagement)
  
  return(network.summary)
  
}
#__________________________________________________________________________________________
#groupNetworks: summarise a network at each time point for all groups
#__________________________________________________________________________________________
#df=all.df; network.type="friend"
groupNetworks <- function(df, network.type){
  if(missing(network.type)){network.type <- "friend"} 
  
  #Group AD
  #subset group AD at time 0 of that particular network type
  ad0         <- whichSubset(df=df,time=0,network=network.type, group="AD") 
  #compute SNA measures for each ego-network of that type in that group at that time
  ad0         <- multipleSNA(df=ad0,network.type=network.type)                  
  #summarise SNA measures across ego networks in that group at that time point
  ad0         <- summarySNA(sna.df=ad0)                                     #summary stats across networks
  #label the group, type of network and time point
  ad0$network <- paste("AD", network.type, "Time 0", sep=" ")
  #Time 1: Repeat for same group at next time point
  ad1         <- whichSubset(df=df,time=1,network=network.type, group="AD")
  ad1         <- multipleSNA(df=ad1,network.type=network.type)
  ad1         <- summarySNA(sna.df=ad1)
  ad1$network <- paste("AD", network.type, "Time 1", sep=" ")
  #Time 2
  ad2         <- whichSubset(df=df,time=2,network=network.type, group="AD")
  ad2         <- multipleSNA(df=ad2,network.type=network.type)
  ad2         <- summarySNA(sna.df=ad2)
  ad2$network <- paste("AD", network.type, "Time 2", sep=" ")
  #Time 3
  ad3         <- whichSubset(df=df,time=3,network=network.type, group="AD")
  ad3         <- multipleSNA(df=ad3,network.type=network.type)
  ad3         <- summarySNA(sna.df=ad3)
  ad3$network <- paste("AD", network.type, "Time 3", sep=" ")
  #Bind summaries from all 4 time points together into a single dataframe
  ad.sna.summary <- rbind(ad0,ad1,ad2,ad3)
  
  #Group EH - repeat as for group AD
  EH0         <- whichSubset(df=df,time=0,network=network.type, group="EH") #define subset
  EH0         <- multipleSNA(df=EH0,network.type=network.type)                  #specific network stats
  EH0         <- summarySNA(sna.df=EH0)                                     #summary stats across networks
  EH0$network <- paste("EH", network.type, "Time 0", sep=" ")                                        #label group of networks
  EH1         <- whichSubset(df=df,time=1,network=network.type, group="EH")
  EH1         <- multipleSNA(df=EH1,network.type=network.type)
  EH1         <- summarySNA(sna.df=EH1)
  EH1$network <- paste("EH", network.type, "Time 1", sep=" ")
  EH2         <- whichSubset(df=df,time=2,network=network.type, group="EH")
  EH2         <- multipleSNA(df=EH2,network.type=network.type)
  EH2         <- summarySNA(sna.df=EH2)
  EH2$network <- paste("EH", network.type, "Time 2", sep=" ")
  EH3         <- whichSubset(df=df,time=3,network=network.type, group="EH")
  EH3         <- multipleSNA(df=EH3,network.type=network.type)
  EH3         <- summarySNA(sna.df=EH3)
  EH3$network <- paste("EH", network.type, "Time 3", sep=" ")
  EH.sna.summary <- rbind(EH0,EH1,EH2,EH3)
  
  #And for group IO
  IO0         <- whichSubset(df=df,time=0,network=network.type, group="IO") #define subset
  IO0         <- multipleSNA(df=IO0,network.type=network.type)                  #specific network stats
  IO0         <- summarySNA(sna.df=IO0)                                     #summary stats across networks
  IO0$network <- paste("IO", network.type, "Time 0", sep=" ")                                        #label group of networks
  IO1         <- whichSubset(df=df,time=1,network=network.type, group="IO")
  IO1         <- multipleSNA(df=IO1,network.type=network.type)
  IO1         <- summarySNA(sna.df=IO1)
  IO1$network <- paste("IO", network.type, "Time 1", sep=" ")
  IO2         <- whichSubset(df=df,time=2,network=network.type, group="IO")
  IO2         <- multipleSNA(df=IO2,network.type=network.type)
  IO2         <- summarySNA(sna.df=IO2)
  IO2$network <- paste("IO", network.type, "Time 2", sep=" ")
  IO3         <- whichSubset(df=df,time=3,network=network.type, group="IO")
  IO3         <- multipleSNA(df=IO3,network.type=network.type)
  IO3         <- summarySNA(sna.df=IO3)
  IO3$network <- paste("IO", network.type, "Time 3", sep=" ")
  IO.sna.summary <- rbind(IO0,IO1,IO2,IO3)
  
  #Group PR
  PR0         <- whichSubset(df=df,time=0,network=network.type, group="PR") #define subset
  PR0         <- multipleSNA(df=PR0,network.type=network.type)                  #specific network stats
  PR0         <- summarySNA(sna.df=PR0)                                     #summary stats across networks
  PR0$network <- paste("PR", network.type, "Time 0", sep=" ")                                        #label group of networks
  PR1         <- whichSubset(df=df,time=1,network=network.type, group="PR")
  PR1         <- multipleSNA(df=PR1,network.type=network.type)
  PR1         <- summarySNA(sna.df=PR1)
  PR1$network <- paste("PR", network.type, "Time 1", sep=" ")
  PR2         <- whichSubset(df=df,time=2,network=network.type, group="PR")
  PR2         <- multipleSNA(df=PR2,network.type=network.type)
  PR2         <- summarySNA(sna.df=PR2)
  PR2$network <- paste("PR", network.type, "Time 2", sep=" ")
  PR3         <- whichSubset(df=df,time=3,network=network.type, group="PR")
  PR3         <- multipleSNA(df=PR3,network.type=network.type)
  PR3         <- summarySNA(sna.df=PR3)
  PR3$network <- paste("PR", network.type, "Time 3", sep=" ")
  PR.sna.summary <- rbind(PR0,PR1,PR2,PR3)
  
  #Group SY
  SY0         <- whichSubset(df=df,time=0,network=network.type, group="SY") #define subset
  SY0         <- multipleSNA(df=SY0,network.type=network.type)                  #specific network stats
  SY0         <- summarySNA(sna.df=SY0)                                     #summary stats across networks
  SY0$network <- paste("SY", network.type, "Time 0", sep=" ")                                        #label group of networks
  SY1         <- whichSubset(df=df,time=1,network=network.type, group="SY")
  SY1         <- multipleSNA(df=SY1,network.type=network.type)
  SY1         <- summarySNA(sna.df=SY1)
  SY1$network <- paste("SY", network.type, "Time 1", sep=" ")
  SY2         <- whichSubset(df=df,time=2,network=network.type, group="SY")
  SY2         <- multipleSNA(df=SY2,network.type=network.type)
  SY2         <- summarySNA(sna.df=SY2)
  SY2$network <- paste("SY", network.type, "Time 2", sep=" ")
  SY3         <- whichSubset(df=df,time=3,network=network.type, group="SY")
  SY3         <- multipleSNA(df=SY3,network.type=network.type)
  SY3         <- summarySNA(sna.df=SY3)
  SY3$network <- paste("SY", network.type, "Time 3", sep=" ")
  SY.sna.summary <- rbind(SY0,SY1,SY2,SY3)
  
  #Group Z
  Z0         <- whichSubset(df=df,time=0,network=network.type, group="Z")
  Z0         <- multipleSNA(df=Z0,network.type=network.type)                  
  #Z0         <- summarySNA(sna.df=Z0)             #ego=1 in group Z, so no summary stats                        
  Z0$network <- paste("Z", network.type, "Time 0", sep=" ")                                        
  Z1         <- whichSubset(df=df,time=1,network=network.type, group="Z")
  Z1         <- multipleSNA(df=Z1,network.type=network.type)
  #Z1         <- summarySNA(sna.df=Z1)
  Z1$network <- paste("Z", network.type, "Time 1", sep=" ")
  Z2         <- whichSubset(df=df,time=2,network=network.type, group="Z")
  Z2         <- multipleSNA(df=Z2,network.type=network.type)
  #Z2         <- summarySNA(sna.df=Z2)
  Z2$network <- paste("Z", network.type, "Time 2", sep=" ")
  #Participant Z got de-selected before Time 3
  #Z3         <- whichSubset(df=df,time=3,network=network.type, group="Z")
  #Z3         <- multipleSNA(df=Z3,network.type=network.type)
  #Z3         <- summarySNA(sna.df=Z3)
  #Z3$network <- paste("Z", network.type, "Time 3", sep=" ")
  Z.sna.summary                 <- rbind(Z0,Z1,Z2)
  
  #add columns to match other sna.summary datasets
  Z.sna.summary$size.range      <- rep("-", length(Z.sna.summary$egoid))
  Z.sna.summary$density.range   <- rep("-", length(Z.sna.summary$egoid))
  Z.sna.summary$i.density.range <- rep("-", length(Z.sna.summary$egoid))
  #since Z cannot be summarised, output is in different format. Need to add columns,re-order & re-name:
    #Names(SY.sna.summary) listed with corresponding z.sna column numbers
    # [1] "size"(4)         "size.range"(18)  [3] "density" (5)        "density.range"(19)  
    # [5] "i.density"       "i.density.range" [7] "between"         "hphy.age"[col.6 in Z]       
    # [9] "hphy.ethnicity"  "hphy.offence"    [11] "hphy.sentence"   "hphy.violence"  
    #[13] "hphy.status"      "hphy.drug"      [15] "hphy.engagement"(col.13) "network"(17) 
  col.index               <- c(4,18,5,19,14,20,15,6:13,17)
  Z.sna.summary           <-  Z.sna.summary[,col.index]
  colnames(Z.sna.summary) <- names(SY.sna.summary)
  
  #Convert mode of all variables to character so that factors don't lead to NA
  Z.sna.summary [,1:16]    <- as.data.frame(sapply(Z.sna.summary [,1:16], as.character))
  SY.sna.summary [,1:16]   <- as.data.frame(sapply(SY.sna.summary [,1:16], as.character))
  PR.sna.summary [,1:16]   <- as.data.frame(sapply(PR.sna.summary [,1:16], as.character))
  IO.sna.summary [,1:16]   <- as.data.frame(sapply(IO.sna.summary [,1:16], as.character))
  EH.sna.summary [,1:16]   <- as.data.frame(sapply(EH.sna.summary [,1:16], as.character))
  ad.sna.summary [,1:16]   <- as.data.frame(sapply(ad.sna.summary [,1:16], as.character))
  
  #Combine output from each group
  df.networks <- rbind(ad.sna.summary,EH.sna.summary,IO.sna.summary,PR.sna.summary,
                       SY.sna.summary,Z.sna.summary)
  
  return(df.networks)
}

#__________________________________________________________________________________________
#directedTies: Create reverse direction ties to allow directional plotting & analysis
#_____________________________________________________________________________________________
#Duplicates the edgelist with all ties represented in reverse order
# e.g. 1 - 9 is now copied and additional row addeed for 9 -1
#      the only actually directed ties = mentee/mentorship, so mentees/mentors are switched

directedTies <- function(ties.df){
  #ties.df<- ties
  
  #ensure that n1 and n2 are characters to allow data frame combinations
  ties.df$n1 <- as.character(ties.df$n1)
  ties.df$n2 <- as.character(ties.df$n2)
  
  #reverse order where n1=mentor so that n1 always = mentee
  ties.df$mentee    <- ties.df$n1_mentee
  mentee1           <- which(ties.df$n2_mentee==1)
  temp              <- ties.df[mentee1,]
  n1                <- temp$n1
  temp$n1           <- temp$n2 
  temp$n2           <- n1
  temp$mentee       <- rep(1, length(temp$egoid)) ##n1 mentor and n2 mentee is double
  ties.df[mentee1,] <- temp
  ties.df$mentor    <- rep(0, length(ties.df$mentee))
  
  #Keep original edgelist to add new one to at the end
  both.df           <- ties.df 
  both.df$direction <- rep(1, length(both.df$egoid))
  
  #Switch order of node ids
  original_n1     <- ties.df$n1
  original_n2     <- ties.df$n2
  ties.df$n1      <- original_n2
  ties.df$n2      <- original_n1
  
  #All nodes marked as "mentees" when n1 are now n2, and therefore mentors
  ties.df$mentor  <- ifelse(ties.df$mentee==1, 1,0)
  ties.df$mentee  <- ifelse(ties.df$mentor==1, 0,ties.df$mentee)  
  
  ties.df$direction <- rep(2, length(ties.df$n1))
  
  #Add new dataset to the original
  both.df         <-  rbind(ties.df,both.df)
  
  return(both.df)
  
}

#df<- t0.df
make_iNet <- function(df, network.type){
  #df<- t0.df
  
  #Create a dataframe of vertex data, extracting n1 data from df with ties in both direction
  #so that all alter-related variables correspond to the alter id and can be matched to vertex names
  #in the igraph object
  alter.index <- unique(c(df$n1,df$n2))
  alter.df    <- df[0,]
  for(i in alter.index){
    rows      <- which(df$n1==i)
    subset    <- df[rows,]
    alter.row <- subset[1,]
    alter.df  <- rbind(alter.df,alter.row)
  }  
  
  #Extract only columns relevant to alters, so that whole alter df can be merged with net vertex df
  #25=alterid,27=age to 37=engagement
  col.alterid      <- which(names(alter.df)=="alterid")
  col.age          <- which(names(alter.df)=="age")
  if(length(col.age)==0){col.age <- which(names(alter.df)=="age.x")}
  col.eng          <- which(names(alter.df)=="engagement")
  alter.index      <- c(col.alterid,col.age:col.eng) #column 23=alterid, column 35=engaged
  alter.df         <- alter.df[,alter.index]
  alter.df$alterid <- as.character(alter.df$alterid) #to allo matching to vertex names
  
  #Create igraph object: subset df to make n1 and n2 the first two columns
  #and to include edge information only on the relevant network
  
  #these are columns for ties: 6-assoc, friend, n1mentor,n1mentee,12-exposure
  edges.col <- which(names(df)=="friends") 
  
  #3,4=n1,n2; 1=egoid,53=e.initial,14=ties, 3,4=backup node ids
  c1 <- which(names(df)=="egoid")
  c2 <- which(names(df)=="n1")
  c3 <- which(names(df)=="n2")
  
  #Note: egoid cannot be first column; must lead with n1 and n2 and then edge strength
  #also note that ties should entered in one direction only
  col.index     <- c(c2,c3,edges.col,c1)
  one.direction <- which(df$direction==1)
  one.direction <- df[one.direction,]     #remove direction 2
  net.df      <- one.direction[,col.index]
  
  #Turn data frame into an igraph object
  net         <- graph.data.frame(net.df, directed=FALSE)
  #net         <- graph.edgelist(net.df,mode="undirected",diag=FALSE)
  
  #Match edges to vertices
  V(net)$friends         <- as.character(df$friends[match(V(net)$name,df$n1)])
  
  #Match alter.df to vertecies
  V(net)$nationality    <- as.character(alter.df$nationality[match(V(net)$name,alter.df$alterid)])
  V(net)$ethnicity      <- as.character(alter.df$ethnicity[match(V(net)$name,alter.df$alterid)])
  V(net)$age            <- as.character(alter.df$age[match(V(net)$name,alter.df$alterid)])
  V(net)$age.cat        <- ifelse(V(net)$age<30, 1, 0)
  V(net)$age.cat        <- ifelse(V(net)$age>=30 & V(net)$age<=40, 2, V(net)$age.cat)
  V(net)$age.cat        <- ifelse(V(net)$age> 40, 3, V(net)$age.cat)
  V(net)$home           <- as.character(alter.df$neighbourhood[match(V(net)$name,alter.df$alterid)])
  V(net)$offence        <- as.character(alter.df$offence[match(V(net)$name,alter.df$alterid)])
  V(net)$prison         <- as.character(alter.df$prison.history[match(V(net)$name,alter.df$alterid)])
  V(net)$sentence       <- as.character(alter.df$sentence[match(V(net)$name,alter.df$alterid)])
  V(net)$violence       <- as.character(alter.df$violence[match(V(net)$name,alter.df$alterid)])
  V(net)$status         <- as.character(alter.df$status[match(V(net)$name,alter.df$alterid)])
  V(net)$status         <- ifelse(V(net)$status=="very high", "high",V(net)$status )
  V(net)$status         <- ifelse(V(net)$status=="", "ego",V(net)$status )
  V(net)$drug           <- as.character(alter.df$drug[match(V(net)$name,alter.df$alterid)])
  V(net)$engagement     <- as.character(alter.df$engagement[match(V(net)$name,alter.df$alterid)])
  V(net)$eng.label      <- ifelse(V(net)$engagement=="engaged", "E","I")
  V(net)$eng.label      <- ifelse(V(net)$engagement=="nonprisoner", "NP",V(net)$eng.label)
  
  return(net)
}


#_________________________________________________________________________________________________________________
#plotEdge Specify plotting parameters for edges
#_________________________________________________________________________________________________________________
#net<- t0.net
plotEdge <- function(net, thickness, colour, network.type, extra){
  require(RColorBrewer)
  
  if(missing(network.type)){network.type <- "all"}
  if(missing(colour))      {colour       <- "type"}
  if(missing(thickness))   {thickness    <- 1}
  if(missing(extra)) {extra <- "all"}
  
  #Assign tie type, where 1=conflict, 2=positive exposure, 3=associate, 4=mentee,5=friend
  E(net)$edge.type <- E(net)$friends
  
  if(thickness=="type"){
    E(net)$width <- ifelse(E(net)$edge.type==1, 0.2,   0)
    E(net)$width <- ifelse(E(net)$edge.type==2, 1,     E(net)$width)
    E(net)$width <- ifelse(E(net)$edge.type==3, 2,   E(net)$width)
    E(net)$width <- ifelse(E(net)$edge.type==4, 0.2,   E(net)$width)
    #     E(net)$width <- ifelse(E(net)$edge.type==5, 2, E(net)$width)
  }
  
  if(network.type!="all"){
    E(net)$width <- ifelse(E(net)$edge.type!=network.type, 0, 1)
  }
  
  if(colour=="type"){
    #Assign tie type, where 1=conflict, 2=positive exposure, 3=associate, 4=mentee,5=friend  
    E(net)$color <- ifelse(E(net)$edge.type==1, "grey", "white")
    E(net)$color <- ifelse(E(net)$edge.type==2, "black", E(net)$color)
    E(net)$color <- ifelse(E(net)$edge.type==3, "black", E(net)$color)
    E(net)$color <- ifelse(E(net)$edge.type==4, "blue", E(net)$color)
    #E(net)$color <- ifelse(E(net)$edge.type==5, "black", E(net)$color)
    
  }else{
    if(colour=="type.ramp"){
      max.x           <- 4
      col.fun         <- E(net)$edge.type/max.x
      col.types       <- colorRamp(c("red","blue"))
      col.scale       <- col.types(col.fun) 
      colours         <- rgb(col.scale, maxColorValue=256)
      E(net)$color  <- colours 
    }
    if(colour=="friend"){
      E(net)$color <- ifelse(E(net)$edge.type==3, "black", "white")
      E(net)$color <- ifelse(E(net)$edge.type==2, "grey", E(net)$color)
    }
  }
  
  if(extra=="friends only"){
    E(net)$width <- ifelse(E(net)$edge.type==1, 1,   0)
    E(net)$width <- ifelse(E(net)$edge.type==2, 1.5,   E(net)$width)
    E(net)$width <- ifelse(E(net)$edge.type==3, 2,   E(net)$width)
    E(net)$width <- ifelse(E(net)$edge.type==4, 0,   E(net)$width)
    E(net)$color <- ifelse(E(net)$edge.type==3, "black", "white")
    E(net)$color <- ifelse(E(net)$edge.type==2, "black", E(net)$color)
    E(net)$color <- ifelse(E(net)$edge.type==1, "grey",  E(net)$color)

  }
  
  return(net)
  
  
}
# #_____________________________________________________________________________________________
# #make_iNet Turn a network into an igraph object
# #_____________________________________________________________________________________________
# #df<- t0.df#; network.type="friend"
# #Function applies to a single network (use whichSubset function to define)
# make_iNet <- function(df, network.type){
#   #df<- t0.df
# 
#   #Create a dataframe of vertex data, extracting n1 data from df with ties in both direction
#   #so that all alter-related variables correspond to the alter id and can be matched to vertex names
#   #in the igraph object
#   alter.index <- unique(c(df$n1,df$n2))
#   alter.df    <- df[0,]
#   for(i in alter.index){
#     rows      <- which(df$n1==i)
#     subset    <- df[rows,]
#     alter.row <- subset[1,]
#     alter.df  <- rbind(alter.df,alter.row)
#   }  
#   
#   #Extract only columns relevant to alters, so that whole alter df can be merged with net vertex df
#   #25=alterid,27=age to 37=engagement
#   col.alterid      <- which(names(alter.df)=="alterid")
#   col.age          <- which(names(alter.df)=="age")
#   col.eng          <- which(names(alter.df)=="engagement")
#   alter.index      <- c(col.alterid,col.age:col.eng) #column 23=alterid, column 35=engaged
#   alter.df         <- alter.df[,alter.index]
#   alter.df$alterid <- as.character(alter.df$alterid) #to allo matching to vertex names
# 
#   #Create igraph object: subset df to make n1 and n2 the first two columns
#   #and to include edge information only on the relevant network
# 
#   #these are columns for ties: 6-assoc, friend, n1mentor,n1mentee,12-exposure
#   edges.col  <- c(6,7,8,10,12) 
# 
#   #3,4=n1,n2; 1=egoid,53=e.initial,14=ties, 3,4=backup node ids
#   col.index   <- c(3,4,edges.col,1,53,14,3,4) 
#   #col.index   <- c(3,4,3,4)
#   net.df      <- df[,col.index]
#   
#   
#   #identify which rows=mentors and delete those so that tie = one direction
# #   mentors <- which(net.df$n1_mentor==1)
# #   net.df  <- net.df[-mentors,]
#   
#   #Turn data frame into an igraph object
#   net         <- graph.data.frame(net.df, directed=FALSE)
#   #net         <- graph.edgelist(net.df,mode="undirected",diag=FALSE)
# 
#   #Match edges to vertices
#   V(net)$friend         <- as.character(df$friend[match(V(net)$name,df$n1)])
#   V(net)$associate      <- as.character(df$associate[match(V(net)$name,df$n1)])
#   V(net)$exposure       <- as.character(df$exposure[match(V(net)$name,df$n1)])
#   V(net)$past.ties      <- as.character(df$ties[match(V(net)$name,df$n1)])
#   
#   #Match alter.df to vertecies
#   V(net)$nationality    <- as.character(alter.df$nationality[match(V(net)$name,alter.df$alterid)])
#   V(net)$ethnicity      <- as.character(alter.df$ethnicity[match(V(net)$name,alter.df$alterid)])
#   V(net)$age            <- as.character(alter.df$age[match(V(net)$name,alter.df$alterid)])
#   V(net)$age.cat        <- ifelse(V(net)$age<30, 1, 0)
#   V(net)$age.cat        <- ifelse(V(net)$age>=30 & V(net)$age<=40, 2, V(net)$age.cat)
#   V(net)$age.cat        <- ifelse(V(net)$age> 40, 3, V(net)$age.cat)
#   V(net)$home           <- as.character(alter.df$neighbourhood[match(V(net)$name,alter.df$alterid)])
#   V(net)$offence        <- as.character(alter.df$offence[match(V(net)$name,alter.df$alterid)])
#   V(net)$prison         <- as.character(alter.df$prison.history[match(V(net)$name,alter.df$alterid)])
#   V(net)$sentence       <- as.character(alter.df$sentence[match(V(net)$name,alter.df$alterid)])
#   V(net)$violence       <- as.character(alter.df$violence[match(V(net)$name,alter.df$alterid)])
#   V(net)$status         <- as.character(alter.df$status[match(V(net)$name,alter.df$alterid)])
#   V(net)$status         <- ifelse(V(net)$status=="very high", "high",V(net)$status )
#   V(net)$status         <- ifelse(V(net)$status=="", "ego",V(net)$status )
#   V(net)$drug           <- as.character(alter.df$drug[match(V(net)$name,alter.df$alterid)])
#   V(net)$engagement     <- as.character(alter.df$engagement[match(V(net)$name,alter.df$alterid)])
#   V(net)$eng.label      <- ifelse(V(net)$engagement=="engaged", "E","I")
#   V(net)$eng.label      <- ifelse(V(net)$engagement=="nonprisoner", "NP",V(net)$eng.label)
#   
#   return(net)
# }
# 
# 
# #_________________________________________________________________________________________________________________
# #plotEdge Specify plotting parameters for edges
# #_________________________________________________________________________________________________________________
# #net<- t0.net
# plotEdge <- function(net, thickness, colour, network.type){
#   require(RColorBrewer)
#   
#   if(missing(network.type)){network.type <- "all"}
#   if(missing(colour))      {colour       <- "type"}
#   if(missing(thickness))   {thickness    <- 1}
#   
#   #Assign tie type, where 1=conflict, 2=positive exposure, 3=associate, 4=mentee,5=friend
#   E(net)$edge.type <- 0
#   E(net)$edge.type[E(net)$friend==1] <- 5  
#   E(net)$edge.type  <- ifelse(E(net)$exposure==-1, 1,E(net)$edge.type)
#   E(net)$edge.type  <- ifelse(E(net)$exposure==1,  2,E(net)$edge.type)
#   E(net)$edge.type  <- ifelse(E(net)$associate==1, 3,E(net)$edge.type)
#   E(net)$edge.type  <- ifelse(E(net)$n1_mentee==1 | E(net)$n1_mentor==1, 4,E(net)$edge.type)
#   E(net)$edge.type  <- ifelse(E(net)$friend==1,    5,E(net)$edge.type) #repeat in case of multiplexity
#   
#   if(thickness=="type"){
#     E(net)$width <- ifelse(E(net)$edge.type==1, 0.5, 0)
#     E(net)$width <- ifelse(E(net)$edge.type==2, 0.2, E(net)$width)
#     E(net)$width <- ifelse(E(net)$edge.type==3, 1, E(net)$width)
#     E(net)$width <- ifelse(E(net)$edge.type==4, 1.5, E(net)$width)
#     E(net)$width <- ifelse(E(net)$edge.type==5, 2, E(net)$width)
#   }
#   
#   if(network.type!="all"){
#     E(net)$width <- ifelse(E(net)$edge.type!=network.type, 0, 1)
#   }
# 
#   if(colour=="type"){
#     #Assign tie type, where 1=conflict, 2=positive exposure, 3=associate, 4=mentee,5=friend  
#     E(net)$color <- ifelse(E(net)$edge.type==1, "red", "white")
#     E(net)$color <- ifelse(E(net)$edge.type==2, "blue", E(net)$color)
#     E(net)$color <- ifelse(E(net)$edge.type==3, "grey", E(net)$color)
#     E(net)$color <- ifelse(E(net)$edge.type==4, "purple", E(net)$color)
#     E(net)$color <- ifelse(E(net)$edge.type==5, "black", E(net)$color)
#     
#   }else{
#         if(colour=="type.ramp"){
#         max.x           <- as.numeric(max(E(net)$edge.type))
#         col.fun         <- E(net)$edge.type/max.x
#         col.types       <- colorRamp(c("red","blue"))
#         col.scale       <- col.types(col.fun) 
#         colours         <- rgb(col.scale, maxColorValue=256)
#         E(net)$color  <- colours 
#         }
#         if(colour=="friend"){
#           E(net)$color <- ifelse(E(net)$edge.type==5, "black", "white")
#           E(net)$color <- ifelse(E(net)$edge.type==2, "grey", E(net)$color)
#         }
#   }
#   
#   return(net)
#   
#   
# }
#_____________________________________________________________________________________________
#Specify Plotting parameters for vertices
#_____________________________________________________________________________________________
#net <- t0.net#; label<- V(net)$eng.label
#shape <- "engagement
plotVertex <- function(net, colour, shape, label, node.names){
  
  #set ego to true, ie ego node will be coloured differently
  if(missing(shape)){shape <- "circle"}
  if(missing(node.names)){node.names <- FALSE}
  
  #Set default parameters for vertex labels
  V(net)$label.color="black"  #colour
  V(net)$label.cex=1          #size
  V(net)$label.font=1         #serif 
  
  #Set actual lables
  if(missing(label)){label <- V(net)$eng.label}  
  V(net)$label             <- label
  V(net)$label[V(net)$status=="ego"]<- paste(V(net)$eng.label[V(net)$status=="ego"],"Ego", sep=".")
  
  if(node.names==TRUE){
    V(net)$label <- paste(V(net)$label,V(net)$name, sep=".")
  }
  
  
  #Set shapes for vertices 
  #call TriangleShape function to add 'triangle' as optional vertex shape
  add.vertex.shape("triangle", clip=igraph.shape.noclip,plot=TriangleShape)
  
  if (shape=="engagement"){
    V(net)$shape[V(net)$engagement=="instrumental"] <- "square"    
    V(net)$shape[V(net)$engagement=="engaged"]      <- "circle" 
    V(net)$shape[V(net)$engagement=="Unknown"|V(net)$engagement=="unknown"]      <- "triangle"
    V(net)$shape[V(net)$engagement=="nonprisoner"]  <- "triangle"
    V(net)$shape[V(net)$engagement==""]              <- "triangle"
    if(length(V(net)$shape[V(net)$engagement=="unknown"])!=0){
      V(net)$size[V(net)$engagement=="unknown"]       <- 2*(V(net)$size[V(net)$engagement=="unknown"]) 
    }
    
  }else{
    
    if (shape=="status"){  
      V(net)$shape[V(net)$status=="high"]   <- "square"    
      V(net)$shape[V(net)$status=="medium"] <- "circle" 
      V(net)$shape[V(net)$status=="low"]    <- "triangle"
      if(length(V(net)$shape[V(net)$status=="low"])!=0){
        V(net)$size[V(net)$status=="low"]     <- 2*(V(net)$size[V(net)$type==3]) 
      }
    }}
  
  
  #Set colour parameters
  V(net)$frame.color <- "black"
  
  if(missing(colour)){
    V(net)$color="slategray1"
    V(net)$color[V(net)$status=="ego"]="slategray"
    
  }else{
    if(colour=="age"){
      x               <- as.numeric(V(net)$age)
      max.x           <- as.numeric(max(V(net)$age))
      min.x           <- as.numeric(min(V(net)$age))
      #col.fun         <- (x-min.x)/(max.x-min.x)
      col.fun         <- V(net)$age/max.x
      col.types       <- colorRamp(c("blue","red"))
      col.scale       <- col.types(col.fun) 
      colours         <- rgb(col.scale, maxColorValue=256)
      V(net)$color    <- colours 
      V(net)$color[V(net)$status=="ego"] <- "white"
      
    }else{
      if(colour=="engagement"){
        V(net)$color[V(net)$engagement=="engaged"]      <- "seashell4"
        V(net)$color[V(net)$engagement=="instrumental"] <- "seashell1"  #"red4" purple or deeppink4
        V(net)$frame.color[V(net)$status=="ego"]        <- "red4"
        
      }else{
        if(colour=="ethnicity"){
          V(net)$color[V(net)$ethnicity=="White"]      <- "white"
          V(net)$color[V(net)$ethnicity=="Traveller"]  <- "seashell1"
          V(net)$color[V(net)$ethnicity=="Black"]      <- "seashell4"
          V(net)$color[V(net)$ethnicity=="Asian"]      <- "seashell3"
          V(net)$color[V(net)$ethnicity=="Mixed"]      <- "seashell2" #mediumpurple2"
          V(net)$frame.color[V(net)$status=="ego"]     <- "red4"   #to ID ego
          
        }else{
          if(colour=="status"){
            V(net)$color                           <- "white"
            V(net)$color[V(net)$status=="high"]    <- "seashell3"
            V(net)$color[V(net)$status=="medium"]  <- "seashell2" #mediumpurple2
            V(net)$color[V(net)$status=="low"]     <- "seashell1"
            V(net)$color[V(net)$status=="ego"]     <- "seashell4"
            V(net)$frame.color[V(net)$status=="ego"] <- "red4"
            
          }else{
            if(colour=="drug"){
              V(net)$color[V(net)$drug=="Heroin"]      <- "seashell2"
              V(net)$color[V(net)$drug=="Crack"]       <- "seashell3" #mediumpurple2
              V(net)$color[V(net)$drug=="None"]        <- "white"
              V(net)$frame.color[V(net)$status=="ego"] <- "red4"
            }
            
          }}}}}
  
  return(net)  
}

#_________________________________________________________________________________________________________________
# TriangleShape : Function to create triangular vetrexes in igraph
#_________________________________________________________________________________________________________________
#creates triangle shape for igraph and assigns it to available shapes in igraph, called with "triangle"
#Adapted from: www.pastebin.com/XiTWiHpL 'Custom polygon vertex shape for igraph' by Deenes, 2nd June 2013

TriangleShape <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/100 * params("vertex", "size")         #original 1/300 = smaller than standard
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  vertex.frame.color <- params("vertex", "frame.color")   #set border colour to colour specified for vertex frame
  if (length(vertex.frame.color) != 1 && !is.null(v)) {
    vertex.frame.color <- vertex.frame.color[v]
  }
  for(i in seq(1,dim(coords)[1])){
    xx <- c(
      coords[i,1],
      coords[i,1]-0.8*vertex.size[i],
      coords[i,1]+0.8*vertex.size[i],
      coords[i,1])
    yy <- c(
      coords[i,2]+0.4*vertex.size[i],
      coords[i,2]-0.8*vertex.size[i],
      coords[i,2]-0.8*vertex.size[i],
      coords[i,2]+0.4*vertex.size[i])
    polygon(x=xx,y=yy,col=vertex.color[i],border=vertex.frame.color) #
  }
}

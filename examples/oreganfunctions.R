#Spatial decay function - probability that node j infects node i
decay<-function(x,mass,tau,beta0,beta1,beta2,beta3){
  return(tmp=1/(1+exp(beta0 +beta1*tau + (beta2*x)/(mass**beta3))))
}

getweights<-function(dist,mass,tau,beta){
  mass=mass%*%t(mass)
  tau=matrix(tau,ncol=length(tau),nrow=length(tau),byrow=TRUE)
  ret=decay(dist,mass,tau,beta[1],beta[2],beta[3],beta[4])
  diag(ret)<-0
  return(ret)
}

#returns probabilities that each node will be infected in the next time step
getprobs<-function(start,dist,mass,tau,b){
  
  #Check Trivial Cases
  if(all(start)){
    return(rep(1,length(start)))
  }
  if(!any(start)){
    return(rep(0,length(start)))
  }
  
  #calculate probability of infection from each infected node node to each uninfected node
  mass=mass%*%t(mass)
  tmp=decay(dist[which(start),which(!start)],mass[which(start),which(!start)], (rep(1,length(which(start))) %*% t(tau[which(!start)])), b[1],b[2],b[3],b[4])
  tmp=tmp*(dist[which(start),which(!start)] != 0)
  
  #combine probabilities of infected nodes for each uninfected node
  if(!is.null(dim(tmp))){
    tmp=log(1-tmp)
    tmp=rep(1,dim(tmp)[1]) %*% tmp
    tmp=1-exp(tmp[1,])
  }else if(sum(!start)==1){
    tmp=1-exp(sum(log(1-tmp)))
  }
  
  #construct return vector
  ret=vector(length=length(start))
  ret[which(start)]=1
  ret[which(!start)]=tmp
  
  return(ret)
}

#calculate log likelihood for a single timestep
lglike<-function(p,d){
  for(i in 1:length(d)){
    if(!d[i]){
      p[i]=1-p[i]
    }
  }
  return(sum(log(p)))
}

#simulates infection spread over multiple years from a given starting point
simforeward<-function(dist, mass, tau, beta, start, dur, first, last){
  ret=start
  for(i in first:last){
    #if all nodes are infected stop the simulation
    if(all(ret!=Inf)){
      return(ret)
    }
    
    recovered=which((ret+dur)<i)
    dist[recovered,]<-0
    dist[,recovered]<-0
    
    #calculate probabilities of infection
    p=getprobs((ret<i),dist,mass,tau,beta)
    for(j in 1:length(ret)){
      #infect nodes with calculated probabilities
      if(start[j] == Inf & p[j] > runif(1,0,1)){
        ret[j] = i
      }
    }
    start=ret
  }
  return(ret)
}

#plot node locations
plotcoords<-function(coords,data=0){
  if(sum(data)){
    plot(x=coords[1,which(as.logical(data))],y=coords[2,which(as.logical(data))],xlim=c(min(coords[1,]),max(coords[1,])),ylim=c(min(coords[2,]),max(coords[2,])), xlab='', ylab='',col='red')
    points(x=coords[1,which(!as.logical(data))],y=coords[2,which(!as.logical(data))],col='blue')
  }else{
    plot(x=coords[1,],y=coords[2,],xlab='',ylab='')
  }
}

#add node locations to existing plot
addnodes<-function(coords,data=0){
  if(sum(data)){
    points(x=coords[1,which(!data)],y=coords[2,which(!as.logical(data))],col='blue')
    points(x=coords[1,which(data)],y=coords[2,which(as.logical(data))],col='red')
  }else{
    points(x=coords[1,],y=coords[2,])
  }
}

#plot connectivity network
plotconnect<-function(coords,dist,mass,beta,add=FALSE){
  n=dim(coords)[2]
  mass=mass%*%t(mass)
  
  if(!add){
    plot(c(0,max(coords)),c(0,max(coords)),col='white', xlab='', ylab='')
  }
  
  for(i in 2:n){
    for(j in 1:(i-1)){
      if(dist[i,j] != 0){
        lines(coords[1,c(i,j)],coords[2,c(i,j)],lwd=5*decay(dist[i,j],mass[i,j],beta[1],beta[2],beta[3]))
      }
    }
  }
}

#plot spread of infection over time
plotspread<-function(coords,data,add=FALSE){
  y=max(data[which(data!=Inf)])+1
  
  if(!add){
    plot(c(0,max(coords)),c(0,max(coords)),col='white', xlab='', ylab='')
  }
  
  for(i in 1:y){
    addnodes(coords,(data<i))
    readline("Press <Enter> to continue")
  }
}

#plot spread w three classes
plotsir<-function(coords,wns,dur){
  plotcoords(c)
  rec=wns+dur
  for(i in 0:max(rec[which(rec!=Inf)])){
    points(x=coords[1,which(i<wns)],y=coords[2,which(i<wns)],col='green')
    points(x=coords[1,which((i>=wns) & (i<rec))],y=coords[2,which((i>=wns) & (i<rec))],col='red')
    points(x=coords[1,which(i>=rec)],y=coords[2,which(i>=rec)],col='blue')
    readline("Press <Enter> to continue")
  }
}


#creates a sparse distance matrix
makedist<-function(coords, cutoff=Inf){
  dmat=sqrt(((coords[1,] %*% t(rep(1,length(coords[1,])))) - (rep(1,length(coords[1,])) %*% t(coords[1,])))**2 + ((coords[2,] %*% t(rep(1,length(coords[2,])))) - (rep(1,length(coords[2,])) %*% t(coords[2,])))**2)
  
  dmat=dmat*(dmat<cutoff)
  
  return(matrix(dmat))
}

#calculates the negative log likelihood over all years (objective function for optimization)
objf<-function(beta, dist, mass, tau, data, dur, years){
  ret=0
  
  for(i in 1:(years)){
    recovered=which((data+dur)<i)
    dist[recovered,]<-0
    dist[,recovered]<-0
    p=getprobs((data<i),dist,mass,tau,beta)
    ret=ret+lglike(p,(data<(i+1)))
  }
  
  return(-ret)
}

#find the best fit for beta by minimizing negative log likelihood
getbeta<-function(dist,mass,tau,data,dur,start=c(10,-.01,.01,.1),years=max(data[which(data!=Inf)])){
  ret=optim(par=start,fn=objf,dist=dist, mass=mass, tau=tau, data=data, dur=dur, years=years)
  return(ret)
}

##
getdelta<-function(beta, dist, mass, tau, data, dur, years=max(data[which(data!=Inf)])){
  require(mvtnorm)
  require(numDeriv)
  
  #Estimate Hessian at mle
  hess=hessian(objf, beta, dist=dist, mass=mass, tau=tau, data=data, dur=dur, years=years)
  #Invert Hessian to get Variance-Covariance Matrix
  A=solve(hess,diag(rep(1,length(beta))))
  #Calculate the 95th equicoordinate quantile
  q=qmvnorm(.95,mean=beta,sigma=A,tail='both',interval=c(5,15))$quantile
  #Get bounds
  ret=q*sqrt(diag(A))/length(beta)
  #Add names
  names=vector()
  for(i in 0:(length(beta)-1)){
    names=c(names,paste('delta',i,sep=''))
  }
  names(ret)<-names
  return(ret)
}

#Samples Beta values assuming a gaussian distrobution
frag_samp<-function(dist, mass, tau, data, dur, years=max(data[which(data!=Inf)]), ndraws=1000, beta=c(10.30425552, -0.03080282, 0.03506665, 0.26193472), seed=748){
  require(mvtnorm)
  require(numDeriv)
  set.seed(seed)
  
  #Estimate Hessian at mle
  hess=hessian(objf, beta, dist=dist, mass=mass, tau=tau, data=data, dur=dur, years=years)
  #Invert Hessian to get Variance-Covariance Matrix
  A=solve(hess,diag(c(1,1,1,1)))
  #Sample beta values
  ret=rmvnorm(ndraws,beta,A)
  colnames(ret)<-c('beta0','beta1','beta2','beta3')
  return(ret)
}

#Simulate multiple runs
makesims<-function(cc, dist, beta, first=0, last=50, nsims=1000, seed=802){
  set.seed(seed)
  ret=cc$FIPS
  names='FIPS'
  start=cc$WNS
  dur=cc$dur
  start[which(start>first)]<-Inf
  for(i in 1:nsims){
    add=simforeward(dist=dist,mass=cc$caves,tau=cc$tau,beta=unlist(beta[i,]),start=start,dur=dur,first=(first+1),last=last)
    ret=cbind(ret,add)
    names=c(names,paste('Sim',i,sep=''))
    print(i)
  }
  colnames(ret)<-names
  return(ret)
}

#creates boxplots of infection time
gofPlot<-function(sims,wns,order=NULL,maxyear=100,main='GOF GravTemp'){
  d=dim(sims)
  d[2]=d[2]-1  #less 1 for FIPS column
  
  #calculate quantiles
  q=apply(sims[,2:d[2]],1,quantile,probs=c(.025,.25,.5,.75,.975))
  
  #default ordering
  if(is.null(order)){
    order=order(q[3,],wns)
  }
  
  #sort counties
  q=q[,order]
  
  #change infinite values
  q[which(q==Inf)]=maxyear
  
  #creat plot
  plot(NA,xlim=c(0,d[1]),ylim=c(0,maxyear),xlab='County',ylab='Year of Infection',main=main)
  for(i in 1:d[1]){
    rect((i-1),q[1,i],i,q[5,i],col=grey(rep(.75,d[1]))) # 95% confidance interval
    rect((i-1),q[2,i],i,q[4,i],col=grey(rep(.25,d[1]))) # interquartile range
    rect((i-1),q[3,i],i,q[3,i],col=grey(rep(.95,d[1]))) # median
  }
  # add actual infection dates
  lines((1:d[1])-.5,wns[order],col='red',type='p')
  lines((1:d[1])-.5,wns[order],col='red',type='p',pch='*')
}

#track fragmentation of epidemic
trackfrag<-function(wns, mask, dur, fips, mode='strong', years=max((wns+dur)[which((wns+dur)!=Inf)])){
  require(igraph)
  
  S=list()
  I=list()
  R=list()
  for(i in 0:(years)){
    S[[i+1]]<-clusters(graph.adjacency(mask[which(wns>i),which(wns>i)]), mode=mode) 
    S[[i+1]]$FIPS=fips[which(wns>i)]
    I[[i+1]]<-clusters(graph.adjacency(mask[which(wns<=i & (wns+dur)>i),which(wns<=i & (wns+dur)>i)]), mode=mode)
    I[[i+1]]$FIPS=fips[which(wns<=i & (wns+dur)>i)]
    R[[i+1]]<-clusters(graph.adjacency(mask[which((wns+dur)<=i),which((wns+dur)<=i)]), mode=mode)
    R[[i+1]]$FIPS=fips[which((wns+dur)<=i)]
  }
  return(list(S=S,I=I,R=R))
}

cumI<-function(sim, years, dur){
  ret=vector()
  for(i in 1:(years+1)){
    ret[i]<-sum((sim<i) & ((sim+dur)>i))
  }
  return(ret)
}

newI<-function(sim, years){
  ret=vector()
  for(i in 0:years){
    ret[i+1]<-sum(sim==i)
  }
  return(ret)
}

median_dist<-function(sim, years, dur, distvec){
  ret=vector()
  for(i in 1:(years+1)){
    ret[i]<-median(distvec[which((sim<i) & ((sim+dur)>i))])
  }
  return(ret)
}

max_dist<-function(sim, years, dur, distvec){
  ret=vector()
  for(i in 1:(years+1)){
    ret[i]<-max(distvec[which((sim<i) & ((sim+dur)>i))])
    #ifelse(max(distvec[which((sim<i) & ((sim+dur)>i))])!=Inf,max(distvec[which((sim<i) & ((sim+dur)>i))]),ret[i-1])  
  } 
  return(ret)
}

plot_newI<-function(sims, years){
  infcount=apply(sims[,-1], 2, newI, years=years)
  
  epi=apply(infcount,1,mean)
  bounds=apply(infcount,1,quantile,probs=c(.025,.975))
  plot(NA, xlab='t', ylab='New Infections', xlim=c(0,years), ylim=c(0,max(bounds)))
  lines(x=0:years, y=epi, type='l')
  lines(x=0:years, y=bounds[1,], type='l', lty=2)
  lines(x=0:years, y=bounds[2,], type='l', lty=2)
}

plot_meddist<-function(sims, years, dur, distvec){
  md=apply(sims[,-1],2,median_dist,years=years, dur=dur, distvec=distvec)
  
  median_dist=apply(md,1,mean)
  bounds=apply(md,1,quantile,probs=c(.025,.975))
  plot(NA,xlab='t',ylab='Median Distance from Epicenter',xlim=c(0,years),ylim=c(0,max(bounds)))
  lines(x=0:years, y=median_dist, type='l')
  lines(x=0:years, y=bounds[1,], type='l', lty=2)
  lines(x=0:years, y=bounds[2,], type='l', lty=2)
} 

plot_maxdist<-function(sims, years, dur, distvec){
  md=apply(sims[,-1],2,max_dist,years=years, dur=dur, distvec=distvec)
  
  max_dist=apply(md,1,mean)
  bounds=apply(md,1,quantile,probs=c(.025,.975))
  plot(NA,xlab='t',ylab='Maximum Distance from Epicenter',xlim=c(0,years),ylim=c(0,max(bounds)))
  lines(x=0:years, y=max_dist, type='l')
  lines(x=0:years, y=bounds[1,], type='l', lty=2)
  lines(x=0:years, y=bounds[2,], type='l', lty=2)
} 

plot_cumI<-function(sims,years,dur){
  infcount=apply(sims[,-1],2,cumI,years=years,dur=dur)
  
  epi=apply(infcount,1,mean)
  bounds=apply(infcount,1,quantile,probs=c(.025,.975))
  plot(NA,xlab='t',ylab='I',xlim=c(0,years),ylim=c(0,max(bounds)))
  lines(x=0:years, y=epi,type='l')
  lines(x=0:years, y=bounds[1,],type='l',lty=2)
  lines(x=0:years, y=bounds[2,],type='l',lty=2)
}
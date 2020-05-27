## Runable script of O'Reagan paper
#setwd("~/Desktop/Pd_Geocache-master 4/examples/ORegan-2016-resources")
#Begin with importing data and basic model 
##cave dataset
nypa_data<-read.csv("NYPA_caves_infections.csv",header=TRUE)

##cave dataset from which cumulative number of cases is calculated:
nypa_data[which(apply(nypa_data[,2:7]>0,1,sum)>1),]

#cumulative number of cases in previous year (predictor) (These are set to 1 instead of 0 for starting values in order for
#identity poisson model to work... ie 0*Z wont work but 1*Z will)
Y<-c(1,2,5,5,1,2,3,1,1,1,5,1,4,6,13,13,1,1,1,1,10,12,1,1,2);
#number of new cases (response)
incidence<-c(2,3,0,0,2,1,1,1,2,5,1,4,2,7,0,1,1,0,2,10,2,0,1,1,0);
data<-data.frame(Y=Y,Z=incidence); #the correct place to put the data
#fit glm model to data
model<-glm(Z ~ Y - 1 ,data=data,family=poisson(link="identity")) #p-value <0.001
summary(model) #questionable number of residuals
#intrinsic growth rate of infection
lambda=as.numeric(model$coefficients) #.5212
lwr.lambda<-confint(model)[1]
upr.lambda<-confint(model)[2]
#seeing all the lambda's 
gamma=1/3; #infectious period
R0=(lambda/gamma)+1 #basic reproduction number of 2.56


x11(height=4, width=4)

plot(data$Y,data$Z,xlab="cumulative number of infected caves",ylab="number of new caves", pch=19, lwd=2, col=colors()[89], bty='l', las=1)
#T=seq(0,length(Y),by=.1)
#lines(T,1+lambda*T, lwd=2,col='brown' )
abline(a=1, b=lambda, col='brown',lwd=2)
title(substitute(paste("Exponential growth rate", ~lambda," = ",lambdaval), list(lambdaval=round(lambda,2))))
#Kind of a silly plot

library(ggplot2); theme_set(theme_bw())
(ggplot(data, aes(Y,Z))
  + stat_sum()
  +  geom_smooth(method=glm,
                 method.args=list(family=quasipoisson(link="identity")),
                 formula=y~x-1)
  + labs(x="previous cumulative incidence",
         y="incidence")
)
#Plot shows its obviously not fitting data as nicely as it should be

plot(Z~Y,data=data)


#now look at the outbreak data 
infectious<-c(2,5,5,5,2,3,4,1,3,5,6,4,6,13,13,14,1,1,3,10,12,12,1,2,2); 
t<-c(1,2,3,4,1,2,3,1,2,1,2,1,2,3,4,5,1,2,3,1,2,3,1,2,3);
cumulativedata<-data.frame(Y=infectious,t=t);
plot(cumulativedata$t,cumulativedata$Y,xlab="Time since initial infection",ylab="Cumulative number of infected caves",pch=19, lwd=2, col=colors()[89], bty='l', las=1)
years=seq(1,5,by=.1)
lines(years,exp(lambda*years), lwd=2, col='brown')
#title(substitute(paste("Basic reproduction number ", R0," = ",R0val), list(R0val=R0)) )

(ggplot(cumulativedata, aes(t,Y))
  + stat_sum()
  + labs(x = "Time since initual infection",
         y = "Cumulative number of infected caves"))
#Consider adding geom_smooth to it

# Now we are going to incorporate the WNS-GT_Duration.R file
# This file does the spatial stuff, log likelihoods, 

## This is all written as one runable function 
wns_duration<-function(...){
  
  # These are the different values of beta?
  # 10.30425552 -0.03080282 0.03506665 0.26193472
  
  #Spatial decay function - probability that node j infects node i
  decay<-function(x,mass,tau,beta0,beta1,beta2,beta3){
    return(tmp=1/(1+exp(beta0 +beta1*tau + (beta2*x)/(mass**beta3))))
  }
  #Derive the weights of the matrix
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
  #unsure as to why we do this AND negative log likelihood
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
  
  #unsure how this is actually plotted as nicely as it is from only x and y coords
  
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
  #interesting approach. Still need to read more about the purpose of this 
  makedist<-function(coords, cutoff=Inf){
    dmat=sqrt(((coords[1,] %*% t(rep(1,length(coords[1,])))) - (rep(1,length(coords[1,])) %*% t(coords[1,])))**2 + ((coords[2,] %*% t(rep(1,length(coords[2,])))) - (rep(1,length(coords[2,])) %*% t(coords[2,])))**2)
    
    dmat=dmat*(dmat<cutoff)
    
    return(Matrix(dmat))
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
  #This makes sense
  
  #find the best fit for beta by minimizing negative log likelihood
  #Need to recreate this in TMB for forsythe project
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
  #Curious as to get a better understanding of what "fragments" of an epidemic means
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
}

#Now we will run a bunch of simulations on the following data with the previous information
#that we have found and functions created

#Read data: US County data (uc)
uc=read.csv('us_data_dur.csv', header=TRUE)
#Subset county data to only Counties with Caves (cc)
cc=uc[which(uc$caves>0),]
#Get coordinates of county centroids
c=rbind(cc$x,cc$y)/1000	#divide by 1000 to convert to km
#Calculate distance matrix
dist=makedist(c)

#Find MLE Beta values
beta=getbeta(dist,cc$caves,cc$tau,cc$WNS,cc$dur,start=c(10.30425552, -0.03080282, 0.03506665, 0.26193472))
#Calculate simultaneous confidence intervals
delta=getdelta(beta$par,dist,cc$caves,cc$tau,cc$WNS,cc$dur)

#Create a matrix with all rows equal to MLE Beta vlaues
b=matrix(beta$par,ncol=4,nrow=10000,byrow=TRUE)

#Simulate foreward from Cave Zero with a sir cave model
dur_sir_sims1_0=makesims(cc,dist,b,nsims=10000,seed=521)
#Simulate foreward from year 5 (all known infection data) with a sir cave model
dur_sir_sims1_5=makesims(cc,dist,b,first=5,nsims=10000,seed=521)

#change durations to set up a si cave model
load('si_dur.Rdata')
cc$dur <- si_dur[which(uc$caves>0),]

#Simulate foreward from Cave Zero with a si cave model
dur_si_sims1_0=makesims(cc,dist,b,nsims=10000,seed=521)
#Simulate foreward from year 5 (all known infection data) with a si cave model
dur_si_sims1_5=makesims(cc,dist,b,first=5,nsims=10000,seed=521)



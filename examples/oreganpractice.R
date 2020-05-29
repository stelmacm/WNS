## Runable script of O'Reagan paper
#setwd("~/Desktop/WNS-master/examples")

#Okay fix up everything so it runs properly
source("oreganfunctions.R")
#So now we have all the functions from the global environment


#Bring in the county duration
source("CountyDurationsScript.R")

#Bring in the gamma dist 
source("GammaDistributionCountyDurationsScript.R")

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



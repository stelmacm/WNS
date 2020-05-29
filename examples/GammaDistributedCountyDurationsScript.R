#Script to calculate duration of epidemic for each county (dur column of us_data_dur) and to
#to calculate number of susceptible caves per county per year over course of epidemic 
#using SIR model parameterized by cave data from NY and PA
##The script creates 2 csv files - us_data_dur.csv and us_data_dur_S_sequence.csv
#Both csv files must be sorted from lowest to highest FIPS number for simulations to work.

#Any questions contact S.M. O'Regan smoregan@uga.edu

GammaSIRWNS<- function(initVars, gamma, R0, populationSize, maxEvents, startTime=0, endTime) {
  #Purpose: Perform numerical integration of SIR system of ODEs using lsoda() ODE solver
  #          to simulate a deterministic SIR model run
  # Returns: a data frame with the following variables:
  #          - time: times at which events occurred
  #          - S: time series of the number/proportion of individuals SUSCEPTIBLE
  #          - I: time series of the number/proportion of individuals INFECTED
  #          - R: time series of the number/proportion of individuals RECOVERED
  #          
  # Parameters:
  #          -initVars: column vector of the initial integers of the population in each class
  #                     [Example (integer-valued): c(S = 1000, I = 2, R = 0) 
  #          -R0: basic reproductive rate, which will be used 
  #                  to calculate beta 
  #          - gamma: reciprocal of infectious period
  #          -maxEvents: number of ODE integration intervals 
  #          -startTime: time point at which to begin integrating [default: 0]
  #          -endTime: time point at which integration will end [default: 100]
  
  # Package
  require (deSolve)
  
  # vector of timesteps (over which to solve the differential equation)
  timeSteps  <- seq(startTime, endTime, length = maxEvents)
  intervalWidth <- round(((endTime - startTime) / maxEvents),4) # Round to 4 decimal places
  beta<-gamma*R0/populationSize #density dependent transmission; beta is a function of the initially fully susceptible population size
  parameters  <- c(gamma,beta)
  
  # INTEGER-VALUED Deterministic SIR Model without demographics
  # ***(NOTE: Standard notation refers to the integer numbers using X,Y,Z
  #      but we here stick to S,I,R for our integer model for consistency)
  
  ###assume gamma distributed infectious period
  
  
  SIRModel <- function(t, x, parms) {
    with(as.list(c(parms, x)), {
      dS <-(-beta*S*(I1+I2+I3+I4+I5)) 		#susceptibles
      dI1 <- beta*S*(I1+I2+I3+I4+I5) -5*(gamma)*I1	#infected   
      dI2 <- 5*(gamma)*I1-5*(gamma)*I2
      dI3 <- 5*(gamma)*I2-5*(gamma)*I3
      dI4 <- 5*(gamma)*I3-5*(gamma)*I4
      dI5<-5*(gamma)*I4-5*(gamma)*I5
      dR <- 5*gamma*I5
      res <- c(dS, dI1,dI2,dI3, dI4, dI5, dR)
      list(res)
    })
  }
  
  ## Numerically integrate using the lsoda() ODE solver
  # Integer-valued model
  out <- as.data.frame(lsoda(initVars, timeSteps, SIRModel, parameters))
  
  
  
} 

#corrected R0 for gamma distributed infectious period
gamma<-1/3
lambda<-0.52
upr.lambda<-0.68
lwr.lambda<-0.39

R0<- lambda/(gamma*(1-(lambda/(gamma*5)+1)^(-5)))
uprR0<-upr.lambda/(gamma*(1-(upr.lambda/(gamma*5)+1)^(-5)))
lwrR0<-lwr.lambda/(gamma*(1-(lwr.lambda/(gamma*5)+1)^(-5)))

caves<-read.csv("ORegan-2016-resources/us_data.csv", header=TRUE) #WNS county dataset

#script to calculate epidemic durations for each county 
#using SIR model parameterized by cave data from NY and PA


unique_cave_numbers<-unique(caves$caves);
unique_cave_numbers<-unique_cave_numbers[-1];
unique_duration<-unlist(lapply(unique_cave_numbers,function (x) {
  results<-GammaSIRWNS(c(S=x-1,  I1=1, I2=0,I3=0, I4=0,I5=0, R=0), gamma, R0, x, 100*10^2, startTime=0, 100);
  ceiling(results[which(rowSums(results[,3:7])<1)[1],1]) #need to change this line.
}))



unique_caves<-data.frame(unique_cave_numbers,unique_duration);

new_caves<-merge(caves,unique_caves,by.x="caves",by.y="unique_cave_numbers",all.x=TRUE)
write.csv(file="us_data_dur_gamma.csv",new_caves) # csv file must be sorted by FIPS number to generate simulations correctly


#script to calculate number of susceptible caves per county per year over course of epidemic 
#using SIR model parameterized by cave data from NY and PA


unique_S_sequence<-unlist(lapply(unique_cave_numbers,function (x) {
  results<-GammaSIRWNS(c(S=x-1,  I1=1, I2=0,I3=0, I4=0,I5=0, R=0), gamma,R0, x, 100*10^2, startTime=0, 100);
  results$S[seq(0,10000,100)+1]
}))
unique_S_sequence<-matrix(t(unique_S_sequence),ncol=length(unique_cave_numbers),nrow=101);
unique_S_sequence<-floor(unique_S_sequence[-101,]);



unique_caves<-data.frame(unique_cave_numbers,unique_duration,t(unique_S_sequence));

new_caves<-merge(caves,unique_caves,by.x="caves",by.y="unique_cave_numbers",all.x=TRUE)
write.csv(file="us_data_dur_S_sequence_gamma.csv",new_caves) # csv file must be sorted by FIPS number to generate simulations correctly

#mean/median duration of epidemic in a county closely agree - about 25 years.
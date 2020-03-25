## ATTEMPT 3

# read in geocache list
m_gc<-read.csv("data/cave-mines-not-complete.csv",header=T)

# fix coords
# on linux, encoding changes to "\xb0"
m_gc$lat <- as.numeric(gsub("\xb0 ",".",gsub("[.]","",gsub(pattern = "N ",replacement = "",m_gc$lat))))
m_gc$lon <- as.numeric(gsub("\xb0 ",".",gsub("[.]","",gsub(pattern = "W ",replacement = "-",m_gc$lon))))

# creat the matrix
pairwise.dist<-distm(m_gc[,c("lon","lat")],m_gc[,c("lon","lat")],fun=distHaversine)


#ATTEMPT 5

# I want to get the x y of all the counties in our dataset and convert them to neighbors
# this will be done via tri2nb function in deldir

#We begin by importing the caves

caves <-read.csv("data/clean-coords.csv")
caves$lat <- caves$X2
caves$long <- caves$X1

locations <- cbind(caves$lat, caves$long)
net1 <- contactnet(type = "random", num.id = 726, location = locations, beta = 0.3)
plot(net1)
infected <- 
  
#ATTEMPT 4  

#Randomize and just like make some random data first 
loc <- cbind(runif(10, 0, 10), runif(10,0,10))
net <- contactnet(type = "random", num.id = 10, location = loc, beta = 0.3)
infection <- c(2.5, 1, 0, 0, 0.5, 0, 2, 1.5, 0, 3)
removal <- c(3.5, 2, 0, 1, 1.5, 0, 3, 2.5, 0, 4)
id <- c(2, 1, 4, 7, 10, 3, 5, 9, 8, 6)
epi <- as.epidat(type = "SIR", kerneltype = "distance", inf.time = infection,
                 rem.time = removal, id.individual = id, location = loc)
epi
# using the data set from the library (obviously given to practice on)
data(NetworkDataSINR)
netSINR<-as.epidat(type = "SINR", kerneltype = "network",
                   incub.time = NetworkDataSINR$epi[,4], inf.time = NetworkDataSINR$epi[,6],
                   rem.time = NetworkDataSINR$epi[,2], id.individual = NetworkDataSINR$epi[,1],
                   location = NetworkDataSINR$loc, network = NetworkDataSINR$net,
                   network.type = "powerlaw")
plot(netSINR, plottype = "history")
#Look into interpretting other plot types
#I dont like N I want SIR
netSIR<-as.epidat(type = "SIR", kerneltype = "network",
                  inf.time = NetworkDataSINR$epi[,6], rem.time = NetworkDataSINR$epi[,2],
                  id.individual = NetworkDataSINR$epi[,1], location = NetworkDataSINR$loc,
                  network = NetworkDataSINR$net, network.type = "powerlaw")
plot(netSIR, plottype = "history")
# OK looks decent, lets try this with my bat data now


#ATTEMPT 1 

# Start with matrix of individuals of indivuals infected
individ <- c(10, 0, 0)

#Have 3 places the indivuals can travel to 
A <- matrix( c(.1, .3, .6,
               .2, .4, .4,
               .3, .2, .5), nrow=3, byrow=TRUE)
A

#Now that I have a transition matrix of an individual going from state i to state j
#I want to have the probability of each edge cause each individual to move for each iteration

#This is going to be a for loop that iterates through 10 time steps for every j individual
T <- 10
for(t in 1:T){
  for(j in 1:3){
    x <- individ[j]
    u <- runif(x)
    cy <- cumsum(y)
    
  }
  
}

# ATTEMPT 2

require(deSolve)
sir.model <- function (t, x, params) {
  S <- x[1] #susceptibles
  I <- x[2] #infected
  R <- x[3] #recovered
  with(
    as.list(params), #local environment to evaluate derivatives
    {
      dS <- mu*(N)-beta*S*I/N-mu*S
      dI <- beta*S*I/N-(mu+gamma)*I
      dR <- gamma*I-mu*R
      dx <- c(dS,dI,dR)
      list(dx)
    }
  )
}


R0 <- function(params) with(as.list(params), beta/(mu+gamma))

times <- seq(0,30,by=1/120)
params <- c(mu=1/70,N=1,beta=400,gamma=365/14)
xstart <- c(S=1-0.001-0.9,I=0.001,R=0.9)

op <- par(fig=c(0,1,0,0.5),mar=c(4,4,2,5))
plot(I~S,data=out,type='b',log='xy',yaxt='n',xlab='S',cex=0.5)
par(fig=c(0,1,0.5,1),mar=c(4,4,2,5), new=TRUE)
plot(S~time,data=out,type='l', ylim=c(0,0.1), xlab='Time')
lines(I~time,data=out,type='l',col='red'); par(new=TRUE)
plot(R~time,data=out,type='l', ylim=c(0.9,1), col='blue', axes=FALSE,
     + xlab='', ylab='', main=paste('R(0) =',round(R0(params),2)), cex.main=0.9)
axis(4)
mtext('R', side=4, line=3)
legend('topright', legend=c('Susceptible', 'Infectious', 'Recovered'), col=c('black','red','blue'), lty=1, bty='n', cex=0.8)
par(op)




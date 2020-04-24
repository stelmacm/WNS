#Create neighbor list from polygon
totalneighbors <- poly2nb(as_Spatial(presence.df$geoms))

#give the neighbors some weights
#why isnt this working unless zero.policy = TRUE
totalneighborweights <- nb2listw(totalneighbors, zero.policy = TRUE)
#I think it means places dont have neighbors

#change these form ID to the other info
names(totalneighborweights$weights) <- presence.df$rownumber

#Something is off.....
#is it really just
totalmatrix <- nb2mat(totalneighbors, style = "B", zero.policy = TRUE)

#Cool so this probably would be super efficient in a for loop....
#My goal is to put all these matracies in a list


#Lets try just the first step with ivec
presence.df$WNS_STATUS[presence.df$WNS_STATUS == "Confirmed"] <- 1
presence.df$WNS_STATUS[presence.df$WNS_STATUS == "Suspect"] <- 0

presence.df$WNS_STATUS<- factor(presence.df$WNS_STATUS)

class(presence.df$WNS_STATUS) <- "numeric"

#Kinda silly to do this but whatever
initialvec <- (presence.df$WNS_STATUS - 1)


#If we go through relevant records, we can create the proper weight matrix which we want
#y2008matrix is the metrix in question
#Alternatively county.m is the constant weight matrix of all the counties from the beginning 

#Errors occuring in matrix multiplication because I think WNS_STATUS is a character not numeric


#Now we are going to a for loop process

#psudo code is:
#After a initial matrix has been defined for starting values
#For every year i in the list of years (which contains 2008-2019)
#uninf <- (initialvec) == 0
#movement <- (y2008matrix)%*%as.numeric(initialvec)
#beta <- -0.19
#FOI <- beta * movement
#hazard <- 1 - exp(FOI)
#I think the size is right. Right?
#initialvec[uninf] <- rbinom(sum(uninf), size = 1, prob = hazard)
#(initialvec)
#Ok so this works but its doesn't feel like its intuitively right to me with what is meant by year changing matrix

#Adrian agrees with year changing weight matrix. Explained it and made sense but forgot to write it down
#Ask Adrian again in future

currentyear <- 2008
#Data is only up until 2019 so 202 should be the cap
while(currentyear < 2020){
  uninf <- (initialvec) == 0
  #Currently using a time averaged matrix until can think a little more about iterating through the list of matricies
  movement <- (county.m)%*%as.numeric(initialvec)
  beta <- -0.19
  FOI <- beta * movement
  hazard <- 1 - exp(FOI)
  #I think the size is right. Right?
  initialvec[uninf] <- rbinom(sum(uninf), size = 1, prob = hazard)
  print(initialvec)
  currentyear<- currentyear + 1
}


#Different version of the Sim. Probably the better version

both.weights<-read.csv("data/gc-shared-users.csv",header=T)

# there are no records from 2016? wierd.

# just loop 'em!
for (i in 2008:2019) {
  x<-both.weights %>%
  filter(year==i) %>%
  select(county1,county2,touching) %>%
  pivot_wider(names_from = county2,values_from = touching) %>%
  replace(is.na(.), 0)
  row.names(x)<-x$county1
  x<-x[,-1]
  assign(paste0("y",i),x)
}

#Lets try just the first step with ivec
initialvec <- (both.weights$incidence)

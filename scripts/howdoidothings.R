#All I am doing here is playing with power exponential and looking into what is the best way to do thing
source("scripts/packages.R")

#Starting off by creating fake dataframe

fakecounties <- (matrix(1, nrow = 3, ncol = 3, dimnames = list(c("NY","OKC","MAINE"), c("NY","OKC","MAINE"))))
#Lets make the values the distance in KM
fakecounties[1,2] <- 150
fakecounties[2,1] <- 150
fakecounties[1,3] <- 25
fakecounties[3,1] <- 25
fakecounties[2,3] <- 175
fakecounties[3,2] <- 175
diag(fakecounties) <- 0

#first inve and then azzalini
invcounty <- 1/fakecounties
diag(invcounty) <- 0
azz <- exp(- invcounty) #But then NY -> OKC has bigger prob than NY -> Maine 
diag(azz) <- 0
#Now to weight matrix
#Can confirm weightmatrix will be the last thing done to this
weight.w <- mat2listw(azz, style = "W")
weight.m <- as(weight.w, "CsparseMatrix") 
weight <- as.matrix(weight.m)
weight #This results in NY -> Maine being smaller

#first azzalini and then inverse??
azzy <- exp(-fakecounties)
inv2 <- 1/azzy #Farther counties have much greater weight 
diag(inv2) <- 0

weight.w2 <- mat2listw(inv2, style = "W")
weight.m2 <- as(weight.w2, "CsparseMatrix") 
weight2 <- as.matrix(weight.m2)
weight2 #Lots of problems

#Playing around
#Trying something with coords and knn
coords <- read.csv("data/latlonofcounty.csv")
#Taking just long lat
lonlat <- cbind(coords$lon, coords$lat)
#Creating Knn to neighbors (just to start)
k1 <- knn2nb(knearneigh(lonlat))

critical.threshold <- max(unlist(nbdists(k1,lonlat)))
critical.threshold

nb.dist.band <- dnearneigh(lonlat, 0, critical.threshold)

distances <- nbdists(nb.dist.band,lonlat)

invd1 <- lapply(distances, function(x) (1/x))
length(invd1) #OK 

invd.weights <- nb2listw(nb.dist.band,glist = invd1,style = "B")
plot(invd.weights, lonlat, lwd=.2, col="blue", cex = .5) #Cool network

#Now doing the super basic can only go 6 neighbors over
k6 <- knn2nb(knearneigh(lonlat, k = 6))
k.distances <- nbdists(k6, lonlat)
invd.weights.knn <- nb2listw(k6,glist = k.distances,style = "B")
plot(invd.weights.knn, lonlat, lwd=.2, col="blue", cex = .5)
#I guess you could say it is much emptier


#Now going to create Gaussian kernal 
kernal.nb <- dnearneigh(lonlat, 0, critical.threshold)
kernalw.distances <- nbdists(kernal.nb, lonlat)
#Gaussian
gaussian.w <- lapply(kernalw.distances, function(x) sqrt(2*pi)*exp((-(x/critical.threshold)^2)/2))
gaussian.weights <- nb2listw(kernal.nb,glist = gaussian.w,style = "B")

plot(gaussian.weights, lonlat, lwd=.2, col="blue", cex = .5) #changed...

#Now going to create Azzalini bubble from neighbors??? Going to replace critical threshold with old optim
kernal.nbA <- dnearneigh(lonlat, 0, 9.5)
kernalw.distancesA <- nbdists(kernal.nb, lonlat)
#Gaussian
azzalini.w <- lapply(kernalw.distancesA, function(x) exp((-(x/9.5)^0.5)))
azzalini.weights <- nb2listw(kernal.nbA,glist = azzalini.w,style = "W")

plot(azzalini.weights, lonlat, lwd=.2, col="blue", cex = .5) #LOL....


#Curious now about the actual that I use
distmat <- read.csv("data/distancematrix.csv")
distmat2 <- distmat[,-1]
rownames(distmat2) <- colnames(distmat2)
d1 <- as.matrix(distmat2)
diag(d1) <- 0 
#INVERSE
d1 <- 1/d1
diag(d1) <- 0
#No Azzalini
localcountymat.w <- mat2listw(d1, style = "W")
plot(localcountymat.w, lonlat, lwd=.2, col="blue", cex = .5) #I don't know what I expected.

#Now trying cutoff
d1[d1 < 0.005] <- 0
smallcounty <- mat2listw(d1, style = "W")
plot(smallcounty, lonlat, lwd=.2, col="blue", cex = .5) #I don't know what I expected.
#seems much more realistic. Hard cut off still seems meh

#Now azzalini (might as well do it with what we have)
aaazzz <- exp(- abs(d1/9.5)^0.5)
aaazzz[aaazzz == 1] <- 0
diag(aaazzz) <- 0

azzalinicounty <- mat2listw(aaazzz, style = "W")
plot(azzalinicounty, lonlat, lwd=.2, col="blue", cex = .5) #Better I guess


#Now with optim Azzalini param
#The key here is that the optim wants to break the matrix
azzy <- exp(-abs(d1/6e-6)^0.5)
azzy[azzy == 1] <- 0
#Connect it
azzycounty <- mat2listw(azzy, style = "W")
plot(azzycounty, lonlat, lwd=.2, col="blue", cex = .5) #Why did this work now??
#I actually cant tell the difference between this and the other one
#did I just freak out over nothing???

#Trying to get a better view of azzy
orderedazzy <- azzy[sort(rownames(azzy)),sort(colnames(azzy))]
#So problem is still occuring...
#Adair OK -> Cherokee OK 34 miles
#Adair OK -> Cheroke Kansas 81 miles
#But in the matrix the greater distance has a larger weight put on it
#This is not what I want. No beuno.

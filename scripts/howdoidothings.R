#Creating an exponential decay spatial weight matrix
#library(conflicted)
#simplified version to run
#import only necessary packages
library(sp)
library(spdep)
library(sf)
## remotes::install_github("yonghah/esri2sf")
library(esri2sf)
library(tidyverse)
library(lubridate)
library(Matrix)
library(raster)
suppressMessages(suppressWarnings(require(spatialreg)))

source("scripts/wns-presence.R")

presence.scrape <- read.csv("data/relevant-records.csv")

#Remove Cali and Wash for now
uniq.df<-presence.df %>% dplyr::filter(! STATEPROV %in% c("California","Washington"))
uniq.df<-uniq.df[!duplicated(uniq.df$county),] #so we only have unique counties

wnslat <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[1]])
wnslon <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[2]])

wns.center.coords <- cbind(wnslat, wnslon)
#rownames(wns.center.coords) <- uniq.df$county

dist.mat <- as.matrix(dist(wns.center.coords, method = "euclidean")) #d_{ij}
#Now I want to perform transformation after x km (say 30 km)

#Turn into exp decay 30 km around point

exponentiate <- function(x) {
  if(x > 30){
      exp(-(x/100)) #I am trying to return a decayed distance rather than a probability
  } else {
      exp(-30/100)
  }
}

xfun2 <- function(x) {
    exp(-pmax(x,30)/100)
}

library(rbenchmark)
library(microbenchmark)
x <- dist.mat
benchmark(
    replications=10,
    x1=ifelse(x>30, exp(-x/100), exp(-30/100)),
    x2=exp(-(ifelse(x>30, x, 30)/100)),
    x3=exp(-pmax(x,30)/100),
    x4=apply(x, 1:2, exponentiate)
)

m1 <- microbenchmark(
    times=10,
    x1=ifelse(x>30, exp(-x/100), exp(-30/100)),
    x2=exp(-(ifelse(x>30, x, 30)/100)),
    x3=exp(-pmax(x,30)/100),
    x4=apply(x, 1:2, exponentiate)
)
autoplot(m1)


## check that it's working
x3 <- exp(-pmax(x,30)/100)
x4 <- apply(x, 1:2, exponentiate)
all.equal(x3,x4)

curve(xfun2,from=0,to=200)

hist(dist.mat)
mean(dist.mat<30)  ## 97.5% of distances < 30 ??

#I think this might actually be promoting the farther points rather than decaying them
decay.mat <- xfun2(dist.mat)
hist(decay.mat)

#Now to weight matrix
localcountymat.w <- mat2listw(decay.mat, style = "W")
localcountymat.m <- as(localcountymat.w, "CsparseMatrix")
dimnames(localcountymat.m) <- list(uniq.df$county,uniq.df$county)

M <- localcountymat.m

rr <- seq(nrow(M)) ## whole matrix
p <- Matrix::image(Matrix(M[rr,rr]), scales=list(x=list(at=seq(length(rr)),labels=rownames(M)[rr]),
                                                 y=list(at=seq(length(rr)),labels=colnames(M)[rr])),
                   xlab="",ylab="",
                   sub="")
#This doesn't look like it is working like it is suppose to 

#My second thought was exponential decay on all this distances rather than 
#just distances from x km onwards being decayed (especially if its only for a weight matrix)
#Is that too aggressive?

decay.all <- exp(-dist.mat)
decaycountymat.w <- mat2listw(decay.all, style = "W")
decaycountymat.m <- as(decaycountymat.w, "CsparseMatrix")
dimnames(decaycountymat.m) <- list(uniq.df$county,uniq.df$county)

B <- localcountymat.m

rr1 <- seq(nrow(B)) ## whole matrix
q <- Matrix::image(Matrix(B[rr1,rr1]), scales=list(x=list(at=seq(length(rr1)),labels=rownames(B)[rr1]),
                                                 y=list(at=seq(length(rr1)),labels=colnames(B)[rr1])),
                   xlab="",ylab="",
                   sub="")

#Me playing with other things
#Inverse matrix just for fun
inv.dist.mat <- 1/dist.mat 
#Turn it into weights
dist.mat.inv <- mat2listw(inv.dist.mat, style = "W", row.names = uniq.df$county)
#This was the most common approach 


#The distance of 30 km
dist30 <- dnearneigh(wns.center.coords, 0, 30, longlat = TRUE)

#Adjecency matrix
touching<-st_intersects(uniq.df$geoms,sparse = F)
touching.m <- as.matrix(touching)
rownames(touching.m)<-colnames(touching.m)<-uniq.df$county


#ew
plot(presence.poly, col = "gray", border = "black")
plot(dist50, wns.center.coords, col = "red", add = TRUE)

#Playing around with spatial weight matrix
#library(conflicted)
#simplified version to run
#import only necessary packages
library(sp)
library(spdep)
library(sf)
## remotes::install_github("yonghah/esri2sf")
library(esri2sf)
library(tidyverse)
library(rgdal)
library(lubridate)
library(Matrix)
suppressMessages(suppressWarnings(require(spatialreg)))
## packrat, pacman, renv, checkpoint
source("scripts/wns-presence.R")

presence.scrape <- read.csv("data/relevant-records.csv")

#Remove Cali and Wash for now
uniq.df<-presence.df %>% dplyr::filter(.,STATEPROV != c("California","Washington"))
uniq.df<-uniq.df[!duplicated(uniq.df$county),] #so we only have unique counties

wnslat <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[1]])
wnslon <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[2]])

wns.center.coords <- cbind(wnslat, wnslon)
rownames(wns.center.coords) <- uniq.df$county

knn.poly <- knn2nb(knearneigh(wns.center.coords, k = 1, longlat = NULL)) #doubles?

wns.threshold.poly <- max(unlist(nbdists(knn.poly, wns.center.coords, longlat = TRUE)))

#distance band approach
neighbor.distance.band.poly <- dnearneigh(wns.center.coords, 0, wns.threshold.poly,
                                          longlat = TRUE)
names(neighbor.distance.band.poly) <- uniq.df$county

localcounty <- nb2listw(neighbor.distance.band.poly, style = "W") 
#Style W I think?
#Vignette does style B but I think they are achieving something else
#https://cran.r-project.org/web/packages/spdep/vignettes/nb_igraph.html

localcounty.matrix <- as(localcounty, "CsparseMatrix")
dimnames(localcounty.matrix) <- list(uniq.df$county,uniq.df$county)
##Tried this method, st_ using sf, unsure what is incorrect about the approach

M <- localcounty.matrix

## rr <- 300:350
rr <- seq(nrow(M)) ## whole matrix
p <- Matrix::image(Matrix(M[rr,rr]), scales=list(x=list(at=seq(length(rr)),labels=rownames(M)[rr]),
                                          y=list(at=seq(length(rr)),labels=colnames(M)[rr])),
                   xlab="",ylab="",
                   sub="")

## pdf("tmp.pdf",width=60,height=60)
## print(p)
## dev.off()







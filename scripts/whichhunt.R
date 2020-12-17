#Testing different types of coord to km types
library(sf)
library(sp)
library(spdep)
library(tidyverse)
library(Matrix)
library(lubridate)
require(esri2sf)#Oops every package ever
source("scripts/wns-presence.R")

presence.scrape <- read.csv("data/relevant-records.csv")

#Remove Cali and Wash for now
uniq.df <- (presence.df
            %>% dplyr::filter(!STATEPROV %in% c("California","Washington"),
                              !duplicated(county)) #so we only have unique counties
)

wnslat <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[1]])
wnslon <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[2]])

wns.center.coords <- cbind(wnslat, wnslon)

d1<- distm(wns.center.coords, fun = distGeo)
dimnames(d1) <- list(uniq.df$county,uniq.df$county)

diag(d1) <- 0 
#convert from m to km
d1 <- d1/1000

#Best log likelihood
param = 100

#Maybe just have to do it by hand
azzelinifun <- exp(-((d1)/param)^2)
diag(azzelinifun) <- 0
azzelinifun[(azzelinifun < 1e-100)] <- 0

#Now to weight matrix
localcountymat.w <- mat2listw(azzelinifun, style = "W")
localcountymat.m <- as(localcountymat.w, "CsparseMatrix") 
Matrix::image(localcountymat.m)
localcountymat <- as.matrix(localcountymat.m)
dimnames(localcountymat) <- list(uniq.df$county,uniq.df$county)
tableofzeros <- which(localcountymat == 0, arr.ind = TRUE)

#Exploration
sum(azzelinifun == 0)
str(tableofzeros)
dim(d1)
dim(azzelinifun)
par(mfrow = c(1,2))
hist(log10(azzelinifun))
hist(log10(localcountymat))
length(which(localcountymat == 0))
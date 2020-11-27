#The first set of models is from modelresid
library(akima)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(DHARMa)
library(lme4)
library(reshape)
usa <- map_data("usa")
#Read in data
modelresid <- read.csv("data/modelresiduals.csv")
#Data includes model variables, glm features,
#glmm resid are called pearsonresid

#Just taking 1 year to play around with and do all the tests on 
#This is glm resid approach
testyear <- modelresid %>% filter(year == 2010)

#Force of infection
akimafoi <- interp(x = testyear$lon, y = testyear$lat, z = testyear$foi, 
                    xo = seq(min(testyear$lon), max(testyear$lon), by = .1), 
                    yo = seq(min(testyear$lat), max(testyear$lat), by = .1),
                    duplicate = "strip")
image(akimafoi, col = rainbow(16, start = .6, end = .1))

#Tryna make the akima into ggplot
#Pearson residual for glm
pearsresid <- with(testyear, interp(x = testyear$lon, y = testyear$lat, z = (testyear$eresid), 
                         xo = seq(min(testyear$lon), max(testyear$lon), by = .1), 
                         yo = seq(min(testyear$lat), max(testyear$lat), by = .1),
                         duplicate = "strip"))

d2 <- melt(pearsresid$z, na.rm = TRUE)
names(d2) <- c("x", "y", "eresid")

d2$lon <- pearsresid$x[d2$x]
d2$lat <- pearsresid$y[d2$y]
#ggplot of pearsonresid
ggplot() +
  geom_tile(data = d2, aes(x = lon, y = lat, fill = eresid)) +
  geom_path(data = usa, aes(x = long, y = lat, group = group)) + 
  scale_fill_gradient(low = "blue", high = "red", na.value = "white") +
  geom_point(data = testyear, aes(x = lon, y = lat, shape = factor(incidence), alpha = factor(incidence))) +
  theme_classic() + coord_equal()

#Now for force of infection
foiinterp <- with(testyear, interp(x = testyear$lon, y = testyear$lat, z = (testyear$foi), 
                                    xo = seq(min(testyear$lon), max(testyear$lon), by = .1), 
                                    yo = seq(min(testyear$lat), max(testyear$lat), by = .1),
                                    duplicate = "strip"))

d3 <- melt(foiinterp$z, na.rm = TRUE)
names(d3) <- c("x", "y", "foi")

d3$lon <- foiinterp$x[d3$x]
d3$lat <- foiinterp$y[d3$y]
#ggplot of FOI
#Realized only way to make this nice is interpolate all of the grid (takes too much time)
ggplot() +
  geom_tile(data = d3, aes(x = lon, y = lat, fill = foi)) +
  geom_path(data = usa, aes(x = long, y = lat, group = group)) + 
  scale_fill_gradient(low = "blue", high = "red", na.value = "white") +
  theme_classic() + coord_equal() +
  geom_point(data = testyear, aes(x = lon, y = lat, shape = factor(incidence), alpha = factor(incidence)))
#Just wanted to see where the other caves are

#This will be glmm resid approach
formula <- incidence ~ (1|year) + (1|county) + offset(log(previousyear + 1))

#modeldataframe <- modelresid %>% dplyr::select(county, year,foi,incidence, previousyear) 

#something went wrong with writing csv and has error. 
modeldf <- read_csv("data/mixedmodeldf.csv") 

#I swear I had the glmer version working until last night???
foimixedmodel2 <- glmer(formula, data = modeldf, family = binomial(link = "cloglog"), nAGQ = 0)
#library(glmmTMB)
#foimixedmodel2 <- glmmTMB(formula,data = modeldataframe, family = binomial(link = "cloglog")) #Back up option
#Doing DHARMa sim from the model bc thats how DHARMa works
simoutput <- simulateResiduals(fittedModel = foimixedmodel2)
plot(simoutput)
hist(simoutput)
#Interesting...
plotResiduals(simoutput,modeldf$year)

#Interesting?
plotResiduals(simoutput, modeldf$foi)

#oh...?
testDispersion(simoutput)
testZeroInflation(simoutput)

#seems kinda silly to do this but...
simoutput2 <- recalculateResiduals(simoutput, group = modeldf$year)
plot(simoutput2) #Ew

#My next thoughts are creating number of observations at a given county and including that into the mixed model
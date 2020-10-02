library(tidyverse)
library(geosphere)
library(conflicted)
## function(distance) {
##   calculate weight matrix, generate distancevalue
##   fit the model (quick)
##   return the log-likelihood
## }

#So we will be making a function to do the above

source("scripts/packages.R")
source("scripts/wns-presence.R") 

uniq.df <- (presence.df
            %>% dplyr::filter(!STATEPROV %in% c("California","Washington"),
                              !duplicated(county)) #so we only have unique counties
)

wnslat <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[1]])
wnslon <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[2]])

wns.center.coords <- cbind(wnslat, wnslon)

d1<- distm(wns.center.coords, fun = distGeo)
diag(d1) <- 0
d1 <- d1/1000

#distance matrix will always be d1 but is called distancematrix incas d2 needs to be entered

loglikfunction <- function(distancematrix = d1, distcutoff = 30, expcutoff = 1e-50){
  
  xfun2 <- function(x) {
    exp(-pmax(x,distcutoff))
  }
  
  decay.mat <- xfun2(distancematrix)
  #Now set cut off
  
  decay.mat[(decay.mat < expcutoff)] <- 0 #Chose 300 but maybe I should do more?
  #Logic for choosing 300 was going through several the matrix and seeing that many
  #of the incredibly "small" points were in the 300's
  #Although there are some in the upper 200 there is not many and they are usually within state
  #which seemed reasonable
  
  #I chose 150 bc I asked my mom to pick a number between 50 and 300
  
  #Actually keep diag set to 0 here bc otherwise it gets set to 1
  diag(decay.mat) <- 0
  
  #Now to weight matrix
  localcountymat.w <- mat2listw(decay.mat, style = "W")
  localcountymat.m <- as(localcountymat.w, "CsparseMatrix") #I hope this makes it sparse
  dimnames(localcountymat.m) <- list(uniq.df$county,uniq.df$county)
  localcountymat <- as.matrix(localcountymat.m)
  
  #Create incidence matrix
  uniq.df$incidence <- ifelse(uniq.df$YR_CONFIRM == " ",0,1)
  #There is no county 2. Dont know how to approach that
  incidencedata <- subset(uniq.df, select = -c(FID, SAMPLEDATE, FIPS,
                                               COUNTRY, suspect1, suspect2,
                                               confirm1, confirm2, smooth,
                                               geoms))
  
  localcountymat2 <- reshape2::melt(localcountymat)[reshape2::melt(upper.tri(localcountymat))$value,]
  names(localcountymat2) <- c("county","county2", "value")
  #Join caves that had any incidence and the probabilities
  
  glmdataframe <- left_join(incidencedata, localcountymat2, by = "county")
  
  glmdataframe <- subset(glmdataframe, select = -c(WNS_MAP_YR, WNS_STATUS, YR_SUSPECT,
                                                   YR_CONFIRM, date))
  
  glmdataframe$year <- factor(glmdataframe$year)
  glmdataframe$county <- factor(glmdataframe$county)
  glmdataframe$county2 <- factor(glmdataframe$county2)
  glmdataframe$incidence <- factor(glmdataframe$incidence)
  
  modelresult <- glm(incidence ~  value, data = glmdataframe, family = binomial(link = "cloglog"))
  modellog <- logLik(modelresult)
  return(modellog)
  
}

#Function takes distancematrix, distcutoff, expcutoff
loglikfunction(d1,30,1e-75)

#No matter what I do I end up getting -89445. something (df=2) it just makes it seem like changing anything becomes useless

#Now I want to do loop of cutoffs
#First we create a list of values that we want to loop (for expcutoff)
#sequence doesnt work with very small numbers?
#expcutofflist <- seq(from = 1e-500,to = 1e-25, by = 1e-25)
#expcutofflist <- c(1e-25,1e-50,1e-75,1e-100,1e-125,1e-150,1e-175,1e-200,1e-225,1e-250,1e-275,1e-300)
#Or does it need to be in this format
#varlist <- c(d1, 30, 1e-50,
#             d1, 30, 1e-100,
#             d1, 30, 1e-150,
#             d1, 30, 1e-200,
#             d1, 30, 1e-250)
#listofloglikelihoods<- sapply(expcutofflist, bigboifunction)
#Played around with sapply and couldnt figure it out
#for loop also never really came to fruition

#Back to the mixed model

realdf <- (read.csv("data/mixedmodeldf.csv")  ## switch to read.csv (encodings)
           %>% as_tibble()
           %>% filter(year>2006)
           %>% mutate(previousyear=lag(incidence))
           %>% dplyr::select(id,year,county, yc, incidence, previousyear)
           %>% mutate_at("year", factor)
)

#view(realdf)

formula <- incidence ~ (1|year) + (1|county) +
  offset(log(previousyear + 1))

#The idea here was that the model used year and county as random effects
#from there we also used the fact if the previous year was infected 
#to make the model more epi focused
#Question still becomes, how to implement the spatial decay matrix into the
#glmm model and does it replace the county random effect
#Are there tests to do (other than likelihood which will compare the 2 models)


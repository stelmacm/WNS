library(tidyverse)
library(geosphere)
library(conflicted)
conflict_prefer("flatten", "purrr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")

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
dimnames(d1) <- list(uniq.df$county,uniq.df$county)


diag(d1) <- 0 #Cant see matrix for some reason 

#convert from m to km
d1 <- d1/1000
#Havent decided yet but both are *reasonable*
#Is it cheating to simply pick which one has the nicer likelihood?

xfun2 <- function(x) {
  exp(-pmax(x,30))
}

decay.mat <- xfun2(d1)
#Now set cut off

decay.mat[(decay.mat < 1e-150)] <- 0 #Chose 300 but maybe I should do more?
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

#Matrix fini
#Now incidence per year
mixedmodeldf <- read.csv("data/mixedmodeldf.csv")

county.incidencee.by.year <- mixedmodeldf[-c(27,28,29,30,31,32,33,34,35,36,37,38,39,
                                             5201, 5202,5203,5204,5205, 5206,5207,5208,5209, 5210,5211,5212,5213,
                                             7151,7152,7153,7154,7155,7156,7157,7158,7159,7160,7161,7162,7163),-4]
  
View(county.incidencee.by.year)

countylist <- county.incidencee.by.year %>%
  arrange(year) %>%
  filter(year == 2006) %>%
  select(county)

yearone <- county.incidencee.by.year %>% 
  arrange(year) %>%
  filter(year == 2006) %>%
  select(incidence)

yearone <- as.matrix(yearone)
rownames(yearone) <- uniq.df$county

yeartwo <- county.incidencee.by.year %>% 
  arrange(year) %>%
  filter(year == 2007) %>%
  select(incidence)

yeartwo <- as.matrix(yeartwo)
rownames(yeartwo) <- uniq.df$county

yearthree <- county.incidencee.by.year %>% 
  arrange(year) %>%
  filter(year == 2008) %>%
  select(incidence)

yearthree <- as.matrix(yearthree)
rownames(yearthree) <- uniq.df$county

yearfour <- county.incidencee.by.year %>% 
  arrange(year) %>%
  filter(year == 2009) %>%
  select(incidence)

yearfour <- as.matrix(yearfour)
rownames(yearfour) <- uniq.df$county

yearfive <- county.incidencee.by.year %>% 
  arrange(year) %>%
  filter(year == 2010) %>%
  select(incidence)

yearfive <- as.matrix(yearfive)
rownames(yearfive) <- uniq.df$county

yearsix <- county.incidencee.by.year %>% 
  arrange(year) %>%
  filter(year == 2011) %>%
  select(incidence)

yearsix <- as.matrix(yearsix)
rownames(yearsix) <- uniq.df$county

yearseven <- county.incidencee.by.year %>% 
  arrange(year) %>%
  filter(year == 2012) %>%
  select(incidence)

yearseven <- as.matrix(yearseven)
rownames(yearseven) <- uniq.df$county

yeareight <- county.incidencee.by.year %>% 
  arrange(year) %>%
  filter(year == 2013) %>%
  select(incidence)

yeareight <- as.matrix(yeareight)
rownames(yeareight) <- uniq.df$county

yearnine <- county.incidencee.by.year %>% 
  arrange(year) %>%
  filter(year == 2014) %>%
  select(incidence)

yearnine <- as.matrix(yearnine)
rownames(yearnine) <- uniq.df$county

yearten <- county.incidencee.by.year %>% 
  arrange(year) %>%
  filter(year == 2015) %>%
  select(incidence)

yearten <- as.matrix(yearten)
rownames(yearten) <- uniq.df$county

yeareleven <- county.incidencee.by.year %>% 
  arrange(year) %>%
  filter(year == 2016) %>%
  select(incidence)

yeareleven <- as.matrix(yeareleven)
rownames(yeareleven) <- uniq.df$county

yeartwelve <- county.incidencee.by.year %>% 
  arrange(year) %>%
  filter(year == 2017) %>%
  select(incidence)

yeartwelve <- as.matrix(yeartwelve)
rownames(yeartwelve) <- uniq.df$county

yearthirteen <- county.incidencee.by.year %>% 
  arrange(year) %>%
  filter(year == 2018) %>%
  select(incidence)

yearthirteen <- as.matrix(yearthirteen)
rownames(yearthirteen) <- uniq.df$county


#BAM
#Have all the individual I_year nx1 matricies


#So somewhere the two lists fuck up and I need to find the differences
#mixedmodeldf has 548 counties

#So we have each year done
#We want to do the multiplication of matrix W %*% incidence I_(year)
#After that we have our force of infection matrix
library(reshape2)
foi.one <- localcountymat.w %*% yearone
foi.two <- localcountymat.w %*% yeartwo
foi.three <- localcountymat.w %*% yearthree
foi.four <- localcountymat.w %*% yearfour
foi.five <- localcountymat.w %*% yearfive
foi.six <- localcountymat.w %*% yearsix
foi.seven <- localcountymat.w %*% yearseven
foi.eight <- localcountymat.w %*% yeareight
foi.nine <- localcountymat.w %*% yearnine
foi.ten <- localcountymat.w %*% yearten
foi.eleven <- localcountymat.w %*% yeareleven
foi.twelve <- localcountymat.w %*% yeartwelve
foi.thirteen <- localcountymat.w %*% yearthirteen

#Truth holds so I guess matrix multiplication is working?
identical(colnames(localcountymat.w), rownames(yearthirteen))


foidataframe <- cbind(foi.one, foi.two, foi.three, foi.four, foi.five, foi.six, foi.seven, foi.eight,
                      foi.nine, foi.ten, foi.eleven, foi.twelve, foi.thirteen)
foidataframe <- as.matrix(foidataframe)

as.list(colnameyears) <- seq(2006, 2018, by = 1)
colnames(foidataframe) <- c("2006","2007","2008","2009","2010","2011","2012","2013","2014",
                            "2015","2016","2017","2018")
View(foidataframe)

write.csv(foidataframe, "data/foidataframe.csv")

#Now we want to leftjoin this dataset to the glm one
#First we have to melt this data set

meltedfoi <- melt(foidataframe, id = "county") %>%
  rename(county = 'Var1', year = 'Var2') %>%
  arrange(county)

#cant factor in the pipe for some reason
meltedfoi$year <- factor(meltedfoi$year)
View(meltedfoi)



library(tidyverse)
#Testing different types of coord to km types
source("scripts/wns-presence.R")

presence.scrape <- read.csv("data/relevant-records.csv")

#Remove Cali and Wash for now
uniq.df <- (presence.df
    %>% dplyr::filter(!STATEPROV %in% c("California","Washington"),
                      !duplicated(county)) #so we only have unique counties
)

### uniq.df<-presence.df %>% dplyr::filter(.,STATEPROV != c("California","Washington"))
##     %>% niq.df<-uniq.df[!duplicated(uniq.df$county),] 

wnslat <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[1]])
wnslon <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[2]])

wns.center.coords <- cbind(wnslat, wnslon)

#method 2 (probably the one I will keep)
library(geosphere)
#Many cool functions to remember for later
#distm is the fun but has different methods. DistGeo and distHaversine both seem reasonable
#distGeo takes shortest dist between 2 points on an ellipoid (WGS84) (called geodesic distance)
#I think distGeo is Vincenty (Vinsanity lol) and is more accurate
#https://stackoverflow.com/questions/38248046/is-the-haversine-formula-or-the-vincentys-formula-better-for-calculating-distan
#SEA LEVELS MATTER??? ugh ridinkidinkulous
#option 1
d1<- distm(wns.center.coords, fun = distGeo)
dimnames(d1) <- list(uniq.df$county,uniq.df$county)
## view(d1)
d2<- distm(wns.center.coords, fun = distHaversine)
dimnames(d2) <- list(uniq.df$county,uniq.df$county)
## view(d2)
identical(d1,d2) #False. mean relative difference is 0.00102. Probably fine
all.equal(d1,d2)
#I am a CLOWN. I spent so much time double checking only to realize im double checking cities not counties
#Which is why everything was wrong

#convert from m to km
d1 <- d1/1000
## Haven't decided (between great-circle [Haversine] distance and
## local (CRS) coordinates but both are *reasonable*
#Is it cheating to simply pick which one has the nicer likelihood? YES

xfun2 <- function(x) {
  exp(-pmax(x,30))
}

xfun2_sc <- function(x) xfun2(x)/xfun2(0)
curve(xfun2_sc, from=0,to=100,log="y")

decay.mat <- xfun2(d1)
#Now set cut off

plot(log10(sort(decay.mat[decay.mat>0])), type="l")
decay.mat[(decay.mat < 1e-50)] <- 0 #Chose 300 but maybe I should do more?
#Logic for choosing 300 was going through several the matrix and seeing that many
#of the incredibly "small" points were in the 300's
#Although there are some in the upper 200 there is not many and they are usually within state
#which seemed reasonable

diag(decay.mat) <- 0 #Cant see matrix for some reason 

#Now to weight matrix
localcountymat.w <- mat2listw(decay.mat, style = "W")
localcountymat.m <- as(localcountymat.w, "CsparseMatrix") #I hope this makes it sparse
dimnames(localcountymat.m) <- list(uniq.df$county,uniq.df$county)
#A *little* upset I can't file_out this in drake so everything can be up to date but its fine
Matrix::image(localcountymat.m)

fix_county2 <- function(x) {
    ## could use pattern "(-[^-]+| County)$"
    x <- stringr::str_remove(x,"-[^-]+$")  ## drop state
    return(x)
}

fix_county1 <- function(x)  {
    x <- stringr::str_remove(x," County$") ## drop extra 'County'
}


##Now organize so we can have a glm format
file.info("data/gc-shared-users.csv")
system("wc data/gc-shared-users.csv")
shareduser <- (read.csv("data/gc-shared-users.csv")
    %>% mutate_at(c("county1","county2"), fix_county1)
)
#A bit confused on how the left_join this together

## countyneighborincidence <- left_join(shareduser, localcountymat.m, by= c("county1","county2")) #This is wrong
#Need to rename dim of localcountymat in order for this to go


mm <- (localcountymat.m
    %>% as.matrix()
    %>% reshape2::melt()
    %>% as_tibble()
    %>% filter(value>0)
    %>% rename(county1="Var1",county2="Var2",dwt="value")
    %>% mutate_at(c("county1","county2"), as.character)
    %>% mutate_at(c("county1","county2"), fix_county2)
    ## %>% left_join(shareduser, by =c("county1", "county2"))
)

## I want: all of the county pairs that occur in mm
##  (truncated distance/weight matrix) [county pair w/ positive weight]
## *do* want all years for each of these county pairs

mm2 <- left_join(mm, shareduser, by=c("county1","county2"))
sum(is.na(mm2$incidence))  ## rows without incidence info
sum(!is.na(mm2$incidence)) ## rows with  incidence info

## mm/cnames: distance values

cnames <- sort(unique(mm$county1))
head(cnames)
head(unique(grep("^A",cnames,invert=TRUE,value=TRUE)))
head(cnames[cnames>="Bennington"])
snames <- sort(unique(shareduser$county1))
head(snames)

## NONE of the county names match ... ????????????????????????????
length(cnames)
nsharedcounty <- length(snames)
nrow(shareduser)
nsharedyr <- length(unique(shareduser$year)) ## 11 years
## should be 3481 pairs?
nsharedyr * nsharedcounty^2 == nrow(shareduser)


length(intersect(cnames,snames))  ## 57 match
setdiff(cnames,snames)  ## in cnames but not snames
setdiff(snames,cnames)  ## vice versa

## trying to figure out if there's a partial match ...
grep("Gasp",cnames,value=TRUE)
grep("ot",cnames,value=TRUE)
grep("Gasp",snames,value=TRUE)
grep("Nipis",cnames,value=TRUE)

with(shareduser,table(
                    table(county1,county2)
                ))
## all county pairs have 11 observations



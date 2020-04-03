# there was absolutely an easier way to do this
y2008 <- relevant.records %>%
  filter(Year == "2008") %>%
  group_by(county, state.prov, Year, coords.x1, coords.x2) %>%
  summarise(total = length(User))
#There are alot of doubles occuring and I'm not sure why
# crs means coord refernce system. This one is ours
y2008.p <- as_Spatial(st_as_sf( y2008,
                                coords = c("coords.x1","coords.x2"),
                                crs = 4326, agr = "constant"))

y2009 <- filter(relevant.records, Year == "2009")
y2010 <- filter(relevant.records, Year == "2010")
y2011 <- filter(relevant.records, Year == "2011")
y2012 <- filter(relevant.records, Year == "2012")
y2013 <- filter(relevant.records, Year == "2013")
y2014 <- filter(relevant.records, Year == "2014")
y2015 <- filter(relevant.records, Year == "2015")
y2016 <- filter(relevant.records, Year == "2016")
y2017 <- filter(relevant.records, Year == "2017")
y2018 <- filter(relevant.records, Year == "2018")
y2019 <- filter(relevant.records, Year == "2019")

#All of this ^ needs to be done in a better way

# now I want to find contact contact counties for each respective year
# at the spatial locations of object x retrieves the indexes or attributes from spatial object y
# index for sites that match up with polys?? Can confirm yes that is what I am doing
# I can't see anything else to do this with other than sp::over and I think thats a solid method
# county polygons that match up with sites from each respective geolocation

polymatch <-sp::over(y2008.p, as_Spatial(presence.df$geoms),fn=NULL)

#from this thingy we wanna grab the list of unique polygons
#any(is.na(y2008unique.df))
#is it worth just putting na.omit or should I check is all of them have Na's 
y2008unique.df<-presence.df[(unique(polymatch)),]

#Take those coords from country poly to centroid points for the given county
#I keep getting errors that centroid isnt center and I run it again and it works??
#Change +datum and Now it just gives me warning not error.... interesting
collectivecounty <- y2008unique.df$geoms %>% st_centroid() %>%
  st_transform(.,"+proj=longlat +datum=WGS84")

#Create neighbor list from centroids
y2008neighbors <- poly2nb(as_Spatial(presence.df$geoms))

#give the neighbors some weights
#why isnt this working unless zero.policy = TRUE
y2008neighborweights <- nb2listw(y2008neighbors, zero.policy = TRUE)
#I think it means places dont have neighbors

#change these form ID to the other info
names(y2008neighborweights$weights) <- presence.df$rownumber

#Something is off.....
#is it really just
y2008matrix <- nb2mat(y2008neighbors, style = "B", zero.policy = TRUE)
#Went with B for style because im unsure which. Maybe ask Ben about it more than what he previously wrote

#Cool so this probably would be super efficient in a for loop....
#My goal is to put all these matracies in a list

#Lets try just the first step with ivec
initialvec <- (presence.df$WNS_STATUS)

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
#initialvec[uninf] <- rbinom(sum(uninf), size = 727, prob = hazard)
#return(initialvec)
#Ok so this works but its doesn't feel like its intuitively right to me with what is meant by year changing matrix

year <- c(2008:2019)
currentyear <- 2008
countymatrix <- list(y2008,y2009,y2010,y2011,y2012,y2013,y2014,y2015,y2016,y2017,y2018,y2019)

while(currentyear < 2020){
  uninf <- (initialvec) == 0
  #Currently using a time averaged matrix until can think a little more about iterating through the list of matricies
  movement <- (county.m)%*%as.numeric(initialvec)
  beta <- -0.19
  FOI <- beta * movement
  hazard <- 1 - exp(FOI)
  #I think the size is right. Right?
  initialvec[uninf] <- rbinom(sum(uninf), size = 727, prob = hazard)
  return(initialvec)
  currentyear<- currentyear + 1
}


#Ask Ben about GAM's from mcgv and gam package and why should one be used over the other. Mildly understand that the scatterplot
#aspect matters into the actual model itself. 

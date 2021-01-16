source("scripts/packages.R")
source("scripts/wns-presence.R")

#remotes::install_github("yonghah/esri2sf")

library(tidyverse)
library(sf)
library(sp)

presence.scrape <- read.csv("data/relevant-records.csv")

#Remove Cali and Wash for now
uniq.df <- (presence.df
            %>% dplyr::filter(!STATEPROV %in% c("California","Washington"),
                              !duplicated(county)) #so we only have unique counties
)

wnslat <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[1]])
wnslon <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[2]])

wns.center.coords <- cbind(wnslat, wnslon)

d1 <- distm(wns.center.coords, fun = distGeo)
dimnames(d1) <- list(uniq.df$county,uniq.df$county)

diag(d1) <- 0 
#convert from m to km
d1 <- d1/1000

#Take known data
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))
#Remove things that don't work for me later on

#Country incidence by year needs to have the following counties removed
#King Washington
#Lewis Washington
#Plumas California
#Year county column removed as well

county.incidence.by.year <- mixedmodeldf %>% filter(county != "Lewis-Washington" &
                                                      county != "King-Washington" &
                                                      county != "Plumas-California") %>%
  dplyr::select(-yc)

#Only way I could figure it out is via
countylist <- county.incidence.by.year %>%
  arrange(year) %>%
  filter(year == 2006) %>%
  dplyr::select(county)

#Create Azzalini power exponential
azzelinifun <- exp(-((d1)/9.505635)^0.512292358)
diag(azzelinifun) <- 0
azzelinifun[(azzelinifun < 1e-100)] <- 0 
#Next thing to look at is maybe hyper param for cut offs

#Now to weight matrix
localcountymat.w <- mat2listw(azzelinifun, style = "W")
localcountymat.m <- as(localcountymat.w, "CsparseMatrix") 
localcountymat <- as.matrix(localcountymat.m)

#Create forloop to create dataframe
#Create dataframe of county by year filled with incidence
#Retake first year english so that sentence makes sense
for(i in levels(mixedmodeldf$year)){
  countylist[,i] <- county.incidence.by.year %>%
    arrange(year) %>%
    filter(year == i) %>%
    dplyr::select(incidence)
}
#view(countylist)
#This data frame is MAYBE NOT OK. Incidence is not the same as it is in countylist 

countylist <- as.data.frame(countylist[,-1])

#Now we need to multiply W_ij by every I_t 
#Another for loop

#Create a base layer to begin with
foidf <- county.incidence.by.year %>%
  arrange(year) %>%
  filter(year == 2006) %>%
  dplyr::select(county)


#For loop that creates force of infection datafrome
for (i in levels(mixedmodeldf$year)) {
  #need to detach
  infectionvector <- countylist[,i]
  infectionvector <- as.matrix(infectionvector)
  #multiply W_ij %*% I_t
  foivector <- localcountymat %*% infectionvector
  #reattach
  foidf[,i] <- foivector
}


#The melt looks something like this
forceofinfectiondf <- reshape2::melt(foidf, id = "county") %>%
  dplyr::rename(year = 'variable', foi = 'value') %>%
  arrange(county) %>%
  mutate(year = factor(year))
#Never arranged alphabetically before. odd 

incidencedata <- (read.csv("data/incidencepercounty.csv"))%>%
  dplyr::select(county, year, incidence) %>%
  mutate(year = factor(year))

modeldataframe <- left_join(forceofinfectiondf, incidencedata, by = c("county","year")) %>%
  mutate(previousyear=lag(incidence)) %>%
  filter(year != "2006") #Maybe there are nicer ways to do this. oh well

modeldataframe$year <- factor(modeldataframe$year)
modeldataframe$previnf <- (lag(modeldataframe$foi))

#Changing to disappear after incidence occures
newdf <- modeldataframe %>% subset((incidence == 0) | (incidence == 1 & previousyear == 0))

#Mixed model formula
formula <- incidence ~ (1|year) + (1|county) + offset(log(previnf + 0.002321367))

foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))

#Now we will make some dharma sims
library(DHARMa)
mmresid <- simulateResiduals(foimm)
plot(mmresid) #yikes
#Use this for map plots
plotResiduals(foimm)

predictions <- predict(foimm, type = "response")
preddf <- as.data.frame(predictions)

#Redoing this because it breaks here
orgin <- na.omit(newdf)
preddf$county <- orgin$county
preddf$year <- orgin$year
predvizdf <- inner_join(orgin, preddf,c("county","year"))
#Predviz has more countries than we have coords for hence nrows dont equal

usa <- map_data("usa")
states <- map_data("state")
#Read in data that contains lon lat
modelresid <- read.csv("data/modelresiduals.csv")

latloninfo <- modelresid %>% dplyr::select(county, year, lon,lat) %>%
  filter(year == 2008) %>% dplyr::select(-year)

predvizspatial <- left_join(preddf, latloninfo, "county")
predvizspatial <- na.omit(predvizspatial)

p <- ggplot() +
  geom_path(data = states, aes(x = long, y = lat, group = group)) +
  geom_point(data = predvizspatial, aes(x = lon, y = lat, colour = predictions)) +
  coord_equal() +
  theme_minimal() +
  scale_colour_gradient(low = "yellow", high = "red") 

p

infodf0 <- na.omit(newdf)
infodf0$pearsonres <- residuals(foimm, type = "pearson")


## not idempotent
## idempotent = f(f(x)) = f(x)
infodf <- left_join(infodf0, latloninfo, "county")

outliers <- infodf %>% filter(abs(pearsonres) > 2) %>% arrange(pearsonres)
View(outliers)


l <- ggplot() +
    geom_path(data = states, aes(x = long, y = lat, group = group)) +
    geom_point(data = outliers, aes(x = lon, y = lat, colour = cut_width(pearsonres,2)),size=5) +
    coord_equal() +
    theme_minimal() 

l




#Doesnt really say much
# #Now lets compare prediction to FOI
# foilatlon <- left_join(predvizdf, latloninfo, "county")
# foilatlon <- na.omit(foilatlon)
# 
# q <- ggplot() +
#   geom_path(data = states, aes(x = long, y = lat, group = group)) +
#   geom_point(data = foilatlon, aes(x = lon, y = lat, colour = predictions)) +
#   geom_point(data = foilatlon, aes(x = lon, y = lat, colour = foi)) +
#   coord_equal() +
#   theme_minimal() 
# 
# q
# #Should this be FOI or incidence that gets compared to the prediction
# #Doesnt look very nice
# 
# firstyearinf <- na.omit(newdf) %>%
#   mutate(pearsonres = residuals(foimm, type = "pearson")) %>% 
#   filter((incidence == 1 & previousyear == 0)) 
# 
# 
# l <- ggplot() +
#   geom_path(data = states, aes(x = long, y = lat, group = group)) +
#   geom_point(data = firstyearinf, aes(x = lon, y = lat, colour = pearsonres)) +
#   coord_equal() +
#   theme_minimal() 
# 
# l
# oddcounty <- modelresid %>% filter(pearsonres > 5)
# View(oddcounty)


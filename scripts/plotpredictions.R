#Find residuals of first year infected in a county

#Take known data
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))
#Remove things that don't work for me later on
county.incidencee.by.year <- mixedmodeldf[-c(27,28,29,30,31,32,33,
                                             34,35,36,37,38,39,
                                             5201, 5202,5203,5204,
                                             5205, 5206,5207,5208,5209,
                                             5210,5211,5212,5213,
                                             7151,7152,7153,7154,7155,7156
                                             ,7157,7158,7159,7160,7161,7162,7163),-4]
#Only way I could figure it out is via
countylist <- county.incidencee.by.year %>%
  arrange(year) %>%
  filter(year == 2006) %>%
  dplyr::select(county)

azzelinifun <- exp(-((d1)/35)^0.9)
diag(azzelinifun) <- 0
azzelinifun[(azzelinifun < 1e-100)] <- 0

#Now to weight matrix
localcountymat.w <- mat2listw(azzelinifun, style = "W")
localcountymat.m <- as(localcountymat.w, "CsparseMatrix") 
localcountymat <- as.matrix(localcountymat.m)


#Create forloop to create dataframe
#Create dataframe of county by year filled with incidence
#Retake first year english so that sentence makes sense
for(i in levels(mixedmodeldf$year)){
  countylist[,i] <- county.incidencee.by.year %>%
    arrange(year) %>%
    filter(year == i) %>%
    dplyr::select(incidence)
}
#view(countylist)

countylist <- as.data.frame(countylist[,-1])

#Now we need to multiply W_ij by every I_t 
#Another for loop

#Create a base layer to begin with
foidf <- county.incidencee.by.year %>%
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

#So problem with mixed modeldf is that the years continue once incidence has 
#occered. So it is redundant.

#Changing to disappear after incidence occures
newdf <- modeldataframe %>% subset((incidence == 0) | (incidence == 1 & previousyear == 0))

#Mixed model formula
formula <- incidence ~ (1|year) + (1|county) + offset(log(previnf + 0.0084))

foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))

#I hate it

predictions <- predict(foimm, type = "response")
preddf <- as.data.frame(predictions)

original <- na.omit(modeldataframe)
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

#Now we will make some dharma sims
library(DHARMa)
mmresid <- simulateResiduals(foimm)
plot(foimm)

#Doesnt really say much
#Now lets compare prediction to FOI
foilatlon <- left_join(predvizdf, latloninfo, "county")
foilatlon <- na.omit(foilatlon)

q <- ggplot() +
  geom_path(data = states, aes(x = long, y = lat, group = group)) +
  geom_point(data = foilatlon, aes(x = lon, y = lat, colour = predictions)) +
  geom_point(data = foilatlon, aes(x = lon, y = lat, colour = foi)) +
  coord_equal() +
  theme_minimal() 

q
#Should this be FOI or incidence that gets compared to the prediction
#Doesnt look very nice

modelresid <- read.csv("data/modelresiduals.csv")
firstyearinf <- modelresid %>% subset((incidence == 1 & previousyear == 0))


l <- ggplot() +
  geom_path(data = states, aes(x = long, y = lat, group = group)) +
  geom_point(data = firstyearinf, aes(x = lon, y = lat, colour = pearsonres)) +
  coord_equal() +
  theme_minimal() 

l

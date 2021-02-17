source("scripts/packages.R")
source("scripts/wns-presence.R")

shareuserlists <- read.csv("data/fullshareduserslist.csv") %>%
  rename(county = county1) %>%
  rename(year = i.year) %>%
  mutate(year = factor(year)) %>%
  filter(year != 2001) %>%
  filter(year != 2002) %>%
  filter(year != 2003) %>%
  filter(year != 2004) %>%
  filter(year != 2005) %>%
  filter(year != 2006)

#Going to build other matrix  
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
bothcounties <- melt(d1)
bothcounties <- bothcounties %>% rename(county = Var1) %>% rename(county2 = Var2) 

#Take known data
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))

county.incidence.by.year <- mixedmodeldf %>% filter(county != "Lewis-Washington" &
                                                      county != "King-Washington" &
                                                      county != "Plumas-California") %>%
  dplyr::select(-yc)

countylist <- county.incidence.by.year %>%
  arrange(year) %>%
  filter(year == 2006) %>%
  dplyr::select(county)

azzelinifun <- exp(-((d1)/9.508066)^0.512292358) #Use old vals for now
diag(azzelinifun) <- 0

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

countylist <- as.data.frame(countylist[,-1])

#Now we need to multiply W_ij by every I_t 
#Another for loop

#Create a base layer to begin with
foidf <- county.incidence.by.year %>%
  arrange(year) %>%
  filter(year == 2006) %>%
  dplyr::select(county)

#Can't order for some reason??

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

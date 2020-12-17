#Create azzalini power exp and test with different parameters abd compare MLE's
source("scripts/packages.R")

#Import dataset
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))


#Testing different types of coord to km types
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
mledf <- data.frame(NULL)

#Azzalini loop function 
for (param in seq(2,100,1)){
  
  
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

#Maybe just have to do it by hand
azzelinifun <- exp(-((d1)/param)^2)
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

#just glm model
foimodel <- glm(incidence ~ offset(log(previnf + 1)),
                data = newdf, family = binomial(link = "cloglog"))

mledf[param,1]<- logLik(foimodel)
}

x <- seq(1,200,1)
plot(x,mledf$V1)

loglikdf <- cbind(x,mledf$V1)
colnames(loglikdf) <- c("X","Y")
loglikdf <- as.data.frame(loglikdf)
p <- ggplot(loglikdf, aes(X,Y,group = 1)) +
  geom_line()+
  geom_point() 
  
ggsave("figures/loglikelihoodofazzalini.png", plot = p)



logLik(foimodel)

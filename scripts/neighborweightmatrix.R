#Playing around with spatial weight matrix

#simplified version to run
#import only necessary packages
library(sp)
library(spdep)
library(sf)
library(esri2sf)
library(tidyverse)
library(rgdal)
library(rgdal)

source("scripts/wns-presence.R")

presence.scrape <- read.csv("data/relevant-records.csv")

#Remove Cali and Wash for now
uniq.df<-presence.df %>% filter(.,STATEPROV != c("California","Washington"))
uniq.df<-uniq.df[!duplicated(uniq.df$county),]

# convert coords from county poly to centroid points for each county. Then reproject.
united.xy <- uniq.df$geoms %>% st_centroid() %>% 
  st_transform(., "+proj=longlat +datum=WGS84")

county_visits <- presence.scrape %>%
  filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
  group_by(county,year) %>% 
  distinct(User) %>%
  summarise(total = length(User))

just.gc <- presence.scrape %>% filter(!is.na(GC))

# get the number of shared users
shared.users<-NULL
for (i.year in unique(just.gc$year)) {
  s<-filter(just.gc,year == i.year)
  for (county1 in unique(s$county)) {
    for (county2 in rev(unique(s$county))) {
      num.shared<-length(intersect(as.character(s[which(s$county == county1),]$User),
                                   as.character(s[which(s$county == county2),]$User)))
      shared.users<-as.data.frame(rbind(shared.users,cbind(i.year,county1,county2,num.shared)))
    }
  }
}

shared.users %>% rename(year = i.year)
shared.users<-expand(shared.users,county1,county2,i.year) %>% left_join(shared.users)
shared.users<-shared.users[shared.users$county1!=shared.users$county2,]
shared.users$year<-as.numeric(as.character(shared.users$i.year))
shared.users$county1<-as.character(shared.users$county1)
shared.users$county2<-as.character(shared.users$county2)

#merge back to the original data
all.shared.users <- presence.df %>% 
  left_join(shared.users,by=c("year"="year","county"="county1"))

#fix number of users
#put in NA's where there was no traffic between caves
all.shared.users$num.shared <-  as.numeric(replace_na(all.shared.users$num.shared,0))

#create binary incidence value
all.shared.users$incidence <- ifelse(all.shared.users$YR_CONFIRM == " ",0,1)

county_rate<-all.shared.users %>% 
  arrange(date) %>%
  group_by(date) %>%
  summarise(county.inf.count = sum(incidence>0),
            uninf.counties = sum(incidence==0)) %>%
  mutate(inf.counties = cumsum(county.inf.count))

#cumulative number of infected and uninfected counties
all.shared.users$inf.counties<-county_rate[match(all.shared.users$date,county_rate$date),]$inf.counties
all.shared.users$uninf.counties<-county_rate[match(all.shared.users$date,county_rate$date),]$uninf.counties

#which counties are touching?
touching<-st_intersects(uniq.df$geoms,sparse = F)

touching.m <- as.matrix(touching)
rownames(touching.m)<-colnames(touching.m)<-uniq.df$county
touching.m2 <- reshape2::melt(touching.m)[reshape2::melt(upper.tri(touching.m))$value,]
names(touching.m2) <- c("county","county2","touching")

#merge gc weights with adjacency score: 1 = touching, 0 = not touching
both.weights<-left_join(all.shared.users,touching.m2,by=c("county","county2"))
both.weights$touching<-if_else(is.na(both.weights$touching),0,1)
#Model with just touching neighbors
model <- glm(as.factor(incidence) ~ as.factor(touching) ,data = both.weights, family = binomial(link = "cloglog"))
summary(model)

#This wrong I think. I think this is still global

site_visits <- presence.scrape %>%
  filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
  group_by(county,year,lon,lat) %>%
  distinct(User) %>%
  summarise(total = length(User)) %>% st_as_sf(coords=c("lat","lon"),crs = 4326)

#Method 1 of creating bubbles based on distance
n_local_neighbors <- lengths(st_is_within_distance(site_visits, dist = 100000))
#No intuitive way to include this 
#Knn did not work 
#https://spatialanalysis.github.io/lab_tutorials/Distance_Based_Spatial_Weights.html




#Original problem: I am a mess
#Repurcussions: GC-shared-users.csv
#This df is created, in geocache-weight.R but it doesnt
#match what is in the csv
#Problem 2: they have different number of cave visits, the one I can recreate
#only has 5 cave visits, the other, has 272 visits.
#The problem is counties are not labelled with states and there are 11 Marion
#counties in America.
#I also cant remake it because the script that makes it makes something completely
#else
#I am playing Scooby-doo and doing the mystery hunt but unsuccessful so far
#At a certain point do I just guess what state the unknown counties are in


#Going to take shared users from way over
#Merge with the melted foi matrix
source("scripts/packages.R")
source("scripts/wns-presence.R")

#All possible data sets
presence.scrape <- read.csv("data/relevant-records.csv")

presence.scrape %>% dplyr::select(year,lat,lon) %>% distinct() %>% nrow()  ## 52 lat/lon/year combinations
presence.scrape %>% dplyr::select(county) %>% distinct() %>% nrow()  ## 553 counties
pp <- (presence.scrape %>% dplyr::select(lat,lon) %>% distinct())
nrow(pp)

## quickie map
library(mapdata)
library(ggplot2)
usa <- map_data("usa")
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill=NA, colour="black") +
    stat_sum(data=presence.scrape,aes(x=lon,y=lat),alpha=0.5)

gcshareduser <- read.csv("data/gc-shared-users.csv") #This is the problem
#view(gcshareduser)
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))
#Remove Cali and Wash for now
uniq.df <- (presence.df
            %>% dplyr::filter(!STATEPROV %in% c("California","Washington"),
                              !duplicated(county)) #so we only have unique counties
)


## BMB: MAYBE ?
(presence.df
    %>% dplyr::filter(!STATEPROV %in% c("California","Washington"))
    %>% dplyr::select(county,geoms)
    %>% distinct()
##    %>% nrow()
)
                      
wnslat <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[1]])
wnslon <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[2]])

wns.center.coords <- cbind(wnslat, wnslon)

d1 <- distm(wns.center.coords, fun = distGeo)
dimnames(d1) <- list(uniq.df$county,uniq.df$county)

diag(d1) <- 0 
#convert from m to km
d1 <- d1/1000

#Creating shared users
united.xy <- uniq.df$geoms %>% st_centroid() %>% 
  st_transform(., "+proj=longlat +datum=WGS84")

keep_types <- c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")
county_visits <- presence.scrape %>%
  filter(Type %in% keep_types) %>%
  group_by(county,year,lon,lat) %>% 
  distinct(User) %>%
    summarise(total = length(User)) %>%
    arrange(lon,lat,year)
## or: summarise(total=length(unique(User))) ?

## Number of intersecting sites within a given radius (10km)
site_visits <- presence.scrape %>%
  filter(Type %in% keep_types) %>%
  group_by(GC,year,lon,lat) %>% 
  distinct(User) %>%
    summarise(total = length(User))  %>%
    arrange(lon,lat,year) %>%
    st_as_sf(coords=c("lat","lon"),crs = 4326)

## 
n_local_neighbors <- lengths(st_is_within_distance(site_visits, dist = 100000))

# need the number of visits at county level that match up with county centroids
# match centroid back to county
# then grab total vists

just.gc <- presence.scrape %>% filter(!is.na(GC))
## or drop_na(GC)

# get the number of shared users
shared.users<-NULL
for (i.year in unique(just.gc$year)) {
  s <- dplyr::filter(just.gc,year == i.year)
  for (county1 in unique(s$county)) {
    for (county2 in rev(unique(s$county))) {
      num.shared<-length(intersect(as.character(s[which(s$county == county1),]$User),
                                   as.character(s[which(s$county == county2),]$User)))
      shared.users<-as.data.frame(rbind(shared.users,cbind(i.year,county1,county2,num.shared)))
    }
  }
}

#Might just want to make dataframe from soley this
shared.users<-tidyr::expand(shared.users,county1,county2,i.year) %>% left_join(shared.users)
shared.users<-shared.users[shared.users$county1!=shared.users$county2,] #Gets rid of diag,
shared.users$i.year<-as.numeric(as.character(shared.users$i.year))
shared.users$county1<-as.character(shared.users$county1)
shared.users$county2<-as.character(shared.users$county2)

shared.users <- shared.users %>% dplyr::rename(year= i.year)

#Going back to playing with d1 which has the county-state names that I want
d1long <- reshape2::melt(d1)[reshape2::melt(upper.tri(d1))$value,]
biglongdf <- as.data.frame(d1long)
names(biglongdf) <- c("county","county2","distance")
biglongdf <- biglongdf %>% arrange(county)

#Just checking were okay
nrow(biglongdf)
n_distinct(biglongdf$county2)
#547*547
#we are not okay..

#I will ignore all my problems until they go away
#Join to the final model df
biggerdf <- left_join(mixedmodeldf,biglongdf, by = "county")
nrow(biggerdf) #Starting to worry what this does to my computer
#gonna clean out just a big
biggerdf <- biggerdf %>% dplyr::select(-c(distance, yc))

#BIG PROBLEM -> gcsharedusers and shared.users do not provide same list of users
#One has many more visits that the other

#Need to change the name so that we can left join
shared.users <- shared.users %>% rename(county = county1)
shared.users$year <- factor(shared.users$year)

#Now I want to join the shared users into this
allatonce <- left_join(biggerdf, shared.users, by= c("county", "county2", "year"))
nrow(allatonce)
allatonce$num.shared[is.na(allatonce$num.shared)] <- 0
visits <- (allatonce$num.shared > 0) #proves its garbage 
sum(visits) #5 total visits across 11 years and 547 counties

# summary of GC finds
# site coords are included here in order to grab the centroid later
site_visits <- relevant.records %>%
  filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
  group_by(GC,Year,coords.x1,coords.x2) %>%
  summarise(total = length(User))

site_visits.p <- as_Spatial(st_as_sf(site_visits,
                                       coords = c("coords.x1","coords.x2"), 
                                       crs = 4326, agr = "constant"))

# at the spatial locations of object x retrieves the indexes or attributes from spatial object y
# index for sites that match up with polys
# these are the county polygons that match up with sites
num<-sp::over(site_visits.p, as_Spatial(presence.df$geoms),fn=NULL)

# pull out a unique list of county polys
uniq.df<-presence.df[na.omit(unique(num)),]

# convert coords from county poly to centroid points for each county. Then reproject.
united.xy <- uniq.df$geoms %>% st_centroid() %>% 
  st_transform(., "+proj=longlat +datum=WGS84")

# need the number of visits at county level that match up with county centroids
# match centroid back to county
# then grab total vists

county_visits <- relevant.records %>%
  # getting rid of visits that don't matter
  filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
  # getting rid of users that don't visit sites in other counties
  group_by(User) %>%
  mutate(total = length(unique(county))) %>% filter(total > 1)

# get the number of shared users
shared.users<-NULL
for (year in unique(as.character(county_visits$wns.map.yr))) {
  s<-subset(county_visits,wns.map.yr==year)
  for (county1 in unique(s$county)) {
    for (county2 in rev(unique(s$county))) {
      num.shared<-length(intersect(s[which(s$county == county1),]$User,
                          s[which(s$county == county2),]$User))
      shared.users<-as.data.frame(rbind(shared.users,cbind(year,county1,county2,num.shared)))
    }
  }
}

shared.users<-shared.users[shared.users$county1!=shared.users$county2,]

# create list of all combinations
all.years <- shared.users %>% expand(year,county1,county2)

# merge with the data we have, filling in gaps
all.shared.users<-merge(shared.users,all.years,all=TRUE)

# create binary incidence value
all.shared.users$incidence <- ifelse(is.na(all.shared.users$num.shared),0,1)

# fix year
all.shared.users$date <- ymd(all.shared.users$year)

# fix number of users
# put in NA's where there was no traffic between caves
all.shared.users$num.shared <-  as.numeric(replace_na(all.shared.users$num.shared,0))

county_rate<-all.shared.users %>% 
  arrange(date) %>%
  group_by(date) %>%
  summarise(county.count = sum(incidence)) %>%
  mutate(inf.counties = cumsum(county.count))

# cumulative number of infected counties
all.shared.users$inf.counties<-county_rate[match(all.shared.users$date,county_rate$date),]$inf.counties

write.csv(all.shared.users,"data/gc-shared-users.csv")

  
# summary of GC finds - GC 
# polygons are included here in order to grab the centroid later

# pull out a unique list of county polys
# not from Cali or Wash for now!
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

# need the number of visits at county level that match up with county centroids
# match centroid back to county
# then grab total vists

just.gc <- presence.scrape %>% filter(!is.na(GC))

# get the number of shared users
shared.users<-NULL
for (i.year in unique(just.gc$year)) {
  s<-filter(just.gc,year == i.year)
  for (county1 in unique(s$county)) {
    for (county2 in rev(unique(s$county))) {
        num.shared<-length(intersect(as.character(s[which(s$county == county1),]$User),
                                     as.character(s[which(s$county == county2),]$User)))
        shared.users<-as.data.frame(rbind(shared.users,cbind(year,county1,county2,num.shared)))
      }
    }
  }

shared.users<-expand(shared.users,county1,county2,year) %>% left_join(shared.users)
shared.users<-shared.users[shared.users$county1!=shared.users$county2,]
shared.users$year<-as.numeric(as.character(shared.users$year))
shared.users$county1<-as.character(shared.users$county1)
shared.users$county2<-as.character(shared.users$county2)

# merge back to the original data
all.shared.users <- presence.df %>% 
  left_join(shared.users,by=c("year"="year","county"="county1"))

# fix number of users
# put in NA's where there was no traffic between caves
all.shared.users$num.shared <-  as.numeric(replace_na(all.shared.users$num.shared,0))

# create binary incidence value
all.shared.users$incidence <- ifelse(all.shared.users$YR_CONFIRM == " ",0,1)

county_rate<-all.shared.users %>% 
  arrange(date) %>%
  group_by(date) %>%
  summarise(county.inf.count = sum(incidence>0),
            uninf.counties = sum(incidence==0)) %>%
  mutate(inf.counties = cumsum(county.inf.count))

# cumulative number of infected and uninfected counties
all.shared.users$inf.counties<-county_rate[match(all.shared.users$date,county_rate$date),]$inf.counties
all.shared.users$uninf.counties<-county_rate[match(all.shared.users$date,county_rate$date),]$uninf.counties

# which counties are touching?
touching<-st_intersects(uniq.df$geoms,sparse = F)

touching.m <- as.matrix(touching)
rownames(touching.m)<-colnames(touching.m)<-uniq.df$county
touching.m2 <- reshape2::melt(touching.m)[reshape2::melt(upper.tri(touching.m))$value,]
names(touching.m2) <- c("county","county2","touching")

# merge gc weights with adjacency score: 1 = touching, 0 = not touching
both.weights<-left_join(all.shared.users,touching.m2,by=c("county","county2"))
both.weights$touching<-if_else(is.na(both.weights$touching),0,1)

# save
write.csv(both.weights,"data/gc-shared-users.csv")

ggplot(uniq.df$geoms)+
  borders("world") +
  borders("state") +
  geom_sf(aes(fill=uniq.df$WNS_MAP_YR))+
  coord_sf(xlim = c(-100, -60), ylim = c(32, 50))+
  # coord_sf(xlim = c(-100, -57.5), ylim = c(35, 50))+
  theme_bw()
ggsave("figures/counties-shared-users.png",plot=last_plot(),dpi=300)

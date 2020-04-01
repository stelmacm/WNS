#### create a spatial weight matrix
# crude method, adjacency measured using centroids of counties

site_visits <- relevant.records %>%
  filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
  group_by(county,Year,coords.x1,coords.x2) %>%
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

# construct neighbour list from county centroids
county.n <- poly2nb(as_Spatial(presence.df$geoms))

# spatial weights of neighbourlist
county.w <- nb2listw(county.n,zero.policy = TRUE)

county.w$neighbours

# add names so that we can match these with the df later
names(county.w$weights) <- presence.df$rownumber

# replace NULL with 0
county.w$weights[sapply(county.w$weights, is.null)] <- 0
weights.long<-unlist(county.w$weights,use.names = T)

# spatial weight matricies from neighbour list
 county.m <- nb2mat(county.n, style = "B", zero.policy = T)

list(time = Sys.time(), tempfile = tempfile())
  

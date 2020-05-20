# summary of GC finds - GC 
# polygons are included here in order to grab the centroid later

# pull out a unique list of county polys
# not from Cali or Wash for now!
uniq.df<-presence.df %>% filter(.,STATEPROV != c("California","Washington"))
uniq.df<-uniq.df[!duplicated(uniq.df$county),]

# convert coords from county poly to centroid points for each county. Then reproject.
united.xy <- uniq.df$geoms %>% st_centroid() %>% 
  st_transform(., "+proj=longlat +datum=WGS84")

full.yc<-expand.grid(uniq.df[,c("year","county")])
full.yc$yc<-paste0(full.yc$year,"-",full.yc$county)
uniq.df$yc<-paste0(uniq.df$year,"-",uniq.df$county)

full.yc$incidence<-ifelse(full.yc$yc %in% uniq.df$yc,1,0)

mixeddataframe <- unique(full.yc)

mixedmodeldf <- mixeddataframe[order(mixeddataframe$county,mixeddataframe$year),]

write.csv(x = full.yc,file = "data/full-county-year-incidence.csv")
write.csv(x = mixedmodeldf, file = "data/mixedmodeldata.csv")

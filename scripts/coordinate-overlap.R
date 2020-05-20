 all_results_merge <- scraped %>%
    filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
    left_join(select(gc_filtered_dat,-X.1,-X,-lon,-lat)) %>%
    mutate(county = paste0(trimws(gsub(pattern = "County.*$",replacement = "",county)),"-",province.state),
    Year = lubridate::year(lubridate::ymd(Date)))
  
  # count users at each site per year  
  # we do this again at a later step, but this is useful to tell if there are any sites that don't get visited in a year
  geocache.locs <- all_results_merge %>% 
    group_by(GC, Year,lat,lon) %>%  
    distinct(User) %>% 
    tally(name = "total")
  
  # order of coords matters!
  geocache.coords <- as_Spatial(st_as_sf(geocache.locs, coords = c("lat","lon"), crs = 4326, agr = "constant"))
  
  # create an index of matching polygons from presence data
  index.df <- as.data.frame(sp::over(geocache.coords, presence.poly, returnList = F))
  
  # bind results with coordinates of geocache sites (they are in the same order as the oringal index)
  geocache.presence.df <- left_join(presence.df,all_results_merge,by=c("STATEPROV"="province.state","county"="county","year"="Year"))
  
  # merge with our scraped dataset of geocache records
  # we want all, because there are a few sites that did not get visited some years
  scraped$year<-lubridate::year(scraped$Date)
  presence.scrape <-left_join(geocache.presence.df, scraped, by = c("GC", "year","User","Title","Log","lon","lat","Date","Type"))

  # recode sites without visits to 0
  is.na(presence.scrape$numfinds) <- 0
  
  # fix date format
  presence.scrape <- presence.scrape %>%
    mutate(date = lubridate::ymd(date),
    wns.map.yr = lubridate::ymd(gsub("-.+", "/01/01", WNS_MAP_YR)),
    r.suspect = lubridate::ymd(gsub("-.+", "/01/01", YR_SUSPECT)),
    yr.confirm = lubridate::ymd(gsub("-.+", "/01/01", YR_CONFIRM)))
  
  # we don't was to discard visits that would have preceeded introdution of infections!
  # filter out geocache logs that were not made before WNS was "suspected"
  write.csv(select(presence.scrape,-"geoms"),"data/relevant-records.csv")
  
  # relevant.records.wpoly<-left_join(relevant.records,presence.df,by=c("county"="COUNTYNAME"))
  # saveRDS(relevant.records.wpoly,file = "data/relevant-records-withPoly.RDS")
  
  list(time = Sys.time(), tempfile = tempfile())

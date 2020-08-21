require(esri2sf)

url2 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/2"
url5 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/5"

get_wns_presence <- function(first_=url2, second_url=url5) {
    df2 <- as.data.frame(esri2sf(url2))
    df5 <- as.data.frame(esri2sf(url5))
    presence.df <- rbind(df2, df5)
    presence.df <- presence.df %>% 
        mutate(county = paste0(trimws(gsub(pattern = "County.*$",replacement = "",COUNTYNAME)),"-",STATEPROV),
               date=lubridate::dmy(paste0("01-01",gsub(pattern = "-.*$",replacement = "",WNS_MAP_YR))),
               year=lubridate::year(date),
               rownumber = 1:nrow(.))
    presence.poly <- as_Spatial(presence.df$geoms)
    return(list(df=presence.df, poly=presence.poly))
}

source("scripts/filter_descriptions.R")

clean_url <- function(x) {
    return(x
           %>% tolower()
           ## %>% stringr::str_remove("[^[:alnum:] ]") ## remove non-alphanumeric
           %>% gsub(pattern="[^[:alnum:] ]",replacement="")
           %>% stringr::str_replace_all(" ","-")
           )
}

#These are from coordinate-overlap.R
#Merge the results 
all.results.merge <- function(scrapedresults, gc_filtered_dat) {
    all_results_merge <- scrapedresults %>%
        filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
        left_join(select(gc_filtered_dat,-X.1,-X,-lon,-lat)) %>%
        mutate(county = paste0(trimws(gsub(pattern = "County.*$",replacement = "",county)),"-",province.state),
               Year = lubridate::year(lubridate::ymd(Date)))
     #??

#create geocache locations from merged location
#merge will be all_results_merged from above
#count users at each site per year  
#we do this again at a later step, but this is useful to tell if there are any sites that don't get visited in a year
    geocache.locs <- all_results_merge %>% 
        group_by(GC, Year,lat,lon) %>%  
        distinct(User) %>% 
        tally(name = "total")
    return(list(all_results_merge = all_results_merge, geocache.locs = geocache.locs)) #??
}

#Make them coords to polygons babbbyyyyy
relevant_records <- function(scraped, geocache.locs, presence.df, all_results_merge, presence.poly, outfile) { 
    #Needs: (scraped, geocache.locs, presence.df, all_results_merge, presence.poly, outfile)
    #Ambitious effort to keep the names the same
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
    write.csv(select(presence.scrape,-"geoms"),outfile)
    
}

#Placed the shapes for county fix into functions instead of plan
#Am I suppose to file in these?
counties = read.csv("data/all-counties.csv") %>%
    separate(1, c("county", "state.province", "Country"), sep = "\t") %>% 
    distinct()

#Canadian `Counties` shapefile
#file_in seems awkward infront of a shape file 
can.shape = readOGR("shape/lcd_000b16a_e/lcd_000b16a_e.shp") #embarassing I cant push this

#USA Counties shape file
#This isnt even from a file :/
usa.shape = maps::map("county", regions = unique(counties[counties$Country == "USA", ]$state.province), fill = T)
#County fix function
#Takes in can.shape, usa.shape, presence.df, presence.poly
county.fix <- function(presence.df, presence.poly, can.shape = can.shape, usa.shape = usa.shape){
    
    rownames(presence.df) <- names(presence.poly)
    poly.df <- SpatialPolygonsDataFrame(presence.poly, presence.df)
    counties$county <- gsub(pattern = " / (.*)", "", counties$county)
    can.shape <- can.shape[can.shape@data$PRNAME %in% c("Quebec / Québec", 
                                                        "Ontario", 
                                                        "Prince Edward Island / Île-du-Prince-Édouard", 
                                                        "Nova Scotia / Nouvelle-Écosse", 
                                                        "New Brunswick / Nouveau-Brunswick"), ]
    
    can.shape.id <- as.character(paste(can.shape$CDNAME, can.shape$PRNAME, sep = ", "))
    can.shape.id <- gsub(pattern = " / (.*)", "", can.shape.id)
    can.shape.id <- gsub(pattern = "Quebec", "Québec", can.shape.id)
    can.shape.sf <- st_as_sf(can.shape)
    can.shape.sf$id <- can.shape.id
    can.shape.sf <- st_transform(can.shape.sf, "+proj=longlat +datum=WGS84")
    
    usa.shape.id <- paste(as.character(sapply(strsplit(usa.shape$names, ","), 
                                              function(x) x[2])), as.character(sapply(strsplit(usa.shape$names, ","), function(x) x[1])), sep = ", ")
    usa.shape <- map2SpatialPolygons(usa.shape, IDs = usa.shape.id, proj4string = CRS("+proj=longlat +datum=WGS84"))
    usa.shape.id <- NULL
    for (i in 1:length(usa.shape)) {
        x <- slot(usa.shape@polygons[[i]], "ID")
        usa.shape.id <- rbind(usa.shape.id, x)
    }
    usa.shape.sf <- st_as_sf(usa.shape)
    usa.shape.sf$id <- usa.shape.id
    united.poly <- rbind(can.shape.sf[, "id"], usa.shape.sf[, 2:1])
    return(united.poly = united.poly)
}

relevant.records.data <- read.csv("data/relevant-records.csv")

#Creating a spatial weight matrix
#Takes relevant records as arguement. returns weight matrix
spatial.weight.matrix <- function(relevant.records) {
    
    
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
    
    return(county.m)
}

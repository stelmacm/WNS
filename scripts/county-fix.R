#### script for creating a spatial polygon dataframe 
#### for all counties/municipalities in US/Canada
#### where we have WNS records from

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
  list(time = Sys.time(), tempfile = tempfile())

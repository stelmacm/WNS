#### create a spatial weight matrix
# crude method, adjacency measured using centroids of counties

  # convert coords from poly to centroid points of counties. Then reproject.
  united.xy <- st_transform(united.poly, 29101) %>% st_centroid() %>% 
    st_transform(., "+proj=longlat +datum=WGS84")
  
  # construct neighbour list from county centroids
  county.n <- poly2nb(as_Spatial(united.poly$geometry), row.names = united.poly$id)
  
  # spatial weights of neighbourlist
  county.w <- nb2listw(county.n,zero.policy = TRUE)
  
  # add names so that we can match these with the df later
  names(county.w$weights) <- united.poly$id
  
  # replace NULL with 0
  county.w$weights[sapply(county.w$weights, is.null)] <- 0
  weights.long<-unlist(county.w$weights,use.names = T)
  
  # spatial weight matricies from neighbour list
  # county.m <- nb2mat(county.n, style = "B", zero.policy = T)
  
  list(time = Sys.time(), tempfile = tempfile())
  
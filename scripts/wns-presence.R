##### WNS Presence shape file
  url2 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/2"
  df2 <- as.data.frame(esri2sf(url2))
  url5 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/5"
  df5 <- as.data.frame(esri2sf(url5))
  presence.df <- rbind(df2, df5)
  presence.df$rownumber = 1:nrow(presence.df)
  presence.poly <- as_Spatial(presence.df$geoms)
  # list(time = Sys.time(), tempfile = tempfile())


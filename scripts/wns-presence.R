##### WNS Presence shape file
  url2 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/2"
  df2 <- as.data.frame(esri2sf(url2))
  url5 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/5"
  df5 <- as.data.frame(esri2sf(url5))
  presence.df <- rbind(df2, df5)
  presence.df <- presence.df %>% 
    mutate(county = paste0(trimws(gsub(pattern = "County.*$",replacement = "",COUNTYNAME)),"-",STATEPROV),
           date=lubridate::dmy(paste0("01-01",gsub(pattern = "-.*$",replacement = "",WNS_MAP_YR))),
           year=lubridate::year(date),
           rownumber = 1:nrow(.))
  presence.poly <- as_Spatial(presence.df$geoms)
  list(time = Sys.time(), tempfile = tempfile())

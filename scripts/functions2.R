
get_wns_presence <- function(first_url, second_url) {
  df2 <- as.data.frame(esri2sf(url2))
  df5 <- as.data.frame(esri2sf(url5))
  presence.df <- rbind(df2, df5)
  presence.df <- presence.df %>% 
    mutate(county = paste0(trimws(gsub(pattern = "County.*$",replacement = "",COUNTYNAME)),"-",STATEPROV),
           date=lubridate::dmy(paste0("01-01",gsub(pattern = "-.*$",replacement = "",WNS_MAP_YR))),
           year=lubridate::year(date),
           rownumber = 1:nrow(.))
  presence.poly <- as_Spatial(presence.df$geoms)
  #return(list(df=presence.df, poly=presence.poly))
}

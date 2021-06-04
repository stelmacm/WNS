#Bernoullisimasafunction doesnt want to work
#Will just do it as a script rather than a function

coords <- read.csv("data/latlonofcounty.csv")

simresults[simresults == "La C\x99te-De-Gasp\x8e-Quebec"] <- 'La Côte-De-Gaspé-Quebec'
simresults[simresults == "L'\x83rable-Quebec"] <- "L'Érable-Quebec"
simresults[simresults == "Lotbini̬re-Quebec"] <- 'Lotbinière-Quebec'
simresults[simresults == "Le Haut-Saint-Laurent-Qu̩bec"] <- 'Le Haut-Saint-Laurent-Québec'
simresults[simresults == "Memphr̩magog-Quebec"] <- 'Memphrémagog-Quebec'
simresults[simresults == "Le Haut-Saint-Fran\x8dois-Quebec"] <- 'Le Haut-Saint-François-Quebec'
simresults[simresults == "Antoine-Labelle-Qu̩bec"] <- 'Antoine-Labelle-Québec'
simresults[simresults == "La C̫te-de-Beaupr̩-Qu̩bec"] <- 'La Côte-de-Beaupré-Québec'

pp <- left_join(simresults, coords, by = "county")

theme_set(theme_bw())


library(mapdata)
library(maptools)
library(grid)
#I am so excited to have found Canada
usa <- map_data("usa")
canada <- map_data("worldHires", "Canada")

#I am mildly aroused
NAmap <- ggplot() + geom_polygon(data = usa, 
                                 aes(x=long, y = lat, group = group), 
                                 fill = "white", 
                                 color="black") +
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") + 
  coord_fixed(xlim = c(-105, -50),  ylim = c(25, 55), ratio = 1.2) + 
  theme(line = element_blank(),
        text = element_blank(), 
        panel.background = element_rect(fill = "steelblue"))


createScaleBar <- function(lon,lat,distanceLon,distanceLat,
                           distanceLegend, dist.units = "km"){
  # First rectangle
  bottomRight <- gcDestination(lon = lon, lat = lat, bearing = 90, 
                               dist = distanceLon, dist.units = dist.units,
                               model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, 
                           dist = distanceLat, dist.units = dist.units, 
                           model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottomRight[1,"long"],
                           bottomRight[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],
                             lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottomRight2 <- gcDestination(lon = lon, lat = lat, bearing = 90, 
                                dist = distanceLon*2, dist.units = dist.units,
                                model = "WGS84")
  rectangle2 <- cbind(lon = c(bottomRight[1,"long"], bottomRight[1,"long"],
                              bottomRight2[1,"long"], bottomRight2[1,"long"],
                              bottomRight[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], 
                            lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, 
                         dist = distanceLegend, dist.units = dist.units, 
                         model = "WGS84")
  onTop2 <- onTop3 <- onTop
  onTop2[1,"long"] <- bottomRight[1,"long"]
  onTop3[1,"long"] <- bottomRight2[1,"long"]
  
  legend <- rbind(onTop, onTop2, onTop3)
  legend <- data.frame(cbind(legend, text = c(0, distanceLon, distanceLon*2)),
                       stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, 
              legend = legend))
}

#Scale bar includes some arrow nonsense. Look to eliminate that
scaleBar <- function(lon, lat, distanceLon, distanceLat, 
                     distanceLegend, dist.unit = "km", rec.fill = "white",
                     rec.colour = "black", rec2.fill = "black", 
                     rec2.colour = "black", legend.colour = "black", 
                     legend.size = 3, orientation = TRUE, arrow.length = 500,
                     arrow.distance = 300, arrow.North.size = 6){
  laScaleBar <- createScaleBar(lon = lon, lat = lat, 
                               distanceLon = distanceLon, 
                               distanceLat = distanceLat, 
                               distanceLegend = distanceLegend, 
                               dist.unit = dist.unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = laScaleBar$rectangle, 
                             aes(x = lon, y = lat), fill = rec.fill, 
                             colour = rec.colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = laScaleBar$rectangle2, 
                             aes(x = lon, y = lat), fill = rec2.fill, 
                             colour = rec2.colour)
  
  # Legend
  scaleBarLegend <- annotate("text", label = paste(laScaleBar$legend[,"text"],
                                                   dist.unit, sep=""), 
                             x = laScaleBar$legend[,"long"], 
                             y = laScaleBar$legend[,"lat"], 
                             size = legend.size, 
                             colour = legend.colour, fontface="bold")
  
  res <- list(rectangle1, rectangle2, scaleBarLegend)
  
  if(orientation){# Add an arrow pointing North
    coordsArrow <- createOrientationArrow(scaleBar = laScaleBar, 
                                          length = arrow.length, 
                                          distance = arrow.distance,
                                          dist.unit = dist.unit)
    arrow <- list(geom_segment(data = coordsArrow$res, 
                               aes(x = x, y = y, xend = xend, yend = yend)),
                  annotate("text", label = "N", 
                           x = coordsArrow$coordsN[1,"x"], 
                           y = coordsArrow$coordsN[1,"y"], 
                           size = arrow.North.size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}

NAmap <- NAmap + scaleBar(lon = -67.5, lat = 25, distanceLon = 500, 
                          distanceLat = 100, distanceLegend = 200, 
                          dist.unit = "km", legend.size = 4, 
                          orientation = FALSE)

#Take known data
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))

startingcounties <- mixedmodeldf %>% filter(year == "2007") %>% filter(incidence == 1)

initialcounties <- left_join(startingcounties, coords, by = "county")
#Now to add the points of incidence
library(colorspace)
library(viridis)
infplot <- NAmap + geom_point(data = pp, aes(x = lon, y = lat, colour = year), size = 0.7) +
  geom_point(data = initialcounties, aes(x = lon, y = lat), colour = "black", size = 0.7) +
  scale_colour_viridis(discrete = TRUE)


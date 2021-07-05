#Questions for BMB July 5th
source("scripts/packages.R")
library(ggpubr)
#From this we will make geom_line plots or similar plots that Kala made
#I should do actual vs predicted.
#Add Geom_ribbon of 95% CI
obsvsproj <- read.csv("data/obsvspredspatialstats.csv")
meandistplot <- ggplot(data = obsvsproj) + geom_line(aes(x = years, y = meandist, colour = type)) +
  scale_x_continuous(name = " ", breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  labs(y = "Kilometers") +
  ggtitle("Mean Distance per Year") +
  theme_minimal() +
  scale_fill_discrete(name = NULL)

maxdistplot <- ggplot(data = obsvsproj) + geom_line(aes(x = years, y = maxdist, colour = type)) +
  scale_x_continuous(name = " ", breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  labs(y = "Kilometers") +
  ggtitle("Max Distance per Year") +
  theme_minimal() +
  scale_fill_discrete(name = NULL)

infayearplot <- ggplot(data = obsvsproj) + geom_line(aes(x = years, y = infayear, colour = type)) +
  scale_x_continuous(name = " ", breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  labs(y = "New Infections") +
  ggtitle("New Infections per Year") +
  theme_minimal() +
  scale_fill_discrete(name = NULL)

library(gridExtra)
library(grid)
#grid.arrange(meandistplot, maxdistplot, infayearplot)

ggarrange(meandistplot, maxdistplot, infayearplot,
          #labels = c("Mean Distance per Year", "Max Distance per Year", "Infections per Year"),
          ncol = 1, nrow = 3, common.legend = TRUE, legend = "bottom")
#QUESTION:
#I want to put 95% CI ribbon on the projected. How should I go about calculating the 95% CI for this?
#Or should this not be something I want to do.

#PLOT 2
library(mapdata)
usa <- map_data("usa")
canada <- map_data("worldHires", "Canada")
source("scripts/bernoullivizfunction.R")
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))

county.incidence.by.year <- mixedmodeldf %>% filter(county != "Lewis-Washington" &
                                                      county != "King-Washington" &
                                                      county != "Plumas-California") %>%
  dplyr::select(-c(yc,id)) %>% arrange(county)

coords <- read.csv("data/latlonofcounty.csv")

county.incidence.by.year[county.incidence.by.year == "La C\x99te-De-Gasp\x8e-Quebec"] <- "La Côte-De-Gaspé-Quebec"
county.incidence.by.year[county.incidence.by.year == "L'\x83rable-Quebec"] <- "L'Érable-Quebec"
county.incidence.by.year[county.incidence.by.year == "Lotbini̬re-Quebec"] <- 'Lotbinière-Quebec'
county.incidence.by.year[county.incidence.by.year == "Le Haut-Saint-Laurent-Qu̩bec"] <- 'Le Haut-Saint-Laurent-Québec'
county.incidence.by.year[county.incidence.by.year == "Memphr̩magog-Quebec"] <- 'Memphrémagog-Quebec'
county.incidence.by.year[county.incidence.by.year == "Le Haut-Saint-Fran\x8dois-Quebec"] <- 'Le Haut-Saint-François-Quebec'
county.incidence.by.year[county.incidence.by.year == "Antoine-Labelle-Qu̩bec"] <- 'Antoine-Labelle-Québec'
county.incidence.by.year[county.incidence.by.year == "La C̫te-de-Beaupr̩-Qu̩bec"] <- 'La Côte-de-Beaupré-Québec'

countyindcidencecoordsperyear <- left_join(county.incidence.by.year, coords) %>% mutate(lastyearincidence = lag(incidence)) %>%
  filter(incidence == 1 & lastyearincidence == 0)

countyincidenceperyearplot <- ggplot() + geom_polygon(data = usa, 
                                                      aes(x=long, y = lat, group = group), 
                                                      fill = "white", 
                                                      color="black") +
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") + 
  coord_fixed(xlim = c(-105, -50),  ylim = c(25, 55), ratio = 1.2) + 
  theme(line = element_blank(),
        text = element_blank(), 
        panel.background = element_rect(fill = "lightblue")) + 
  scaleBar(lon = -67.5, lat = 25, distanceLon = 500, 
           distanceLat = 100, distanceLegend = 200, 
           dist.unit = "km", legend.size = 4, 
           orientation = FALSE) +
  geom_point(data = countyindcidencecoordsperyear, aes(x = lon, y = lat, colour = factor(year))) +
  scale_colour_viridis(discrete = TRUE) +
  guides(color = guide_legend(override.aes = list(size = 3) ) )
#Kind of stuck. Need BMB help for legend

countyincidenceperyearplot

#QUESTION:
#How should I go about fixing the legend so that it displays the years that are attributed to each
#I am able to put the legend where I want it on the plot but the years simply don't show.

#QUESTION:
#I made boxplots of the parameter confidence intervals but then I realized boxplots are for quantiles.
#What kind of plot would be recommended for the parameter estimates and their confidence intervals
#Perhaps just https://clauswilke.com/dataviz/visualizing-uncertainty.html#fig:cocoa-CI-vs-n

#QUESTION:
#Have played a bit with library(MRFcov) for Markov Random Fields. Am I going in the right direction?

#QUESTION:
#Should I visualize bias in anyway? Or its not that critical and I should just write about it and thats fine. 

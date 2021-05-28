#Script for BMB meeting May 28
#Lets start off with comparisons of BMBTMB and SBSTMB
#Chaos for the next few lines
#These are the same plots from last week. Just so that we have them
source("scripts/packages.R")
library(mapdata)
library(maptools)
library(grid)
library(TMB)
library(tmbstan)
library(rstan)
library(shinystan)
sbsmodellikelihood <- read.csv("data/TMBnllprofile.csv")

likesurface <- ggplot(data = sbsmodellikelihood) +
  geom_tile(aes(x = d, y = theta, fill = gridlikelihood - min(gridlikelihood))) + #yikes im so confused as to what I am geom tiling
  facet_grid(logsdcounty + logsdyears ~ rho + offsets) +
  scale_fill_viridis_c(trans = "log10")
likesurface  

bmbmodellikelihood <- read.csv("data/BMBTMBllgrid.csv")

likesurface2 <- ggplot(data = bmbmodellikelihood) +
  geom_tile(aes(x = d, y = theta, fill = gridlikelihood2 - min(gridlikelihood2))) + #yikes im so confused as to what I am geom tiling
  facet_grid(logsdcounty + logsdyears ~ rho + offsets) +
  scale_fill_viridis_c(trans = "log10")
likesurface2

library(gridExtra)
grid.arrange(likesurface, likesurface2, ncol=2)
#Likelihood plots look exactly the same beside one another

#Now going to compare the gradient of the two
bmbgradients <- read.csv("data/bmbtmbgridgradient.csv")
sbsgradients <- read.csv("data/sbsgridgradient.csv")
#goal is to compare the two together to see if they give the same answers 
dgrdiff <- bmbgradients$dgr - sbsgradients$dgr
thetadiff <- bmbgradients$thetagr - sbsgradients$thetagr
rhogrdiff <- bmbgradients$rhogr - sbsgradients$rhogr
offsetgrdiff <- bmbgradients$offsetsgr - sbsgradients$offsetsgr
logsdcountydiff <- bmbgradients$logsdcountygr - sbsgradients$logsdcountygr
logsdyeardiff <- bmbgradients$logsdyearsgr - sbsgradients$logsdyearsgr

gradientdifferences <- data.frame(dgrdiff, thetadiff, rhogrdiff, offsetgrdiff,logsdcountydiff, logsdyeardiff)

#K screw it
dgraddiff <- ggplot(data = gradientdifferences) +
  geom_point(aes(x = 1:nrow(gradientdifferences), y = abs(dgrdiff)))
dgraddiff

thetagraddiff <- ggplot(data = gradientdifferences) +
  geom_point(aes(x = 1:nrow(gradientdifferences), y = abs(thetadiff)))

rhograddiff <- ggplot(data = gradientdifferences) +
  geom_point(aes(x = 1:nrow(gradientdifferences), y = abs(rhogrdiff)))

offsetgraddiff <- ggplot(data = gradientdifferences) +
  geom_point(aes(x = 1:nrow(gradientdifferences), y = abs(offsetgrdiff)))

logsdcountygraddiff <- ggplot(data = gradientdifferences) +
  geom_point(aes(x = 1:nrow(gradientdifferences), y = abs(logsdcountydiff)))

logsdyeargraddiff <- ggplot(data = gradientdifferences) +
  geom_point(aes(x = 1:nrow(gradientdifferences), y = abs(logsdyeardiff)))

allthedifferences <- grid.arrange(dgraddiff, thetagraddiff, rhograddiff, offsetgraddiff, logsdcountygraddiff, logsdyeargraddiff,
                                  ncol = 2, nrow = 3)

#Wasn't too sure what other creative plot to do... 
#Maybe something funky comparing the results of each gradient with its param?
#I dont even know
zz <- cbind(sbsgradients, gradientdifferences)
ggplot(data = zz) +
  geom_point(aes(x = d, y = dgr)) +
  facet_grid(rho ~ offsets)

#Now we will be looking at the results of a bunch of optims from various starting points
paramsandoutputs <- read.csv("data/optimgridtesting.csv")

optimresults <- ggplot(data = paramsandoutputs) +
  geom_point(aes(x = d, y = (optimll - min(optimll))))
optimresults
#Bimodality babbyyyyyy

compareseq <- ggplot(data = paramsandoutputs) +
  geom_tile(aes(x = dseq, y = thetaseq, fill = optimll)) +
  facet_grid(rhoseq ~ offsetseq) + 
  scale_fill_viridis_c(trans = "log10")
compareseq

#Now why dont we look into the posterior simulations of each of these...
#Interesting thing that happens is with the optimal step by step params
#I cannot create a weight matrix because of underflow? (values turn to 0 of azzalinimat)

p <- c(-10.1052, 0.670263)
d <- exp(p[1])
theta <- (p[2])
azzelinifun <- exp(-((d1)/d)^theta)
#This whole matrix is filled with 0's...

#Now if we do this simulation at the other dip of this likelihood surface
simresults <- read.csv("data/TMBpriorsimresults.csv") %>% 
  mutate(year = factor(year)) 

coords <- read.csv("data/latlonofcounty.csv")

simresults[simresults == "La C\x99te-De-Gasp\x8e-Quebec"] <- 'La Côte-De-Gaspé-Quebec'
simresults[simresults == "L'\x83rable-Quebec"] <- "L'Érable-Quebec"
simresults[simresults == "Lotbini̬re-Quebec"] <- 'Lotbinière-Quebec'
simresults[simresults == "Le Haut-Saint-Laurent-Qu̩bec"] <- 'Le Haut-Saint-Laurent-Québec'
simresults[simresults == "Memphr̩magog-Quebec"] <- 'Memphrémagog-Quebec'
simresults[simresults == "Le Haut-Saint-Fran\x8dois-Quebec"] <- 'Le Haut-Saint-François-Quebec'
simresults[simresults == "Antoine-Labelle-Qu̩bec"] <- 'Antoine-Labelle-Québec'
simresults[simresults == "La C̫te-de-Beaupr̩-Qu̩bec"] <- 'La Côte-de-Beaupré-Québec'

mm <- left_join(simresults, coords, by = "county")
theme_set(theme_bw())

usa <- map_data("usa")
canada <- map_data("worldHires", "Canada")

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
NAmap + geom_point(data = mm, aes(x = lon, y = lat, colour = year), size = 0.7) +
  geom_point(data = initialcounties, aes(x = lon, y = lat), colour = "black", size = 0.7) +
  scale_colour_viridis(discrete = TRUE)

#So I have not done any spatial statistics on this sim yet but this is looks meh reasonable
#I need to compare to actual a bit more but this looks promising..

#Now lets look at some shiny stuff... 1 divergent..should I be concerned?
#There are a few other things wrong with this but its a start
obj3stan <- readRDS("data/stepbysteppriorSTAN.RDS")
shinystan::launch_shinystan(obj3stan)


source("scripts/creatingfunctiondf.R")
compile("scripts/stepbystepwithpriors.cpp")
dyn.load(dynlib("scripts/stepbystepwithpriors"))


countyeffect = rep(0,548)
yeareffect = rep(0,13)
dparam = 50
thetaparam = 1.4
rhoparam = 0.6  
a = 0.001

#It does not accept the values from the function into the list? Get NaN for obj3$fn(obj3$par)
dd1 <- list(dist = orderedmat, dim = 548, SM = bigsharedusers, numberofyears = 13,
            fullcountyincidence = countylist, dmean = log(505), dsd = log(165), thetamean = 1.25, 
            thetasd = .4583)#, offsetmean = 0.5, offsetsd = 0.15)
#Hard coded offset priors bc logitnormal looks like something I dont want to play with too much

pp1 <- list(log_d = log(dparam), theta = thetaparam, logit_rho = qlogis(rhoparam),  log_offsetparam = log(a),
            logsd_County = 0, logsd_Year = 0,
            YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

obj3 <- MakeADFun(data=dd1, parameters=pp1, DLL="stepbystepwithpriors",
                  silent=TRUE,
                  random=c("YearRandomEffect","CountyRandomEffect"))

nlminboptprior <- with(obj3, nlminb(start = par, obj = fn, gr=gr,
                                    control=list(trace=10)))

nlminboptprior$par #Yoinks
#I say yoinks because the params become the same thing as their priors...
#Need to change that but am struggling with the most basic changes of priors in TMB
#Cant even implement dunif without tmb hating me
#another view of where we see the bimodality
dprof <- tmbprofile(obj3, "log_d")
plot(dprof) #This is it

rhoprof <- tmbprofile(obj3, "logit_rho")
plot(rhoprof)

#TMB hating me
compile("scripts/TMBmodelwithpriors.cpp")

#Thank you for putting up with my chaotic coding
#Questions: Bimodality: Seems like the issue fixes itself? Or is the naive?
#Priors: How the heck do I implement in TMB? Probably atomic functions

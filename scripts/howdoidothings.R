#Questions for BMB July 9th
#Question 1:
#How many plots is too many plots? (vague question I know)

#Question 2:
#I want to compare rho = 0 and rho = 1 (all shared users and no shared users) and I want to make a plot
#of their spatial statistics. I likely cannot confidence intervals on this unless I create a new model and
#find the intervals using the posterior

#Question 3:
#I am having a tough time talking more about biological explanations for my model
#other than the spatial statistics

#Question 4:
#I am having the realization that it is ok to talk about the obvious (ex. if theta is less than 1 on exp
#dist then we have a probelem). I guess I should assume nothing is obvious?

#Question 5: (more of a comment)
#I think I want to make a nice pairs plot

#Question 6:
#I have discussed a little bit about HMC diagnostics and what that is like. Should I make that a more
#important component of my methods sections? Should I be using Vehtari et al.?

#Question 7:
#I want to find a way to compare the different computational considerations I had done for the model.
#I have run benchmark tests on several but am unsure if it makes sense to just talk about them or maybe I
#should put them in a table or a bar graph or something?

#Question 8:
#How many citations should I have? I am still looking for a few (ex. recent Reed Frost paper)


source("scripts/packages.R")
source("scripts/wns-presence.R")
library(mgcv)
library(mgcViz)
library(sf)
objectmulti <- presence.df$geoms
objectpoly <- st_cast(objectmulti, "POLYGON")

bb <- read.csv("data/mixedmodeldf.csv")
dd <- bb %>% dplyr::select(c(county,foi, year))

presencedd <- left_join((presence.df %>% dplyr::select(c(county,geoms))),
                        dd, by = "county") %>% na.omit() %>% dplyr::filter(year == 2011) %>%
  mutate(county = as.factor(county))

presencedd$geoms <- st_cast(presencedd$geoms, "POLYGON")

smooth.construct.mrf.smooth.spec(
    list(term = "county",
         xt = objectpoly,
         bs.dim = 20,
         fixed = FALSE,
         dim = 1,
         p.order = NA,
         by = "NA",
         label = "junk",
         id = NULL,
         sp = NULL),
    data = presencedd,
    knots = list()
)

xp <- list(polys = objectpoly)
names(xp) <- attr(xp, "county")
gam(foi ~ s(county, bs = "mrf", xt = xp), data = presencedd, method = "REML") #huh???

#Got frusterated and gave up because there are other things I want to work on

library(ggpubr)
#From this we will make geom_line plots or similar plots that Kala made
#I should do actual vs predicted.
##Add Geom_ribbon of 95% CI

## geom_ribbon( lwr, upr, alpha, fill, colour = NA)
##
## Chapter 7 of Ecological Models and Data in R
##
## (0) IF thing you want to compute is a fairly trivial
## function of the parameters, you could reparameterize your model
## to use the predicted 'thing' as the parameter
## e.g. y = a + b*x + c*x^2  and I want to compute the peak location and height
## reparameterize as y =  h + s*(x-x0)^2
##
## (1) delta method (TMB can do this automatically if you
##  include the desired quantity in an ADREPORT())
##  (assumes weak nonlinearity over the range of the parameter uncertainty)
##
## (2) 'posterior predictive intervals' or 'parametric bootstrap [not really]'
## sample from the _assumed_ MVN sampling distribution of the parameters (i.e.
##  MASS::mvrnorm(n, mu = <estimated parameters>, Sigma = <vcov>)
## then feed these values through your prediction function, find quantiles etc.
## if you're frequentist, this is sort of cheesy - assumes sampling distribution is MVN
## if you're Bayesian, less cheesy, but still assumes posterior is MVN
##
## (3) _importance sampling_; correct #2 for the actual computed likelihood.
##
## (4) real parametric bootstrap (super-expensive); take _estimated_ parameters, simulate
## many realizations, re-fit the model, take that ensemble of parameters to predict
##
## (5) real Bayesian posterior samples = posterior predictive intervals

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
##Or should this not be something I want to do.

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

countyincidenceperyearplot <- (ggplot()
  + geom_polygon(data = usa,
                 aes(x=long, y = lat, group = group),
                 fill = "white",
                 color="black")
  + geom_polygon(data = canada, aes(x=long, y = lat, group = group),
               fill = "white", color="black")
  + coord_fixed(xlim = c(-105, -50),  ylim = c(25, 55), ratio = 1.2)
  + theme(line = element_blank(),
          ## text = element_blank(),
          panel.background = element_rect(fill = "lightblue"))
  + scaleBar(lon = -67.5, lat = 25, distanceLon = 500,
           distanceLat = 100, distanceLegend = 200,
           dist.unit = "km", legend.size = 4,
           orientation = FALSE)
  + geom_point(data = countyindcidencecoordsperyear, aes(x = lon, y = lat, colour = factor(year)))
  + scale_colour_viridis(discrete = TRUE)
  + guides(color = guide_legend(override.aes = list(size = 3) ))
)
#Kind of stuck. Need BMB help for legend

countyincidenceperyearplot

#QUESTION:
#How should I go about fixing the legend so that it displays the years that are attributed to each
#I am able to put the legend where I want it on the plot but the years simply don't show.

#QUESTION:
#I made boxplots of the parameter confidence intervals but then I realized boxplots are for quantiles.
#What kind of plot would be recommended for the parameter estimates and their confidence intervals
#Perhaps just https://clauswilke.com/dataviz/visualizing-uncertainty.html#fig:cocoa-CI-vs-n

## geom_pointrange() {minimalist} OR geom_point() + geom_errorbar() {more traditional} OR geom_violin()
## (for posterior distributions, although this is usually overkill unless the posterior distribution is
## an interesting shape
## use geom_pointrange() for 95% intervals, then overlay geom_linerange for 50% intervals
## (or geom_linerange(skinny) + geom_linerange(fat) + geom_point())

#QUESTION:
#Have played a bit with library(MRFcov) for Markov Random Fields. Am I going in the right direction?

library(mgcv)
## Load Columbus Ohio crime data (see ?columbus for details and credits)
data(columb)       ## data frame
data(columb.polys) ## district shapes list
xt <- list(polys=columb.polys) ## neighbourhood structure info for MRF
## how do we call smooth.construct.mrf.smooth.spec
## directly? (i.e. what does s() do before it passes stuff on to smooth.* ?
debug(smooth.construct.mrf.smooth.spec)
b <- gam(crime ~ s(district, bs="mrf", xt=xt, k = 20),
         data=columb, method="REML")
## names(object) passed to smooth.*
##  [1] "term"    "bs.dim"  "fixed"   "dim"     "p.order" "by"      "label"
##  [8] "xt"      "id"      "sp"

fakesmooth_args <- list(term="district", ## relevant column in 'data'
                   bs.dim = 20, ## basis dimension
                   fixed = FALSE, ## should we hold this constant? (we want to estimate it)
                   dim = 1, ## OK???
                   p.order = NA,
                   by = "NA",
                   label = "s(whatever)",
                   xt = list(polys = columb.polys),
                   id = NULL,
                   sp = NULL)

ss <- smooth.construct.mrf.smooth.spec(fakesmooth_args, data = columb, knots = list())
## put this in with a eta <- eta + X * (sigma*b) [adding spatially structured/penalized noise]
## and nll -= sum(dnorm(b)) [penalty term]

#QUESTION:
#Should I visualize bias in anyway? Or its not that critical and I should just write about it and thats fine.


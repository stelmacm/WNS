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


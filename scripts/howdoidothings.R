library(mgcv)
library(sp)
library(gstat)
library(tidyverse)

test <- read.csv("data/samplestations.csv")
test$LATITUDE <- as.numeric(test$LATITUDE)
test$LONGITUDE <- as.numeric(test$LONGITUDE)

ww <- gam(MIN ~ LATITUDE + LONGITUDE, data = test) #This seems to be a bit wild.
#from here I would predict ww

coordinates(test) <- ~ LONGITUDE + LATITUDE
class(test)
bbox(test)
test.vgm <- variogram(MIN~LATITUDE + LONGITUDE, test)
test.fit <- fit.variogram(test.vgm, model = vgm(c("Exp", "Sph", "Mat")))

plot1 <- test %>% as.data.frame %>%
  ggplot(aes(LONGITUDE, LATITUDE)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points with station measurements")
plot1

x <- seq(-104.8489, -60.05, 0.01)
y <- seq(25.6475, 58.72, 0.01)
test.grid <- expand.grid(x, y) #Large

plot2 <- test.grid %>% 
  ggplot(aes(Var1, Var2)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points at which to estimate temp at")
#Pretty silly plot if we thing about it...
#Probably not worth rendering

#library(gridExtra)
#grid.arrange(plot1, plot2, ncol = 2)


#OK so this is with a clean environment now
#I have a fear I am looking at the wrong type residual
library(tidyverse)
library(gstat)
library(sp)
library(maptools)
library(reshape)

realvspreddata <- read.csv("data/realvspred.csv") %>%
  mutate(year.x = factor(year.x))

#Tried many types of ways of interpolation 
#All of them look incredible rough and ugly

testyear <- realvspreddata %>% filter(year.x == 2009)
testyear <- na.omit(testyear)

akimatest <- interp(x = testyear$lon, y = testyear$lat, z = testyear$rawred, 
                    xo = seq(min(testyear$lon), max(testyear$lon), by = .1), 
                    yo = seq(min(testyear$lat), max(testyear$lat), by = .1),
                    duplicate = "strip")
image(akimatest, col = rainbow(16, start = .6, end = .1))
#different viz of akima
#examp is lon lat eresid

examp <- testyear %>% dplyr::select(lon, lat, rawred)
d1 <- with(examp, interp(x = testyear$lon, y = testyear$lat, z = (testyear$eresid), 
                         xo = seq(min(testyear$lon), max(testyear$lon), by = .1), 
                         yo = seq(min(testyear$lat), max(testyear$lat), by = .1),
                         duplicate = "strip"))

d2 <- melt(d1$z, na.rm = TRUE)
head(d2)
names(d2) <- c("x", "y", "eresid")

d2$lon <- d1$x[d2$x]
d2$lat <- d1$y[d2$y]

usa <- map_data("usa")

#Very VERY ugly. I'm not happy with this but other things never worked
ggplot() +
  geom_tile(data = d2, aes(x = lon, y = lat, fill = eresid)) +
  geom_path(data = usa, aes(x = long, y = lat, group = group)) + 
  scale_fill_gradient(low = "blue", high = "red", na.value = "white") +
  theme_classic() 
#Side question, how do I change alpha


#OPTION 2

coordinates(testyear) = ~ lon + lat
#possible to create 
x.range <- as.integer(range(testyear@coords[,1]))
y.range <- as.integer(range(testyear@coords[,2]))

#Changing grid bc obviously this sucks
x.range <- as.integer(c(-110, -51))
y.range <- as.integer(c(25, 54))

grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=.5),
                   y=seq(from=y.range[1], to=y.range[2], by=.5))

coordinates(grd) = ~ x + y
gridded(grd) <- TRUE

#Now doing IDW

idw<-idw(formula = eresid ~ 1, locations = testyear, newdata = grd)

idw.output=as.data.frame(idw)
names(idw.output)[1:3]<-c("long","lat","var1.pred")

w <- ggplot(data = idw.output, aes(x = long, y = lat)) +
  geom_tile(data = idw.output, aes(fill = var1.pred)) +
  geom_path(data = usa, aes(x = long, y = lat, group = group)) +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_equal()
w
#Now just fit the variogram ez pz right?
semivariog<-variogram(eresid~1, locations=testyear, data=testyear)

model.variog<-vgm(psill=0.2, model="Sph", nugget=.75, range=20) 
#spline method hates me so spherical for now...
fit.variog<-fit.variogram(semivariog, model.variog)
plot(semivariog, fit.variog)

#Krige
krig<-krige(formula=eresid ~ 1, locations=testyear, newdata=grd, model=model.variog)
#All the warnings are skips :(
#Maybe I need to reconsider formula....
krig.output=as.data.frame(krig)
names(krig.output)[1:3]<-c("long","lat","var1.pred")

cry<- ggplot(data = krig.output, aes(x = long, y = lat)) +
  geom_tile(data = krig.output, aes(fill = var1.pred)) +
  theme_classic()
#geom_path(data = states, aes(x = long, y = lat, group = group)) +
#scale_fill_gradient(low = "blue", high = "red")
cry #Change variable later

library(tidyverse)
usa <- map_data("usa")
states <- map_data("state")
#Read in data
modelresid <- read.csv("data/modelresiduals.csv")
#Data includes model variables, glm features,
#glmm resid are called pearsonresid

#Just taking 1 year to play around with and do all the tests on 
testyear <- modelresid %>% filter(year == 2010)

#plot of infected vs uninfected

p <- ggplot() +
  geom_path(data = states, aes(x = long, y = lat, group = group)) +
  geom_point(data = testyear, aes(x = lon, y = lat, colour = factor(incidence))) +
  coord_equal() +
  theme_minimal()

p

#Now find out where incidence occurs 

occured <- testyear %>% filter(incidence == 1)
prevoccured <- testyear %>% filter(previousyear == 1)

t <- ggplot() +
  geom_path(data = states, aes(x = long, y = lat, group = group)) +
  geom_point(data = occured, aes(x = lon, y = lat, alpha = 0.1) ,colour = "red") +
  geom_point(data = prevoccured, aes(x = lon, y = lat, alpha = 0.1), colour = "yellow") +
  coord_equal() +
  theme_minimal() +
  guides(alpha = FALSE)
  
t

testyear2 <- modelresid %>% filter(year == 2014)

occured2 <- testyear2 %>% filter(incidence == 1)
prevoccured2 <- testyear2 %>% filter(previousyear == 1)

t2 <- ggplot() +
  geom_path(data = states, aes(x = long, y = lat, group = group)) +
  geom_point(data = occured2, aes(x = lon, y = lat, alpha = 0.1) ,colour = "red") +
  geom_point(data = prevoccured2, aes(x = lon, y = lat, alpha = 0.1), colour = "yellow") +
  coord_equal() +
  theme_minimal() +
  guides(alpha = FALSE)


t2

#azzalini distance matrix %*% shared users mat
# "foi" = above mat %*% incidence vector
library(tidyverse)
library(DHARMa)
library(ggeffects)
library(lme4)

newdf <- read.csv("data/shareusermodeldataframe.csv")
nanewdf <- na.omit(newdf)
#Mixed model formula
formula <- incidence ~ (1|year) + (1|county) + offset(log(previnf + 1))
#Still need to run optimizer for params
foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))

#Doing DHARMa sim from the model bc thats how DHARMa works
simoutput <- simulateResiduals(fittedModel = foimm)
plot(simoutput)
hist(simoutput)

summary(foimm)

#a graph
plot(fitted(foimm)~na.omit(offset(log(newdf$previnf))))

#Marginal effects and estimated marginal means 
cc <- ggpredict(foimm, "county")
yy <- ggpredict(foimm, "year")

#meh
plot(cc)
plot(yy)

#Now I should just make predictions
trials <- data.frame(x = seq(0,1, length.out = nrow(nanewdf)))
nanewdf$fit <- fitted(foimm, trails$x , type = "response")
plot(trials$fit)

#I dont know why I did select filter select year but I'm sure there must have been 
#a logical reason for it right???
latloninfo <- modelresid %>% dplyr::select(county, year, lon,lat) %>%
  filter(year == 2008) %>% dplyr::select(-year)

modelinfo <- left_join(nanewdf, latloninfo, by = "county")

usa <- map_data("usa")
states <- map_data("state")

year2008 <- modelinfo %>% filter(year == 2008)

#I should probably look into setting the scale from 1 to 0
p <- ggplot() +
  geom_path(data = states, aes(x = long, y = lat, group = group)) +
  geom_point(data = year2008, aes(x = lon, y = lat, colour = fit)) +
  coord_equal() +
  theme_minimal() +
  scale_colour_gradient(low = "yellow", high = "red") 
p

#On my to do list:
# run loglik for hyperparams
# turn shared users matrix into a row normalize weight matrix (rows sum to 1)
# Do some better predictions and plots. Plot residuals.
# Reading some mobility and infection papers: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6542035/
# concern would be that shared users matrix can be relatively sparse for certain years
# read.csv("data/sharedcountyuserslong.csv")
# Maybe want to try and create (1|county1) + (1|county2) as part of model
# I think then I would have to change the whole structure of FOI in the formula

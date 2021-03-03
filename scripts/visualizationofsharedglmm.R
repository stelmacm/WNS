#azzalini distance matrix %*% shared users mat
# "foi" = above mat %*% incidence vector
library(DHARMa)
library(magrittr)
library(ggeffects)
library(sjmisc)
library(lme4)
library(splines)
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

cc <- ggpredict(foimm, "county")
yy <- ggpredict(foimm, "year")

#Uh oh
plot(cc)
plot(yy)

#lattice dotplot
library(lattice)
library(cowplot)
dotplot(ranef(foimm))$county #Yikes
dotplot(ranef(foimm))$year 

#Try some simulations
nanewdf <- na.omit(newdf)
nanewdf$pred <-simulate(~ (1|year) + (1|county) + offset(log(previnf + 1)),
                                                       newdata = nanewdf,
                                                       family = binomial(link = "cloglog"),
                        newparams=list(beta=1, theta=c(1,2))) #gross

overdisp_fun = function(model) {
  sum( residuals(model, type = "pearson")^2)/df.residual(model)
}
overdisp_fun(foimm)

overdisp_fun2 <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun2(foimm)

modelfit.all <- lme4::allFit(foimm)
ss <- summary(modelfit.all) #Ugly 10 different ways




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
# Do some better predictions and plots
# Reading some mobility and infection papers: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6542035/
# concern would be that shared users matrix can be relatively sparse for certain years
# read.csv("data/sharedcountyuserslong.csv")
# What next?
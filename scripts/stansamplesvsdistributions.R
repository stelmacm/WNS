#Script creating plots and info of stan object
#Will start with the old stan object and then add the new one once its made
library(shinystan)
library(tidyverse)
#First object
obj3stan <- readRDS("data/stepbysteppriorSTAN.RDS")
#now 2nd stan object
priorobj <- readRDS("data/stepbysteppriorstanmodel.RDS")
#Ugh okay so we have an issue.... It actually is a bad model

bayesplot::color_scheme_set("brightblue")
pairs1 <- pairs(obj3stan, pars = c("log_d", "theta", "logit_rho", "log_offsetparam", "logsd_Year", "logsd_County", "lp__"))
#interesting.... theta and d catch my eye
pairs2 <- pairs(priorobj, pars = c("log_d", "theta", "logit_rho", "log_offsetparam", "logsd_Year", "logsd_County", "lp__"))


list_of_draws <- rstan::extract(obj3stan , pars = c("log_d", "theta", "logit_rho",
                                                    "log_offsetparam", "logsd_Year", 
                                                    "logsd_County", "lp__")) #pars is an arguement as well...
#print(names(list_of_draws))

list_of_draws2 <- rstan::extract(priorobj , pars = c("log_d", "theta", "logit_rho",
                                                    "log_offsetparam", "logsd_Year", 
                                                    "logsd_County", "lp__")) #pars is an arguement as well...

scaleparamsamples <- data.frame(val = rnorm(4000, log(505), log(146.666)))

#I mean clearly this is just the wrong stan object right???
scaleplot <- ggplot() +
  geom_density(data = scaleparamsamples, aes(val), alpha = 0.2, fill = "red") +
  geom_density(data = as.data.frame(list_of_draws$log_d), aes(list_of_draws$log_d), fill = "blue", alpha = 0.2) +
  ggtitle("Scaling Parameter")

#So something is wrong but I think the fix is ez pz
thetasamples <- data.frame(val = rnorm(4000, 1.25 , .4583))
thetaplot <- ggplot() +
  geom_density(data = thetasamples, aes(val), alpha = 0.2, fill = "red") +
  geom_density(data = as.data.frame(list_of_draws$theta), aes(list_of_draws$theta), fill = "blue", alpha = 0.2) +
  ggtitle("Theta Parameter")

rhosamples <- data.frame(val = qlogis(rnorm(4000, .5, .1666))) 
rhoplot <- ggplot() +
  geom_density(data = rhosamples, aes(val), alpha = 0.2, fill = "red") +
  geom_density(data = as.data.frame(list_of_draws$logit_rho), aes(list_of_draws$logit_rho), fill = "blue", alpha = 0.2)+
  ggtitle("Rho Parameter")

offsetsamples <- data.frame(val = rnorm(4000, log(2), log(1.5))) #Idk what to put as the mean and sd
offsetplot <- ggplot() +
  geom_density(data = offsetsamples, aes(val), alpha = 0.2, fill = "red") +
  geom_density(data = as.data.frame(list_of_draws$log_offsetparam), aes(list_of_draws$log_offsetparam), fill = "blue", alpha = 0.2) +
  ggtitle("Offset Parameter")

library(gridExtra)
library(grid)

firststan <- grid.arrange(scaleplot, thetaplot, rhoplot, offsetplot)
#Too much thinking to get only 1 legend on plot...brain too small...
#Blue is stan samples, red is distribution

scaleplot2 <- ggplot() +
  geom_density(data = scaleparamsamples, aes(val), alpha = 0.2, fill = "red") +
  geom_density(data = as.data.frame(list_of_draws2$log_d), aes(list_of_draws2$log_d), fill = "blue", alpha = 0.2) +
  ggtitle("Scaling Parameter")

thetaplot2 <- ggplot() +
  geom_density(data = thetasamples, aes(val), alpha = 0.2, fill = "red") +
  geom_density(data = as.data.frame(list_of_draws2$theta), aes(list_of_draws2$theta), fill = "blue", alpha = 0.2) +
  ggtitle("Theta Parameter")

rhoplot2 <- ggplot() +
  geom_density(data = rhosamples, aes(val), alpha = 0.2, fill = "red") +
  geom_density(data = as.data.frame(list_of_draws2$logit_rho), aes(list_of_draws2$logit_rho), fill = "blue", alpha = 0.2)+
  ggtitle("Rho Parameter")

offsetplot2 <- ggplot() +
  geom_density(data = offsetsamples, aes(val), alpha = 0.2, fill = "red") +
  geom_density(data = as.data.frame(list_of_draws2$log_offsetparam), aes(list_of_draws2$log_offsetparam), fill = "blue", alpha = 0.2) +
  ggtitle("Offset Parameter")

secondstan <- grid.arrange(scaleplot2, thetaplot2, rhoplot2, offsetplot2)
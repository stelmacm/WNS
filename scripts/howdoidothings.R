library(tidyverse)
library(DHARMa)
library(sf)
library(sp)
library(spdep)
library(lme4)

bestloglik <- read_csv("data/azzaliniparam.csv") %>%
  mutate(theta = factor(theta))

zoomedinalittle <- bestloglik %>% filter(ID > 5 & ID < 75)

ggplot(data = zoomedinalittle, aes(x = ID, y = Loglikelihood)) +
  geom_line(aes(color = theta))

#I dont think this one makes sense but that is what has the best MLE
#Unsure basically which one to select if theyre all so close
#The thing is that all of these are not very "sparse" and the matrix is very dense


optimalone <- read_csv("data/mixedmodelpoint933.csv") %>%
  mutate(year = factor(year)) %>%
  mutate(incidence = factor(incidence))

formula <- incidence ~ (1|year) + (1|county) + offset(log(previnf + 1))

foimm <- glmer(formula, data = optimalone, family = binomial(link = "cloglog"))
summary(foimm)

#Doing DHARMa sim from the model bc thats how DHARMa works
simoutput <- simulateResiduals(fittedModel = foimm)
plot(simoutput)
hist(simoutput)
#Now at 1.4 and 70 and compare models

optimalone2 <- read_csv("data/mixedmodel1point470.csv") %>%
  mutate(year = factor(year)) %>%
  mutate(incidence = factor(incidence))

foimm2 <- glmer(formula, data = optimalone2, family = binomial(link = "cloglog"))
summary(foimm2)

#Doing DHARMa sim from the model bc thats how DHARMa works
simoutput2 <- simulateResiduals(fittedModel = foimm2)
plot(simoutput2)
hist(simoutput2)

optimalone3 <- read_csv("data/mixedmodel2and96.csv") %>%
  mutate(year = factor(year)) %>%
  mutate(incidence = factor(incidence))

foimm3 <- glmer(formula, data = optimalone3, family = binomial(link = "cloglog"))
summary(foimm3)

#Doing DHARMa sim from the model bc thats how DHARMa works
simoutput3 <- simulateResiduals(fittedModel = foimm3)
plot(simoutput3)
hist(simoutput3)

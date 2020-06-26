#This is the script where I will play around with glmmLasso
#I will attempt other methods of penalized regression
#In an attempt to avoid the bayesian world

#Afterwards we wil play around with the bayesian 
#programs like stan and shinystan
#did not get to shiny stan

#questions regarding CSE 
#https://cse.mcmaster.ca/graduate-studies/program-requirements.html
#https://cse.mcmaster.ca/images/CSE_Handbook_2018_19.pdf
#contradict one another
library(tidyverse)
library(lme4)
library(ggplot2)
library(brglm2)
library(glmmLasso)
library(glmmTMB)
library(brms)
library(rstan)
library(rstanarm)
library(glmnet)
library(caret)
set.seed(101)
realdf <- (read.csv("data/mixedmodeldf.csv")  ## switch to read.csv (encodings)
           %>% as_tibble()
           %>% filter(year>2006)
           %>% mutate(previousyear=lag(incidence))
           %>% select(id,year,county, yc, incidence, previousyear)
           %>% mutate_at("year", factor)
)

#changed the NA to have a 0 
ijustwannaseesomething <- realdf
ijustwannaseesomething$previousyear[1] <- 0
ijustwannaseesomething <- ijustwannaseesomething[,-c(1,4)]

formula <- incidence ~ (1|year) + (1|county) +
  offset(log(previousyear + 1))

#glmmLasso does not take offsets and it occured to me that regularization
#might not make sense to do if there is an offset
#Lasso is better for large data sets right?
#Ridge was tricky bc tibble to model.matrix was having trouble
#Caret is great for stuff like this

#This doesn't seem right bc no cloglog and I don't know that this
#works the same for mixed models as it does for regular studd

training.samples <- ijustwannaseesomething$incidence %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- ijustwannaseesomething[training.samples, ]
test.data <- ijustwannaseesomething[-training.samples, ]

xvars <- model.matrix(incidence~., ijustwannaseesomething)[,-1]
yvar <- ijustwannaseesomething$incidence

glmnet(xvars, yvar, family = "binomial", alpha = 1, lambda = NULL)

#gets stuck here
cv.lasso <- cv.glmnet(xvars, yvar, alpha = 1, family = "binomial")
#this is what it would be with the ridge part
model <- glmnet(xvars, yvars, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)

#glmmLasso encounters many problems with NA's and matrix formats?
#went through source code and got stuck a few places
l1 <- glmmLasso(fix = incidence ~ (log(previousyear+1)), rnd = NULL,
                data = ijustwannaseesomething, lambda = 10, family = binomial(link = "cloglog"),
                control = list(0))

#stan worked for me 2 days and then ran into errors everytime after
#Error in unserialize(node$con) : error reading from connection
#Calls: <Anonymous> -> slaveLoop -> makeSOCKmaster
#But this only happens after I add multiple cores?
#I probably should have asked for help earlier and I apologize for not doing so
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

formula <- incidence ~ (1|year) + (1|county) +
  offset(log(previousyear + 1))

m1 <- glmmTMB(formula ,
              family=binomial(link = "cloglog"), data=ijustwannaseesomething)

m2 <- stan_glmer(formula ,
                 family=binomial(link = "cloglog"), data=ijustwannaseesomething)

m3 <- stan_glmer(formula ,
                       family=binomial(link = "cloglog"), data=realdf)
launch_shinystan(m2)
launch_shinystan(m3)

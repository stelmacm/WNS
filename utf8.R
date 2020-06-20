library(lme4)
library(rstanarm)
library(shinystan)

county <- scan(file="data/encoding.csv",what=character(1),sep="\n")
county <- readr::read_table(file="data/encoding.csv")[[1]]
dd <- expand.grid(county=county,
                  year=factor(2000:2004),
                  rep=1:4)

set.seed(101)
dd$y <- simulate(~1 + (1|county) + (1|year),
                 family=binomial,
                 size=1,
                 newdata=dd,
                 newparams=list(beta=1,theta=c(1,1)))[[1]]
m1 <- stan_glmer(y~1 + (1|county) + (1|year), data=dd, cores=4, family=binomial)
launch_shinystan(m1)



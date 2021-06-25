#June 25th meeting with BMB
source("scripts/packages.R")
#Previously on the exponential prior of theta

#DATA_SCALAR(theta);
#nll -= pow(exp(- sqrt(pow(theta - thetamean ,2)) / thetapriorscale), thetapowerscale);
#Where thetapriorscale was 5.67 and thetapowerscale was 2 (as per the get_gnorm values returned)
#This kept resulting in very wonky errors and a chaotic theta parameter which made no sense
firstmodel <- readRDS("data/TMBmodelwithpriorsstan2.RDS")

list_of_draws <- rstan::extract(firstmodel , pars = c("log_d", "theta", "logit_rho",
                                                 "log_offsetparam", "logsd_Year", 
                                                 "logsd_County", "lp__"))
#thetasamples <- data.frame(val = rgnorm(4000, mu = 1.5 , alpha = 5.67, beta = 2))
thetaplot <- ggplot() +
  #geom_density(data = thetasamples, aes(val), alpha = 0.2, fill = "red") +
  geom_density(data = as.data.frame(list_of_draws$theta), aes(list_of_draws$theta), fill = "blue", alpha = 0.2) +
  ggtitle("Theta Parameter")
thetaplot

#Now I changed the thetapowerscale to .7 and the thetapowerscale to 3 and recieved a much better answer but was still getting very large answers
secondmodel <- readRDS("data/TMBmodelsparserandpriors.RDS")

list_of_draws2 <- rstan::extract(secondmodel , pars = c("log_d", "theta", "logit_rho",
                                                      "log_offsetparam", "logsd_Year", 
                                                      "logsd_County", "lp__"))
thetaplot <- ggplot() +
  #geom_density(data = thetasamples, aes(val), alpha = 0.2, fill = "red") +
  geom_density(data = as.data.frame(list_of_draws2$theta), aes(list_of_draws2$theta), fill = "blue", alpha = 0.2) +
  ggtitle("Theta Parameter")
thetaplot
#Still dont like it...

#I was sick of it so I change theta in terms of log
#DATA_SCALAR(logtheta);
#Type theta = exp(logtheta);
#nll += thetapowerscale * (sqrt(pow(logtheta - thetamean,2))/thetapriorscale);

bestmodel <- readRDS("data/stanmodelobject1.RDS")

list_of_draws3 <- rstan::extract(bestmodel , pars = c("log_d", "logtheta", "logit_rho",
                                                        "log_offsetparam", "logsd_Year", 
                                                        "logsd_County", "lp__"))
thetaplot <- ggplot() +
  #geom_density(data = thetasamples, aes(val), alpha = 0.2, fill = "red") +
  geom_density(data = as.data.frame(list_of_draws3$logtheta), aes(list_of_draws3$logtheta), fill = "blue", alpha = 0.2) +
  ggtitle("Theta Parameter")
thetaplot

#kind of pointy but seems cool.
#Everything seems great about this except for the low-ish n-eff of logcounty_sd (random effects prior)
shinystan::launch_shinystan(bestmodel)

#Had a version that looked like this
#modelobjstanextra <- tmbstan(modelobj, iter = 3000, control = list(adapt_delta = 0.99)) #Just seeing what happens. Maybe we need to do more
#running for 13 hours and only reached 300 iterations on every chain...
#AWS caps out after 15 minutes of computation
#Did sims with this set of results. Looks good
#Need to do more stuff like interpolating the error with these set params 

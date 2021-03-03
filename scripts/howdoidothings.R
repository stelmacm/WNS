source("scripts/packages.R")
source("scripts/sharedusersglmm.R")
source("scripts/buildmodelquick.R")

#Original model with shared users
#Doing DHARMa sim from the model bc thats how DHARMa works
simoutput <- simulateResiduals(fittedModel = foimm)
plot(simoutput)
hist(simoutput)

pred <- simoutput$fittedPredictedResponse
pred2 <- rank(pred, ties.method = "average")
pred2 <- pred2/max(pred2)
plot(pred2,simoutput$scaledResiduals)
plot(pred,simoutput$scaledResiduals)

#a graph
plot(fitted(foimm)~na.omit(offset(log(newdf$previnf))))

dotplot(ranef(foimm))$county #Yikes
dotplot(ranef(foimm))$year 
set.seed(10)
#Try some simulations
nanewdf <- na.omit(newdf)
nanewdf$pred <-simulate(~ (1|year) + (1|county) + offset(log(previnf + 1)),
                        newdata = nanewdf,
                        family = binomial(link = "cloglog"),
                        newparams=list(beta=1, theta=c(1,2))) #gross
#Not even worth plotting

#Now doing it with better params
#Doing DHARMa sim from the model bc thats how DHARMa works
simoutput2 <- simulateResiduals(fittedModel = model2)
plot(simoutput2)
hist(simoutput2)
plotResiduals(simoutput2) #oof


summary(model2) #highest loglik from guess and check params

dotplot(ranef(model2))$county 
dotplot(ranef(model2))$year 

#All ugly, but I guess expected since the optim param
#Having trouble with optim in
#source("scripts/optimparamforsharedusers.R)
#I need to eead into this since I am unsure where things break



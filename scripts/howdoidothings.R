#Meeting notes

#Last time optim just was taking forever. Now having a problem of non convergence
#Basically gets caught in a valley and can't actually get out to find global optim
#I am convinced the surface looks very rough

#Looked into where we get the super crazy values from in optim.
#Turns out the model does not even build. The reason for this (guessing) is
#super small values for the force of infection. Putting control on glmer helps.

#Finally when optim stops running I get this
#Error in pwrssUpdate(pp, resp, tol = tolPwrss, GQmat = GHrule(0L), compDev = compDev,  : 
#pwrssUpdate did not converge in (maxit) iterations
#Breaks glmm so maybe cant converge that way
#Changed control = glmerControl(tolPwrss=1e-5) and works but feels like cheating

#Tried to change starting values (not by too much) and it would end in a different
#valley with the same error.
#Unsure what happens

#Ran a sharedusers only matrix. When optimizing for the glmer constant (for log), the constant 
#would end up being the upper bound always. I had it set to 1 to be reasonable but played with
#larger limits and it would only want the upper bound. I think this is bc shared users mat is 
#extremely sparse and the addition of a constant is messing it up.
#Loglik also was about 100 less than what it is with both dist and shared users

#Log lik for that model was -1329 when looking for a between 0 and 1 (it chose 1) and -1332 for
#an a value between 0 and 10 (it chose 10)

#DEoptim never ends up finishing running. Always freezes at a certain random value. I checked the
#values it stopped at and they don't seem crazy which leads to believe something I am doing in the 
#control section is incorrect.

#Did the for loop of a few points to create loglik surfaces 
forloopsummary <- read.csv("data/forloophyperparams.csv")

library(plotly)
#Probably more important one
fig5 <- plot_ly(
  type = 'mesh3d', 
  x = ~as.numeric(forloopsummary$rho),
  y = ~as.numeric(forloopsummary$theta),
  z = ~(-1*forloopsummary$loglikelhood),
) %>% layout(
  title = "Layout options in a 3d scatter plot",
  scene = list(
    xaxis = list(title = "rho"),
    yaxis = list(title = "theta"),
    zaxis = list(title = "loglik")
  ))
fig5

#I think I just need to find this out manually and do another more precise for loop
#Is changing glmerControl(tolPwrss=1e-5)) a viable solution worth going into

source("scripts/shorterbuildmodelandoptim.R")

#What my DEoptim would be
controlDE <- list(reltol=.000001,steptol=150, itermax = 500,trace = 250,
                   strategy=1, c=0,
                   NP=3600)
#DEoptim version
attempt1 <- DEoptim(fn = buildthemodel, lower = (c(3,0.5,0.0001,0.01)), upper = (c(150, 1.99, 2, 0.999)),
                    control = controlDE)

#This might not be the direction to go in. It would be nice to just get an answer for hyper params and move to the next
#step or redo parts of the model that include shared users to make something that makes sense




library(TMB)
source("scripts/creatingfunctiondf.R")
compile("scripts/TMBmodelyearfixedeffect.cpp")
dyn.load(dynlib("scripts/TMBmodelyearfixedeffect"))

countyeffect = rep(0,548)
yeareffect = rep(0,13)
dparam = 50
thetaparam = 1.4
rhoparam = 0.6  

a <- 0.001
dd <- list(dist = orderedmat, dim = 548, SM = bigsharedusers, numberofyears = 13,
           fullcountyincidence = countylist)
pp <- list(log_d = log(dparam), theta = thetaparam, logit_rho = qlogis(rhoparam),  log_offsetparam = log(a),
           logsd_County = 0, logsd_Year = 0,
           YearEffect = yeareffect, CountyRandomEffect = countyeffect)

obj <- MakeADFun(data=dd, parameters=pp, DLL="TMBmodelyearfixedeffect",
                 silent=TRUE,
                 random=c("CountyRandomEffect"))
opt2 <- with(obj, nlminb(start = par, obj = fn, gr=gr,
                         control=list(trace=10)))
#So in summary, basically the same garbage as before

#log_d              theta          logit_rho            log_offsetparam       logsd_County 
#-10.106597331        0.678088300       -1.447667092       -6.770819633       -0.206067724 
#YearEffect         YearEffect         YearEffect         YearEffect         YearEffect 
#1.735159104        1.414794850        1.126131117        1.040656073        0.803688858 
#YearEffect         YearEffect         YearEffect         YearEffect         YearEffect 
#0.998166909        1.020842341        1.042321866        0.803163892        1.792329040 
#YearEffect         YearEffect         YearEffect 
#1.984401158        5.779988107        0.000000000

library(tmbstan)
st1 <- tmbstan(obj, init="last.par.best")


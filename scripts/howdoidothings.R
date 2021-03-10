#Script fot Bolker to run
source("scripts/packages.R")
source("scripts/sharedusersmatrix.R")
source("scripts/weightedshareduserdf.R")

#The data frame with the best parameters
hessian <- read.csv("data/paramshessian.csv")
hessian <- hessian[,-1]
hessian.inv <- solve(hessian)
param.se <- sqrt(diag(hessian.inv))
param.se

#Create Confidence interval matrix 
CI.matrix <- as.data.frame(matrix(NA, nrow = 3, ncol = 4))
#Parameters from optim function 
optimparameters <- c(9.4570985, -0.3302666, -0.8784447,  0.3435393)

CI.matrix[1,] <- optimparameters
CI.matrix[2,] <- optimparameters - 1.96 * param.se
CI.matrix[3,] <- optimparameters + 1.96 * param.se
colnames(CI.matrix) <- c("scaling parameter", "theta", "a", "rho")
rownames(CI.matrix) <- c("ML", "95% Lower bound", "95% Upper bound")

CI.matrix$theta <- plogis(CI.matrix$theta)*2
CI.matrix$a <- exp(CI.matrix$a)
CI.matrix$rho <- exp(CI.matrix$rho)

CI.matrix #Not a good looking matrix....

#In wsuhypparamoptim.R is the optim function and I think the problem comes
#from using log exp for rho variable


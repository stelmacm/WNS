#Stan model looks nice
#If I run 10 chains and I thin by 10 I get warning that there 
#is essentially too thin
#So I have thin at 5 and I get very nice results but is that cheating?

#https://bragqut.files.wordpress.com/2015/05/on-thinning-of-chains-in-mcmc.pdf
#^article kinda going into thinning and why I might be cheating

#How do I test the "enforcement of priors", I made up random data and just tested it
#and that seemed to work fine but I'm not sure if that is sufficient

#I am happy with priors = normal(... autoscale = TRUE) bc it makes things look nice
#If TRUE then the scales of the priors on the intercept and regression coefficients may 
#be additionally modified internally by rstanarm in the following cases. 
#First, for Gaussian models only, the prior scales for the intercept, coefficients, 
#and the auxiliary parameter sigma (error standard deviation) are multiplied by sd(y). 

#I put year as a fixed variable and played around with that but the beta values of each year 
#increase as the year progresses ie. 2007 has a value of 3 but 2011 is 9 and 2017 
#has a value of 18
#Not a problem but having year as a random effect isnt the prettiest but I playing around with 
#priors doesnt change much. 

#prior auxiliary. I feel like it's cheating changing my dispersion. Wouldn't you always want
#to decrease you dispersion? I played with both and creating lower reciprocal dispersion value
#and greater dispersion happened (as anticipated), but there was not a big difference in the
#errors for each significant change
#I kept prior aux as exponential but dont know if I should change that?

#What are implications of monte carlo uncertainty? Should I not be worried and it will work itself out 
#with my priors? 

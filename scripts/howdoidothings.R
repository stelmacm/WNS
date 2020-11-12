#Summary/Questions:

#Kriging:
#Tried to do things with ggmap and ggplot but 
#too much distance between points so tiling becomes funky 
#But it is not sufficient for krigging
#Option 1: Create empty grid in the background 
#Option 2: Create spatial domain using gstat (same thing basically)
#Unsure fully how this works but seems more legit than fake background 
#Is variogram model the same as a normal model? from my understanding it is
#spatial layout of how my variables interact
#Unsure how to really make that equation

#Residuals:
#Not understanding residuals
#This is of glm(incidence ~ offset(log(previousyear + 1))
#Should I not worry and just plot mixed model df
#Should I just do residuals per variable rather than one the model
#Why are my residuals the way they are :(
#plot is in  stelmacm/wns "Other/glmresid.png" 


## SIMULATION protocol

## SI model with spatially weighted contacts; annual time steps

## 1. Get  a list of contact matrices, one for each year,
##       from the data (caves or counties, your choice)
##   Could be sparse, but probably doesn't matter
##   choices:
##    1. nearest-neighbour (null hypothesis)
##    2. global (null hypothesis)
##    3. based on number of shared geocache visits
##
## Q: how should these be normalized?  Probably want row sums equal to 1?
## W[[year]] is the contact matrix for a given year

## pick a value for beta
## make up some starting values (i.e. pick some infected caves in a sensible way): ivec is the state vector (=0 if uninfected, 1 if infected)

## For each year

uninf <- ivec==0
foi <- beta* (W[[year]] %*% ivec)
hazard <- 1-exp(-foi)
ivec[uninf] <- rbinom(sum(uninf), prob=hazard)



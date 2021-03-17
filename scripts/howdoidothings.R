source("scripts/packages.R")
#Another model builder but with weighted shared users already created
#Take known data
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))

distmat <- read.csv("data/distancematrix.csv")
distmat2 <- distmat[,-1]
rownames(distmat2) <- colnames(distmat2)
d1 <- as.matrix(distmat2)
diag(d1) <- 0 
#convert from m to km
d1 <- d1/1000

#import already weighted matrix

numbersharedcountiesusers <- read_csv("data/weightedshareduserdf.csv") %>%
  mutate(year = factor(year)) %>% dplyr::select(-X1)

#test <- vroom("data/weightedsharedusers.csv") %>% dplyr:: select(-1) %>% mutate(year = factor(year))
#Pulling this out of for loop
incidencedata <- (read.csv("data/incidencepercounty.csv"))%>%
  dplyr::select(county, year, incidence) %>%
  mutate(year = factor(year))

county.incidence.by.year <- mixedmodeldf %>% filter(county != "Lewis-Washington" &
                                                      county != "King-Washington" &
                                                      county != "Plumas-California") %>%
  dplyr::select(-c(yc,id)) %>% arrange(county)

countylist <- list()
#Can't have county incidence by year bc that makes no sense
for(i in levels(mixedmodeldf$year)){
  countylist[[i]] <- county.incidence.by.year %>%
    filter(year == i) %>% dplyr::select(-c(county,year))
}

sharedusersperyear <- list()
for (i in levels(mixedmodeldf$year)) {
  #Could ultimately do this outside of for loop since its always constant and have a list of vectors inside
  sharedusersperyear[[i]] <- (numbersharedcountiesusers %>% filter(year == i) %>%
    dplyr::select(-year))
}

sharedmatrix <- list()
sharedusers <- list()
for (i in levels(mixedmodeldf$year)){
  #perfect
  sharedmatrix[[i]] <- dcast(sharedusersperyear[[i]], county ~ county2, value.var = "num.shared")
  #Works because 1st column is names is row 1
  removingcounty <- sharedmatrix[[i]]
  sharedusers[[i]] <- removingcounty[,-1]
}

foidf <- county.incidence.by.year %>% filter(year == 2010) %>%
  dplyr::select(county)

#Thoughts:
#The only thing that really has to be built in the optim model is the weight matrix and the glmm. Everything else cant be built outside
#Should build so that all we need to do is create local.county.m and multiply it by shared useds and incidence
#to create foi df to use in glmm


buildthemodel <- function(p) {
  ## unpack parameter vector, via link functions
  cat(p,"\n")
  d <- p[1]   ## identity (no constraint, we'll hope we don't hit 1.0 exactly)
  theta <- plogis(p[2])*2  ## plogis -> [0,1], then double it for [0,2]
  a <- exp(p[3])           ## must be positive
  rho <- plogis(p[4])
  #cutoffpoint <- (1/exp(p[4]))
  #cutoffpoint <- (1e-300)
  #Maybe just have to do it by hand
  azzelinifun <- exp(-((d1)/d)^theta)
  diag(azzelinifun) <- 0
  #azzelinifun[(azzelinifun < cutoffpoint)] <- 0
  
  #Now to weight matrix
  localcountymat.w <- mat2listw(azzelinifun, style = "W")
  localcountymat.m <- as(localcountymat.w, "CsparseMatrix") 
  localcountymat <- as.matrix(localcountymat.m)
  #Order the matrix so multiplication makes sense
  orderedmat <- localcountymat[sort(rownames(localcountymat)),sort(colnames(localcountymat))]
  
  #Multiply the matrices
  for (i in levels(mixedmodeldf$year)){
    #Maths
    userstimeslocation <- rho*as.matrix(sharedusers[[i]]) + (1-rho)*orderedmat
    #multiply W_ij %*% I_t
    foivector <- userstimeslocation %*% as.matrix(countylist[[i]])
    #reattach
    foidf[,i] <- foivector
  }
  
  #The melt looks something like this
  forceofinfectiondf <- reshape2::melt(foidf, id = "county") %>%
    dplyr::rename(year = 'variable', foi = 'value') %>%
    arrange(county) %>%
    mutate(year = factor(year))
  #Never arranged alphabetically before. odd 

  modeldataframe <- left_join(forceofinfectiondf, incidencedata, by = c("county","year")) %>%
    mutate(previousyear=lag(incidence)) %>%
    filter(year != "2006") #Maybe there are nicer ways to do this. oh well

  modeldataframe$year <- factor(modeldataframe$year)
  modeldataframe$previnf <- (lag(modeldataframe$foi))

  #So problem with mixed modeldf is that the years continue once incidence has 
  #occered. So it is redundant.

  #Changing to disappear after incidence occures
  newdf <- modeldataframe %>% subset((incidence == 0) | (incidence == 1 & previousyear == 0))

  #Mixed model formula
  formula <- incidence ~ (1|year) + (1|county) + offset(log(previnf + a))

  foimm <- try(glmer(formula, data = newdf, family = binomial(link = "cloglog")),
               silent=TRUE)
  
  if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  return(optimalloglik)
}

linkfun <- function(p) c(p[1], qlogis(p[2]/2), log(p[3]), qlogis(p[4]))

test1 <- buildthemodel(linkfun(c(10,.5,.1,.3)))
test2 <- buildthemodel(linkfun(c(60,1.3,.01,.5)))

#Don't run
#bestparams <- optim(par = linkfun(c(10,.5,.01,0.5)), fn = buildthemodel, control=list(trace=1000),hessian = TRUE)

listem <- function(q){
  scalingparam <- q[1]   ## identity (no constraint, we'll hope we don't hit 1.0 exactly)
  tita <- plogis(q[2])*2  ## plogis -> [0,1], then double it for [0,2]
  aa <- exp(q[3])
  row <- plogis(q[4])
  return(c(scalingparam, tita, aa, row))
}

#On attempt one with LL of -1452.86 and param 10, .5, .01 , .5
bp <- listem(c(5.624565, 3.228535, -6.105417, 7.515743))
aa <- buildthemodel(linkfun(bp))

#In the extended version
bp2 <- listem(c(9.422264, -4.170525, -6.069059, 7.979524))
buildthemodel(linkfun(bp2))
#Returns NA????
bpbuild <- buildthemodel(linkfun(c(9.422264000, 0.030418512, 0.002313349, 0.999657715)))
#so I will do it manually
#yikes
source("scripts/errorwithlowestloglik.R") #foimm2
#Basically the MLE happens when all the FOI is 0 or close to 0 which is not good because we need it to grow

#Model that is just shared users optim
source("scripts/packages.R")
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

buildsharedusermodel <- function(p) {
  ## unpack parameter vector, via link functions
  a <- p[1]
  #cutoffpoint <- (1/exp(p[4]))
  #cutoffpoint <- (1e-300)
  #Maybe just have to do it by hand

  #Multiply the matrices
  for (i in levels(mixedmodeldf$year)){
    #Maths
    userstimeslocation <- as.matrix(sharedusers[[i]])
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
  
  #foimm <- try(glmer(formula, data = newdf, family = binomial(link = "cloglog")),silent=TRUE)
  foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))
  
  
  #if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  return(optimalloglik)
}


logfunc <- function(p)c(log(p))

test1 <- buildsharedusermodel((0.8))

#oof
besta <- optim(par = c(.3), fn = buildsharedusermodel, control = list(trace = 1000),
               hessian = TRUE, method = "Brent", lower = 0.0001, upper = 1)


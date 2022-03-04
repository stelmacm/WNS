#modelwithwinterdays.R but as function
#So it becomes modelasfunctionwithwinterdays.R

source("scripts/packages.R")

mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))

numbersharedcountiesusers <- read_csv("data/numbersharedcountiesusers.csv") %>%
  dplyr::select(-X1) %>% mutate(year = factor(year))

#Taking this from creatingfunctiondf.R
sharedusersperyear <- list()
for (i in levels(mixedmodeldf$year)) {
  sharedusersperyear[[i]] <- (numbersharedcountiesusers %>% filter(year == i) %>%
                                dplyr::select(-year))
}

sharedmatrix <- list()
sharedusers <- list()
for (i in levels(mixedmodeldf$year)){
  sharedmatrix[[i]] <- dcast(sharedusersperyear[[i]], county ~ county2, value.var = "num.shared")
  #Works because 1st column is names is row 1
  removingcounty <- sharedmatrix[[i]]
  sharedusers[[i]] <- removingcounty[,-1]
}

#Doing this to find the mean for row normalization purposes
bigsharedusers <- cbind(sharedusers[[1]], sharedusers[[2]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[3]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[4]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[5]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[6]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[7]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[8]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[9]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[10]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[11]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[12]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[13]])
bigsharedusers <- as.matrix(bigsharedusers)
#Extracting the row means that we want in question
SUmean <- mean(rowMeans(bigsharedusers)) #0.0009559544
#Dividing the shared users matrices by the mean
SUmatrixmeaned <- list()
for (i in levels(mixedmodeldf$year)) {
  SUmatrixmeaned[[i]] <- sharedusers[[i]]/SUmean
}

#Now creating the distance decay matrix
distmat <- read.csv("data/distancematrix.csv")
distmat2 <- distmat[,-1]
rownames(distmat2) <- colnames(distmat2)
d1 <- as.matrix(distmat2)
diag(d1) <- 0 

incidencedata <- (read.csv("data/incidencepercounty.csv"))%>%
  dplyr::select(county, year, incidence) %>%
  mutate(year = factor(year))

county.incidence.by.year <- mixedmodeldf %>% filter(county != "Lewis-Washington" &
                                                      county != "King-Washington" &
                                                      county != "Plumas-California") %>%
  dplyr::select(-c(yc,id)) %>% arrange(county)

countylist <- list()
for(i in levels(mixedmodeldf$year)){
  countylist[[i]] <- county.incidence.by.year %>%
    filter(year == i) %>% dplyr::select(-c(county,year))
}

foidf <- county.incidence.by.year %>% filter(year == 2010) %>%
  dplyr::select(county)

frenchify <- function(countydf){
  countydf[countydf == "La C\x99te-De-Gasp\x8e-Quebec"] <- "La Côte-De-Gaspé-Quebec"
  countydf[countydf == "L'\x83rable-Quebec"] <- "L'Érable-Quebec"
  countydf[countydf == "Lotbini̬re-Quebec"] <- 'Lotbinière-Quebec'
  countydf[countydf == "Le Haut-Saint-Laurent-Qu̩bec"] <- 'Le Haut-Saint-Laurent-Québec'
  countydf[countydf == "Memphr̩magog-Quebec"] <- 'Memphrémagog-Quebec'
  countydf[countydf == "Le Haut-Saint-Fran\x8dois-Quebec"] <- 'Le Haut-Saint-François-Quebec'
  countydf[countydf == "Antoine-Labelle-Qu̩bec"] <- 'Antoine-Labelle-Québec'
  countydf[countydf == "La C̫te-de-Beaupr̩-Qu̩bec"] <- 'La Côte-de-Beaupré-Québec'
  return(countydf)
}#OK

foidf <- frenchify(foidf)

wintercountyperyear <- read.csv("data/winterdayspercountyperyear.csv") %>% 
  rename("winterdays" = "listofwinterdays2007") %>% mutate(year = factor(year))

#Mixed model formula


winterizedmodelbuild <- function(p) {
  ## unpack parameter vector, via link functions
  cat(p,"\n")
  d <- exp(p[1])   ## identity (no constraint, we'll hope we don't hit 1.0 exactly)
  theta <- (p[2])  ## plogis -> [0,1], then double it for [0,2]
  a <- exp(p[3])           ## must be positive
  rho <- plogis(p[4])     ##between 0 and 1
  #cutoffpoint <- (1/exp(p[4]))
  #cutoffpoint <- (1e-300)
  cat(d,theta,a,rho,"\n")
  azzelinifun <- exp(-((d1)/d)^theta)
  diag(azzelinifun) <- 0
  #azzelinifun[(azzelinifun < cutoffpoint)] <- 0
  azzelinimean <- mean(rowMeans(azzelinifun))
  azzeliniprime <- azzelinifun / azzelinimean
  
  #Order the matrix so multiplication makes sense
  orderedmat <- azzeliniprime[sort(rownames(azzeliniprime)),sort(colnames(azzeliniprime))]
  
  #Multiply the matrices
  for (i in levels(mixedmodeldf$year)){
    #Maths
    userstimeslocation <- rho*as.matrix(SUmatrixmeaned[[i]]) + (1-rho)*orderedmat
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
  
  modeldataframe <- left_join(forceofinfectiondf, incidencedata, by = c("county","year")) %>%
    mutate(previousyear=lag(incidence)) 
  
  modeldataframe$year <- factor(modeldataframe$year)
  modeldataframe$previnf <- (lag(modeldataframe$foi))
  
  #Add winter to the model
  newmodeldf <- left_join(modeldataframe, wintercountyperyear, by = c("county", "year"))
  
  #Changing to disappear after incidence occures
  newdf <- newmodeldf %>%
    filter(year != "2006") %>% subset((incidence == 0) | (incidence == 1 & previousyear == 0))
  
  formula <- incidence ~ (1|year) + (1|county) + winterdays + offset(log(previnf + a))
  
  foimm <- try(glmer(formula, data = newdf, family = binomial(link = "cloglog")),
               silent=TRUE)
  
  if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  return(optimalloglik)
}
pp <- winterizedmodelbuild(c(3.94594302, 1.80933352, -4.81928386,0.02135985))
pp <- winterizedmodelbuild(c(3.94594302, 1.80933352, -1.81928386,0.02135985))
summary(pp)

modelobjoptima <-optim(par = c(log(50), 1.2, log(0.01), qlogis(0.5)), fn = winterizedmodelbuild, 
                        control=list(trace=10, maxit = 1000))

#Print/save ridic values parameters in a list 

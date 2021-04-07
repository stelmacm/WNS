#Creating one of each function to simulate and optimize.
#Startng with a shared users only model

#I realize I still want distance as part of this just not azzalini bubble
#Now to weight matrix
countymat.w <- mat2listw(distancemat, style = "W")
countymat.m <- as(countymat.w, "CsparseMatrix") 
countymat <- as.matrix(countymat.m)
#Order the matrix so multiplication makes sense
ordereddistmat <- countymat[sort(rownames(countymat)),sort(colnames(countymat))]

#OR maybe I don't.... TBD

#Coming back and redoing with proper IDW, I kinda know exactly what I wanted to know
#Nothing really changed. These are bad models and they are too sparse and just dont make sense.


fullsharedusermodel <- function(p) {
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
  formula <- incidence ~ (1|county) + (1|year) + offset(log(previnf + a))
  
  #foimm <- try(glmer(formula, data = newdf, family = binomial(link = "cloglog")),silent=TRUE)
  foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))
  
  
  #if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  return(optimalloglik)
}

#Now we are doing the shared model but year is linear
linearyearsharedusermodel <- function(p) {
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
  formula <- incidence ~ (1|county) + year + offset(log(previnf + a))
  
  #foimm <- try(glmer(formula, data = newdf, family = binomial(link = "cloglog")),silent=TRUE)
  foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))
  
  
  #if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  return(optimalloglik)
}

#Now we are doing the shared model but county is linear
linearcountysharedusermodel <- function(p) {
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
  formula <- incidence ~ county + (1|year) + offset(log(previnf + a))
  
  #foimm <- try(glmer(formula, data = newdf, family = binomial(link = "cloglog")),silent=TRUE)
  foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))
  
  
  #if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  return(optimalloglik)
}

#Get rid of a value and run a full model
fullsharedusermodelbutnoavalue <- function(p) {
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
  formula <- incidence ~ (1|county) + (1|year) + offset(log(previnf))
  
  #foimm <- try(glmer(formula, data = newdf, family = binomial(link = "cloglog")),silent=TRUE)
  foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))
  
  
  #if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  return(optimalloglik)
}

#Get rid of a and make it year linear
linearyearsharedusermodelnoaval <- function(p) {
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
  formula <- incidence ~ (1|county) + year + offset(log(previnf))
  
  #foimm <- try(glmer(formula, data = newdf, family = binomial(link = "cloglog")),silent=TRUE)
  foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))
  
  
  #if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  return(optimalloglik)
}

#Get rid of a and make county linear
linearcountysharedusermodelnoaval <- function(p) {
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
  formula <- incidence ~ county + (1|year) + offset(log(previnf))
  
  #foimm <- try(glmer(formula, data = newdf, family = binomial(link = "cloglog")),silent=TRUE)
  foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))
  
  
  #if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  return(optimalloglik)
}

#Now I want to do optim for all of these
val1 <- optim(par = c(.3), fn = fullsharedusermodel,
                hessian = TRUE, method = "Brent", lower = 0.0001, upper = 5)
#Goes to upper limit

val2 <- optim(par = c(.3), fn = linearyearsharedusermodel,
                hessian = TRUE, method = "Brent", lower = 0.0001, upper = 5)
#Goes to upper limit

val3 <- optim(par = c(.3), fn = linearcountysharedusermodel,
              hessian = TRUE, method = "Brent", lower = 0.0001, upper = 5)
#Doesnt like alot of values and then goes to upper limit

val4 <- optim(par = c(.3), fn = fullsharedusermodelbutnoavalue,
              hessian = TRUE, method = "Brent", lower = 0.0001, upper = 5)
#Doesnt work

val5 <- optim(par = c(.3), fn = linearyearsharedusermodelnoaval,
              hessian = TRUE, method = "Brent", lower = 0.0001, upper = 5)
#Doesnt work
val6 <- optim(par = c(.3), fn = linearcountysharedusermodelnoaval,
              hessian = TRUE, method = "Brent", lower = 0.0001, upper = 5)
#Doesnt work
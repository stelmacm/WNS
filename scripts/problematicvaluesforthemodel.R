#Problematic builds

problematicbuilds <- function(p) {
  ## unpack parameter vector, via link functions
  cat(p,"\n")
  d <- p[1]   ## identity (no constraint, we'll hope we don't hit 1.0 exactly)
  theta <- (p[2])  ## plogis -> [0,1], then double it for [0,2]
  a <- (p[3])           ## must be positive
  rho <- (p[4])
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
  
  foimm <- try(glmer(formula, data = newdf, family = binomial(link = "cloglog"), 
                     control = glmerControl(tolPwrss=1e-5)),
               silent=TRUE)
  
  if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  return(foimm)
}

whereoptimstops <- problematicbuilds(c(5.624565, 1.92378, 0.00223 ,0.99945))
whereoptimstops
whereoptimgetscrazyvalues <- problematicbuilds(c(6.309984, 3.263814, -6.109932, 7.503976))
whereoptimgetscrazyvalues

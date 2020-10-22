#Now we are creating the script that has all the glm as a function
#The will run that function


#First we start with everything we need outside the function
source("scripts/packages.R") 
source("scripts/wns-presence.R")
library(conflicted)
library(glmmTMB)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("lag", "dplyr")

#Import dataset to create county incidence matrix
mixedmodeldf <- read.csv("data/mixedmodeldf.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))

#Remove things that don't work for me later on (this is ugly but
#other things weren't working)
county.incidencee.by.year <- mixedmodeldf[-c(27,28,29,30,31,32,
                                             33,34,35,36,37,38,39,
                                             5201, 5202,5203,5204,
                                             5205, 5206,5207,5208,
                                             5209, 5210,5211,5212,
                                             5213,7151,7152,7153,7154,
                                             7155,7156,7157,7158,7159,
                                             7160,7161,7162,7163),-4]

#Define the first set of counties
countylist <- county.incidencee.by.year %>%
  arrange(year) %>%
  filter(year == 2006) %>%
  dplyr::select(county)

#another incidence matrix but I swear its not that bad 
#importing 2 of the same dataset
incidencedata <- (read.csv("data/mixedmodeldf.csv"))%>%
  dplyr::select(county, year, incidence) %>%
  mutate(year = factor(year))

#Create a base layer to begin with
foidf <- county.incidencee.by.year %>%
  arrange(year) %>%
  dplyr::filter(year == 2006) %>%
  dplyr::select(county)

#Create distance matrix
uniq.df <- (presence.df
            %>% dplyr::filter(!STATEPROV %in% c("California","Washington"),
                              !duplicated(county)) #so we only have unique counties
)

wnslat <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[1]])
wnslon <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[2]])

wns.center.coords <- cbind(wnslat, wnslon)

d1<- distm(wns.center.coords, fun = distGeo)
diag(d1) <- 0
d1 <- d1/1000

formula <- incidence ~ (1|year) + (1|county) +
  offset(log(previousyear + 1))

#Now here we will make it so the user input the distance and cut off of their choice.

runthemodel <- function(distancecutoff, expcutoff){
  
  xfun2 <- function(x) {
    exp(-pmax(x,distancecutoff))
  }
  #Apply function
  decay.mat <- xfun2(d1)
  #Now set cut off
  decay.mat[(decay.mat < expcutoff)] <- 0 
  #SEt diag to 0
  diag(decay.mat) <- 0
  
  #Now to weight matrix
  localcountymat.w <- mat2listw(decay.mat, style = "W") #Rows sum to 1 (W is row standardized)
  localcountymat.m <- as(localcountymat.w, "CsparseMatrix") 
  dimnames(localcountymat.m) <- list(uniq.df$county,uniq.df$county)
  localcountymat <- as.matrix(localcountymat.m) 

  #Create forloop to create datafram
  for(i in levels(mixedmodeldf$year)){
    countylist[,i] <- county.incidencee.by.year %>%
      arrange(year) %>%
      filter(year == i) %>%
      select(incidence)
  }
  #Get rid of names because I am a child who doesn't know how to multiply things normally
  #And has to remove the names of counties
  countylist <- as.data.frame(countylist[,-1])
  
  #For loop that creates force of infection datafrome
  for (i in levels(mixedmodeldf$year)) {
    #need to detach
    infectionvector <- countylist[,i]
    infectionvector <- as.matrix(infectionvector)
    #multiply W_ij %*% I_t
    foivector <- localcountymat %*% infectionvector
    #reattach
    foidf[,i] <- foivector
  }
  
  
  #The melt looks something like this
  forceofinfectiondf <- reshape2::melt(foidf, id = "county") %>%
    dplyr::rename(year = 'variable', foi = 'value') %>%
    arrange(county) %>%
    mutate(year = factor(year))
  #Never arranged alphabetically before. odd 
  
  modeldataframe <- left_join(forceofinfectiondf, incidencedata, 
                              by = c("county","year")) %>%
    mutate(previousyear=lag(foi))%>%
    filter(year != "2006") #Was having trouble filtering factor 
    #(converting to numeric would make df disapear)
  
  #foimodel <- glm(incidence ~ offset(log(previousyear + 1)),
  #                data = modeldataframe, family = binomial(link = "cloglog"))
  
  foimixedmodel <- glmmTMB(formula, data = modeldataframe,
                           family = binomial(link = "cloglog"))
  
  return(foimixedmodel)
  
}

#test<- runthemodel(100, 1e-150)

cutoffvalues <- 10^(seq(-300, -100, by = 25))
distcoefvec <- exp(seq(log(2), log(200), length.out=30)) #Actually really like the values
llikvec <- rep(NA,length(distcoefvec))

#I guess my brain kinda broke on what else to do with this for loop
for(i in seq_along(distcoefvec)){
  result <- runthemodel(i, 1e-150)
  llikvec[i] <- logLik(result)
  #I have no clue how to get coeffs
}

llikvec #I can't think of a nice plot for this
#So mixedmodel loglikelihood show -1511.619 for anything under
#Next step is optimizing? Not sure what the really means...

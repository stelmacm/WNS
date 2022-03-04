#The original version of this will be only American counties because Canada is whack
#I am mildly unconvinced of the number of days below 10 degrees ex. Quebec 334
#But thats the minimum temperature so I am not going to say anything. 
source("scripts/packages.R")
wintercountyperyear <- read.csv("data/winterdayspercountyperyear.csv") %>% 
  rename("winterdays" = "listofwinterdays2007") %>% mutate(year = factor(year))

#Now creating the distance decay matrix
distmat <- read.csv("data/distancematrix.csv")
distmat2 <- distmat[,-1]
rownames(distmat2) <- colnames(distmat2)
d1 <- as.matrix(distmat2)
diag(d1) <- 0 

azzelinifun <- exp(-((d1)/500)^1.25)
diag(azzelinifun) <- 0
#azzelinifun[(azzelinifun < cutoffpoint)] <- 0
azzelinimean <- mean(rowMeans(azzelinifun))
azzeliniprime <- azzelinifun / azzelinimean

#Order the matrix so multiplication makes sense
orderedmat <- azzeliniprime[sort(rownames(azzeliniprime)),sort(colnames(azzeliniprime))]

#Multiply the matrices
for (i in levels(mixedmodeldf$year)){
  #Maths
  userstimeslocation <- 0.5*as.matrix(SUmatrixmeaned[[i]]) + (1-0.5)*orderedmat
  #multiply W_ij %*% I_t
  foivector <- userstimeslocation %*% as.matrix(countylist[[i]])
  #reattach
  foidf[,i] <- foivector
}

forceofinfectiondf <- reshape2::melt(foidf, id = "county") %>%
  dplyr::rename(year = 'variable', foi = 'value') %>%
  arrange(county) %>%
  mutate(year = factor(year))

modeldataframe <- left_join(forceofinfectiondf, incidencedata, by = c("county","year")) %>%
  mutate(previousyear=lag(incidence)) 

modeldataframe$year <- factor(modeldataframe$year)
modeldataframe$previnf <- (lag(modeldataframe$foi))

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

modeldataframe <- frenchify(modeldataframe)
#Now I want to join the winterdays dataframe to this modeldataframe

newmodeldf <- left_join(modeldataframe, wintercountyperyear, by = c("county", "year"))

#Changing to disappear after incidence occures
newdf <- newmodeldf %>%
  filter(year != "2006") %>% subset((incidence == 0) | (incidence == 1 & previousyear == 0))

#Mixed model formula
formula <- incidence ~ (1|year) + (1|county) + winterdays + offset(log(previnf + 0.1))

foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))
summary(foimm)

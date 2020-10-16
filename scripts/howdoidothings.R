#Hey look at me clogging up the global enviroment with useless packages
source("scripts/packages.R")
library(conflicted)
conflict_prefer("flatten", "purrr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")

source("scripts/wns-presence.R") 

foidataframe <- read.csv("data/foidataframe.csv")

#This is me trying to get better at pipping 
meltedfoi <- melt(foidataframe, id = "county") %>%
  rename(county = 'Var1', year = 'Var2', foi = 'value') %>%
  arrange(county) # %>%
  #mutate_at(factor(year))  #crying 

#can't factor in the pipe for some reason
meltedfoi$year <- factor(meltedfoi$year) 
#So this is the new data frame
#I need it in the form to have a lag 

lagfoidataframe <- meltedfoi %>%
  as_tibble() %>%
  mutate(previousyear=lag(foi))

#I need to set to 0 right? or is this unnecessary 
lagfoidataframe[1,4] <- 0 #I forget it needed since +1 in formula
#Amazing. Now need to left join incidence data

incidencedata <- (read.csv("data/mixedmodeldf.csv"))%>%
  select(county, year, incidence) 

incidencedata$year <- factor(incidencedata$year)

combined.data <- left_join(lagfoidataframe, incidencedata, by = c("county", "year"))

#This is where I don't know what im doing anymore. Did I include FOI correctly?
#I am unsure how to use it for the formula. I have made previousyear to be previousFOI instead
#of previous incidence. Am I losing my mind?
foimodel <- glm(incidence ~ offset(log(previousyear + 1)),
                data = lagfoidataframe, family = binomial(link = "cloglog"))

formula <- incidence ~ (1|year) + (1|county) +
  offset(log(previousyear + 1))

foimixedmodel <- glmm(formula, data = lagfoidataframe, family = binomial(link = "cloglog"))

#So once I figure out what is actually what with the equations and formula's I am to look at loglikelihood
#of different weight matricies? (I am hiding really really ugly FOI matrix code) But that would go into conideration
#of the different weight matricies? I guess it would if the final formula calls for it.

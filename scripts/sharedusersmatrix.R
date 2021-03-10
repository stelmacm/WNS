library(reshape2)
shareuserlists <- read.csv("data/fullshareduserslist.csv") %>%
  rename(county = county1) %>%
  rename(year = i.year) %>%
  mutate(year = factor(year)) %>%
  filter(year != 2001) %>%
  filter(year != 2002) %>%
  filter(year != 2003) %>%
  filter(year != 2004) %>%
  filter(year != 2005)

#Take known data
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year)) %>%
  dplyr::select(-yc)

county.incidence.by.year <- mixedmodeldf %>% filter(county != "Lewis-Washington" &
                                                      county != "King-Washington" &
                                                      county != "Plumas-California") 

source("scripts/wns-presence.R")
#Remove Cali and Wash for now
uniq.df <- (presence.df
            %>% dplyr::filter(!STATEPROV %in% c("California","Washington"),
                              !duplicated(county)) #so we only have unique counties
)

wnslat <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[1]])
wnslon <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[2]])

wns.center.coords <- cbind(wnslat, wnslon)

d1 <- distm(wns.center.coords, fun = distGeo)
dimnames(d1) <- list(uniq.df$county,uniq.df$county)

diag(d1) <- 0 
#convert from m to km
d1 <- d1/1000
bothcouties <- melt(d1)
bothcounties <- bothcouties %>% dplyr::rename(county = Var1) %>% dplyr::rename(county2 = Var2) 
head(bothcounties)

county.incidence.by.year[county.incidence.by.year == "La C\x99te-De-Gasp\x8e-Quebec"] <- 'La Côte-De-Gaspé-Quebec'
county.incidence.by.year[county.incidence.by.year == "L'\x83rable-Quebec"] <- "L'Érable-Quebec"
county.incidence.by.year[county.incidence.by.year == "Lotbini̬re-Quebec"] <- 'Lotbinière-Quebec'
county.incidence.by.year[county.incidence.by.year == "Le Haut-Saint-Laurent-Qu̩bec"] <- 'Le Haut-Saint-Laurent-Québec'
county.incidence.by.year[county.incidence.by.year == "Memphr̩magog-Quebec"] <- 'Memphrémagog-Quebec'
county.incidence.by.year[county.incidence.by.year == "Le Haut-Saint-Fran\x8dois-Quebec"] <- 'Le Haut-Saint-François-Quebec'
county.incidence.by.year[county.incidence.by.year == "Antoine-Labelle-Qu̩bec"] <- 'Antoine-Labelle-Québec'
county.incidence.by.year[county.incidence.by.year == "La C̫te-de-Beaupr̩-Qu̩bec"] <- 'La Côte-de-Beaupré-Québec'


biglong <- left_join(county.incidence.by.year,bothcounties, by = "county")
biglong2 <- left_join(biglong, shareuserlists, by = c("county", "county2","year"))

#write.csv(bothcounties, "data/bothcountiespaired.csv")

#twocounties <- read.csv("data/bothcountiespaired.csv")

n_distinct(county.incidence.by.year$county)
biglong <- left_join(county.incidence.by.year,bothcounties, by = "county")
head(biglong)

biglong2 <- left_join(biglong, shareuserlists, by = c("county", "county2","year"))

biglong2$num.shared <- replace(biglong2$num.shared, is.na(biglong2$num.shared), 0)

##This is a data frame that I want to take with me
numbersharedcountiesusers <- biglong2 %>% dplyr::select(-c(X,incidence,id, value)) 

write.csv(numbersharedcountiesusers, "data/numbersharedcountiesusers.csv")
#yearone <- numbersharedcountiesusers %>% filter(year == 2008) %>%
 # dplyr::select(-year)

#perfect
#sharedmatrix <- dcast(yearone, county ~ county2)
#Works because 1st column is names is row 1
#gg <- sharedmatrix[,-1]



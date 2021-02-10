#Getting the shared users from the scrape itself 
source("scripts/packages.R")
gcscrapedresults <- read.csv("data/gc-scrape-results.csv")
relrec <- read.csv("data/relevant-records.csv")
#View(gcscrapedresults)
n_distinct(gcscrapedresults$User)
n_distinct(gcscrapedresults$GC)

#Shows me number of multi users
random <- gcscrapedresults %>% group_by(User) %>%
  dplyr::filter(n() > 2)
#Number of multiple users 
length(unique(random$User))
n_distinct(random$GC)
#So we have 1555 users who went to 202 caves
sum(is.na(random$Type)) #3 loser counties

gcusersdf <- random %>% na.omit() %>% dplyr::select(-c(Title, Log, Type)) %>%
  rename(c(long = lat, lat = lon))

testdf <- data.frame(x = gcusersdf$long, y = gcusersdf$lat)

source("scripts/lonlattocountyconversion.R")

countylist <- as.data.frame(latlong2county(testdf))
n_distinct(countylist)
nrow(countylist)
sum(is.na(countylist$`latlong2county(testdf)`)) #Problem
#Canada doesn't have counties silly

gcusersdf$countylist <- countylist$`latlong2county(testdf)`

Canadiancounties <- gcusersdf %>% filter(is.na(countylist))

canGCcounty <- relrec %>% dplyr::select(c(STATEPROV, COUNTRY, COUNTYNAME, county,
                                   year, GC, Title, lon, lat, User)) %>%
  rename(c(long = lat, lat = lon)) %>%
  filter(COUNTRY == "Canada")

#I know I could have just done this but I want both
canGCusers <- canGCcounty %>% dplyr::select(c(county, GC))

lefin <- left_join(Canadiancounties, canGCusers, by = c("GC"))
View(lefin)
n_distinct(lefin$GC)
sum(is.na(lefin$county)) #John A MacDonald rolling over in his grave

whoshere <- unique(lefin$GC)
View(whoshere)




source("scripts/packages.R")
gcscrapedresults2 <- read.csv("data/gc-scrape-results-from-AF.csv")
relrec <- read.csv("data/relevant-records-from-AF.csv")
#View(gcscrapedresults)
n_distinct(gcscrapedresults2$User)
n_distinct(gcscrapedresults2$GC)

#Shows me number of multi users
random2 <- gcscrapedresults2 %>% group_by(User) %>%
  dplyr::filter(n() > 2)
#Number of multiple users 
length(unique(random2$User))
n_distinct(random2$GC)
#So we have 1555 users who went to 202 caves
sum(is.na(random2$Type)) 

gcusersdf2 <- random2 %>% na.omit() %>% dplyr::select(-c(Title, Log, Type)) %>%
  rename(c(long = lat, lat = lon))

testdf2 <- data.frame(x = gcusersdf2$long, y = gcusersdf2$lat)

source("scripts/lonlattocountyconversion.R")

countylist2 <- as.data.frame(latlong2county(testdf2))
n_distinct(countylist2)
nrow(countylist2)
sum(is.na(countylist2$`latlong2county(testdf2)`)) #Problem
#Canada doesn't have counties silly

gcusersdf2$countylist2 <- countylist2$`latlong2county(testdf2)`

#write.csv(gcusersdf2, "data/americancountyGCusers.csv")

Canadiancounties2 <- gcusersdf2 %>% filter(is.na(countylist2))
nrow(Canadiancounties2)

canGCcounty2 <- relrec %>% dplyr::select(c(STATEPROV, COUNTRY, COUNTYNAME, county,
                                          year, GC, Title, lon, lat, User)) %>%
  rename(c(long = lat, lat = lon)) %>%
  filter(COUNTRY == "Canada")
nrow(canGCcounty2) #fine to be different
#I know I could have just done this but I want both
canGCusers2 <- canGCcounty2 %>% dplyr::select(c(county, GC))
nrow(canGCusers2)
#view(canGCusers2)

canGCusers2 <- distinct(canGCusers2) #Needed

CanGCidentified <- left_join(Canadiancounties2, canGCusers2, by = c("GC")) %>%
  dplyr::select(-countylist2) #this is the problem
#Joining the few other counties with this one creates chaos (duplicates)

#View(CanGCidentified)
n_distinct(CanGCidentified$GC)
sum(is.na(CanGCidentified$county)) #John A MacDonald rolling over in his grave

#write.csv(CanGCidentified, "data/canadiancountyGCusers.csv")


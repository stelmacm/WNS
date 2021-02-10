# continuation from huntingfordifferentsharedusers.R
americanGCcounties <- read_csv("data/americancountyGCusers.csv") %>%
  na.omit() %>% rename(county = countylist2)

americanGCcounties$county <- gsub("(\\w+),(\\w+)", "\\2,\\1", americanGCcounties$county)

simpleCap <- function(x) {
  s <- strsplit(x, ",")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse="-")
}

americanGCcounties$county <- sapply(americanGCcounties$county, simpleCap)
#View(americanGCcounties)

canadianGCcounties <- read_csv("data/canadiancountyGCusers.csv")
#view(canadianGCcounties)
head(canadianGCcounties)
head(americanGCcounties)

GCcounties <- rbind(canadianGCcounties, americanGCcounties)
GCcounties$Date <- year(GCcounties$Date)

#Doing some extra massaging
GCcounties <- GCcounties %>% rename(year = Date)
GCcounties$year <- factor(GCcounties$year)

#Issues before shared.used can be created:
#New York is coming out incorrectly
#New Jersey is coming out incorrectly
#Can I just sort counties alphabetically to ensure they multiply good and well
#Thoughts on scaling. What did Irena do? 

# get the number of shared users
shared.users<-NULL
for (i.year in unique(GCcounties$year)) {
  s <- dplyr::filter(GCcounties,year == i.year)
  for (county1 in unique(s$county)) {
    for (county2 in rev(unique(s$county))) {
      num.shared<-length(intersect(as.character(s[which(s$county == county1),]$User),
                                   as.character(s[which(s$county == county2),]$User)))
      shared.users<-as.data.frame(rbind(shared.users,cbind(i.year,county1,county2,num.shared)))
    }
  }
}

travellers <- shared.users %>% filter(num.shared > 0) 

#view(travellers)
nrow(travellers) #3950 with 04,05,06

reltravellers <- shared.users %>% filter(num.shared > 0) %>%
  filter(i.year != 2004) %>% filter(i.year != 2005) %>% filter(i.year != 2006)
nrow(reltravellers) #3822 without those years. 

#write.csv(shared.users,"data/fakeshareduserslist.csv")

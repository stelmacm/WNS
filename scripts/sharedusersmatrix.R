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
grep("Le Haut.*Fr.*Qu.*",presence.df$county,value=TRUE)

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
grep("Le Haut.*Fr.*Qu.*",rownames(d1), value=TRUE)
grep("Le Haut.*Fr.*Qu.*",colnames(d1), value=TRUE)

diag(d1) <- 0 
#convert from m to km
d1 <- d1/1000
bothcounties0 <- melt(d1)
bothcounties <- bothcounties0 %>% dplyr::rename(county = Var1) %>% dplyr::rename(county2 = Var2) 
head(bothcounties)
unique(grep("Le Haut.*Fr.*Qu.*",bothcounties$county, value=TRUE))
unique(grep("Le Haut.*Fr.*Qu.*",bothcounties$county2, value=TRUE))

unique(grep("Le Haut.*Fr.*Qu.*",county.incidence.by.year$county,value=TRUE,ignore.case=TRUE))

badstr <- "Le Haut-Saint-Fran̤ois-Quebec"
badletter <- substr(gsub(".*n","",badstr),1,1)
charToRaw(badletter)
charToRaw("̤")
charToRaw("\x8d") ## Macintosh Latin character set

matchtab <- read.table(sep="|", strip.white=TRUE,
                       header=TRUE, quote="", text = "
regex | replace
La C.*te-De-Gasp.*-Quebec | La Côte-De-Gaspé-Quebec
L'.*rable-Quebec | L'Érable-Quebec
Lotbini.*re-Quebec | Lotbinière-Quebec
Le Haut-Saint-Laurent-Qu.*bec | Le Haut-Saint-Laurent-Québec
Memphr.*magog-Quebec | Memphrémagog-Quebec
Le Haut-Saint-Fran.*ois-Quebec | Le Haut-Saint-François-Quebec
Antoine-Labelle-Qu.*bec | Antoine-Labelle-Québec
La C.*te-de-Beaupr.*-Qu.*bec | La Côte-de-Beaupré-Québec
")

for (i in 1:nrow(matchtab)) {
    matches <- grepl(matchtab$regex[i], county.incidence.by.year$county)
    cat("matched",sum(matches),"\n")
    county.incidence.by.year$county[matches] <- matchtab$replace[i]
}
    
bad <- anti_join(county.incidence.by.year, bothcounties, by="county")
stopifnot(nrow(bad)==0)

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

numbersharedcountiesusers %>% dplyr::filter(grepl("Le Haut.*Fran.*Qu.*",
                                                  county))

write.csv(numbersharedcountiesusers, "data/numbersharedcountiesusers.csv")
#yearone <- numbersharedcountiesusers %>% filter(year == 2008) %>%
 # dplyr::select(-year)

#perfect
#sharedmatrix <- dcast(yearone, county ~ county2)
#Works because 1st column is names is row 1
#gg <- sharedmatrix[,-1]



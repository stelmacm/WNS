library(rlang)
library(esri2sf)

relevant.records <-read.csv("data/relevant-records.csv", header=TRUE) 
wnspresence <- wnspresence[,-c(2,18)]
head(wnspresence)
unique(wnspresence$year)

url2 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/2"
df2 <- as.data.frame(esri2sf(url2))
head(df2)

url5 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/5"
df5 <- as.data.frame(esri2sf(url5))
head(df5)


presence.df <- rbind(df2, df5)
presence.df$rownumber = 1:nrow(presence.df)
presence.poly <- as_Spatial(presence.df$geoms)
#Take the CSV so we have something to go forward with
write.csv(file="whitenosepresence.csv",presence.df) 

unique(presence.df$WNS_STATUS)

#Convert infection status to binary
presence.df$WNS_STATUS[presence.df$WNS_STATUS == "Confirmed"] <- 1
presence.df$WNS_STATUS[presence.df$WNS_STATUS == "Suspect"] <- 0

unique(presence.df$WNS_STATUS)

#Create spatial matrix from all of this
#First fit the counties into spatial set

#THIS IS ALL GARBAGE

presence.df1 <- presence.df[order(presence.df$WNS_MAP_YR),]
presence.df1[1:46,] <- presence.df1$INITIAL == 1
presence.df1$INITIAL[46:nrow(presence.df1),] <- 0
#Ok so this is the first geocache locations found.
#What we could do is just go and set the remaining ones equal to 0
#Or we could keep them as is.
#I think becuase there is such a range in years that 
presence.df2 <- presence.df1[1:46,]


# THIS ISNT GARBAGE

initialvec <- (presence.df$WNS_STATUS)
#ok so that was useless
#If we go through relevant records, we can create the proper weight matrix which we want
#county.m is the metrix in question

#Question I have is my ivec suppose to be just 2008 or the whole column throughout all the years
#nrow of presence.df and county.m is both 727 so I think that is the way to go
#Errors occuring in matrix multiplication because I think WNS_STATUS is a character not numeric
uninf <- (initialvec) == 0
movement <- (county.m)%*%as.numeric(initialvec)
beta <- -0.19
FOI <- beta * movement
hazard <- 1 - exp(FOI)
#I think the size is right. Right?
intialvec[uninf] <- rbinom(sum(uninf), size = 727, prob = hazard)
#Ok so this works fantastically
#Now we need to change a weight matrix per year


source("scripts/packages.R")
source("scripts/wns-presence.R")

shareuserlists <- read.csv("data/fullshareduserslist.csv") %>%
  rename(county = county1) %>%
  rename(year = i.year) %>%
  mutate(year = factor(year)) %>%
  filter(year != 2001) %>%
  filter(year != 2002) %>%
  filter(year != 2003) %>%
  filter(year != 2004) %>%
  filter(year != 2005) %>%
  filter(year != 2006)

## examples
ddd <- data.frame(f=factor(2001:2010))
ddd %>% filter(f==2002) ## coerces 2002 to a string then does the comparison
ddd %>% filter(as.numeric(f)==2) ## operate on codes
## convert *back* to numeric for filtering purposes
## https://cran.r-project.org/doc/FAQ/R-FAQ.html#How-do-I-convert-factors-to-numeric_003f
ddd %>% filter(as.numeric(as.character(f))==2002)
mknum <- function(x) as.numeric(levels(x)[x]) ## R FAQ
ddd %>% filter(mknum(f)==2002) ## operate on codes
ddd %>% mutate(f=ordered(f)) %>% filter(f>2002)  ## works
## using ordered() will set default contrasts to orthogonal polynomials
## instead of coefficients intercept, `2002`, `2003` (diffs from baseline)
## it would give you intercept, 'L' (linear), 'Q' (quadratic), ...
ddd %>% filter(f %in% 2004:2006)


#Going to build other matrix  
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
bothcounties <- reshape2::melt(d1)
## not idempotent
bothcounties <- bothcounties %>% rename(county = Var1, county2 = Var2) 

#Take known data
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))

county.incidence.by.year <- (mixedmodeldf
    ## either document what's missing or ideally
    ## programmatically find the list of 'bad' counties
    %>% filter(!county %in% c("Lewis-Washington",
                              "King-Washington",
                              "Plumas-California"))
    %>% dplyr::select(-yc)
)

countylist <- county.incidence.by.year %>%
  arrange(year) %>%
  filter(year == 2006) %>%
  dplyr::select(county)

azzelinifun <- exp(-((d1)/9.508066)^0.512292358) #Use old vals for now
diag(azzelinifun) <- 0

#Now to weight matrix
localcountymat.w <- mat2listw(azzelinifun, style = "W")
localcountymat.m <- as(localcountymat.w, "CsparseMatrix") 
localcountymat <- as.matrix(localcountymat.m)

#Create forloop to create dataframe
#Create dataframe of county by year filled with incidence
#Retake first year english so that sentence makes sense
for(i in levels(mixedmodeldf$year)){
  countylist[,i] <- county.incidence.by.year %>%
    arrange(year) %>%
    filter(year == i) %>%
    dplyr::select(incidence)
}#
#view(countylist)

m <- matrix(1:16,nrow=4,dimnames=list(LETTERS[1:4],LETTERS[1:4]))
v <- c(B=2, A=1, D=3, C=4)

## if we have a vector that's named and a matrix that has dimnames
v[rownames(m)]  ## sorts v into the same order as rows/cols of m
# or
m[names(v),names(v)] ## the other way around

## or more mechanically
v[match(names(v),rownames(m))]
ord <- match(rownames(m),names(v))
m[ord,ord]
## ?? why match(y,x) not match(x,y) ... ???
thirdlist <- c("C","A","B","D")
v[match(thirdlist,names(v))]
ord <- match(thirdlist,rownames(m))
m[ord,ord]

countylist <- as.data.frame(countylist[,-1])

#Now we need to multiply W_ij by every I_t 
#Another for loop

#Create a base layer to begin with
foidf <- county.incidence.by.year %>%
  arrange(year) %>%
  filter(year == 2006) %>%
  dplyr::select(county)

#Can't order for some reason??

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

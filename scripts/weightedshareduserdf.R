#A simpler weightmatrixsharedusers derived from the original numbersharedusers
numbersharedcountiesusers <- read_csv("data/numbersharedcountiesusers.csv") %>%
  dplyr::select(-X1) %>% mutate(year = factor(year))

weightedsharedusersdf <- data.frame()
for (i in levels(numbersharedcountiesusers$year)) {
  
  sharedusersperyear <- numbersharedcountiesusers %>% filter(year == i) %>%
    dplyr::select(-year)
  
  #perfect
  sharedmatrix <- dcast(sharedusersperyear, county ~ county2)
  #Works because 1st column is names is row 1
  usermatrix <- sharedmatrix[,-1]
  pp <- as.matrix(usermatrix)
  #Set diag = 0
  diag(pp) <- 0
  
  #Now to weight matrix
  sharedusers.w <- mat2listw(pp, style = "W")
  sharedusers.m <- as(sharedusers.w, "CsparseMatrix") 
  sharedusers <- as.matrix(sharedusers.m)
  rownames(sharedusers) <- colnames(usermatrix)
  colnames(sharedusers) <- colnames(usermatrix)
  
  weightedsharedusers <- melt(sharedusers)
  weightedsharedusers$year <- i
  weightedsharedusersdf <- rbind(weightedsharedusersdf, weightedsharedusers)
}

weightedsharedusersdf <- weightedsharedusersdf %>% rename(county = Var1) %>% 
  rename(county2 = Var2) %>% rename(num.shared = value)
  
write.csv(weightedsharedusersdf, "data/weightedshareduserdf.csv")

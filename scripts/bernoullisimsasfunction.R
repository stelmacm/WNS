#A function that just creates sims based on the dataframe built on 
#buildmodelbasedonTMB.R... arguement will just be modeldataframe (df)
#Result will be simdf

set.seed(13)

berniesims <- function(modeldf){
  #Now I need to start with the fist year and remove infection
  modeldf$bernie <- 0
  startingcounties <- modeldf %>% filter(year == "2007") %>% filter(incidence == 1)
  
  yearone <- modeldf %>% filter(year == "2007") %>% filter(incidence ==0)
  
  #Year one
  for (i in 1:nrow(yearone)) {
    yearone[i,6] <- rbinom(1,1,yearone[i,3])
  }
  
  totalinf <- yearone %>% filter(bernie == 1)
  totalinf$yrofinf <- 2007
  
  for (yr in 2008:2018) {
    #Filter year you want
    yearinquestion <- modeldf %>% filter(year == yr)
    #Remove starting counties that were infected
    yearsansstartinginf <- anti_join(yearinquestion, startingcounties, by = "county")
    #Remove the previosly infected counties
    noninfectedcountiesinyr <- anti_join(yearsansstartinginf, totalinf, by = "county")
    
    #Simulate
    for (i in 1:nrow(noninfectedcountiesinyr)) {
      noninfectedcountiesinyr[i,6] <- rbinom(1,1,noninfectedcountiesinyr[i,3])
    }
    #Identify which counties got infected
    yrinf <- noninfectedcountiesinyr %>% filter(bernie == 1)
    if(nrow(yrinf) >= 1)
    {
      yrinf$yrofinf <- yr    #identify the year they were infected in
      totalinf <- rbind(totalinf, yrinf)
    }#Add it to the df that then removes
    #LE FIN
  }
  return(totalinf)
}

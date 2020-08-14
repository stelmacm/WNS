#plan2.0
library(drake)

plantwopointo <- drake_plan(
  
  ### Start with raw list
  # Read in GC list
  gc_dat = { read.csv(file_in("data/gc-list-unfiltered.csv"))
    
    # generate url for scraping
    gc_dat$url<-paste("https://www.geocaching.com/geocache/",gc_dat$GC,"_",gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", gc_dat$title))),sep="")
    #gc_dat #return the whole object
  },
  
  #My first option would be to recode alot of things into code_to_functions
  #Everything should just be in function format
  #That's a bit much and involves lots of redoing
  #Maybe I should just redo so everything is mine and I'm not scared of stepping on needles
  #So that I don't ruin anything of Adrian's
  
  # clean the list of GC sites based on sampling keywords from descriptions
  source("scripts/filter_descriptions.R"),
  
  filtered = file_in(filter.description(gc_dat)),
  
  gc_filtered_dat = na.omit(read.csv(file_in("data/gc-list-filtered.csv",header=T))),
  
  scraped = read.csv(file_in("data/gc-scrape-results.csv",header=T,colClasses = c(rep("factor",2),rep("numeric",2),"Date",rep("factor",2)))),

  # WNS presence at county level polygon
  source("scripts/wns-presence.R"),
  
  # Find overlaps between GC sites and WNS infected counties
  source("scripts/coordinate-overlap.R"),
  
  #Shape files become a real ugly disaster plus they are just static
  # Visualization of movement between sites
  source("scripts/geocache_mapping.R"),
  mapping = mapping(scraped),
  
  # List of all counties within the States/Provinces in original GC list
  counties = read.csv("data/all-counties.csv", header = T) %>%
    separate(1, c("county", "state.province", "Country"), sep = "\t") %>% distinct(),
  
  # Canadian `Counties` shapefile
  can.shape = readOGR("shape/lcd_000b16a_e/lcd_000b16a_e.shp"),
  
  # USA Counties shape file
  usa.shape = maps::map("county", regions = unique(counties[counties$Country == "USA", ]$state.province), fill = T),
  
  # Stich both countries together to create one polygon and fix names
  source("scripts/county-fix.R"),
  
  # Constructs spatial weight matricies from neighbour lists of county centroids
  # source("scripts/spatial-weight-matrix.R"),
  
  # Constructs both adjacency and visit-based weights at the county level in long-format
  source("scripts/geocache-weight.R"),
  
  
  )

questionable_config <- drake_config(plantwopointo)
vis_drake_graph(questionable_config)


##### Drake Plan
library(drake)
plan <- drake_plan(

  ### Start with raw list
  # Read in GC list
  gc_dat = read.csv("data/gc-list-unfiltered.csv",header=T,fill = T,sep = ",",na.strings = "",quote = "",comment.char = ""),
  
  # Take a sample of the description sections from GC pages
  # source("scripts/sample_descriptions.R"),
  # sampled = sample.description(gc_dat),
  
  # clean the list of GC sites based on sampling keywords from descriptions
  # source("scripts/filter_descriptions.R"),
  # filtered = filter.description(gc_dat),
  gc_filtered_dat = na.omit(read.csv("data/gc-list-filtered.csv",header=T)),
  
  # Now scrape just the sites that have these keywords
  # source("scripts/scrape_geocaches.R"),
  # scrape = start.scrape(filtered_dat),
  scraped = read.csv("data/gc-scrape-results.csv",header=T,colClasses = c(rep("factor",2),rep("numeric",2),"Date",rep("factor",2))),
  
  # Where else have these users been?
  # source("scripts/user_lookup.R"),
  # rev_lookup = rev_lookup(scrape_dat),
  
  # Visualization of movement between sites
  # source("scripts/geocache_mapping.R"),
  # mapping = mapping(scrape_dat),
  
  # WNS presence at county level polygon
  source("scripts/wns-presence.R"),
  

  # Find overlaps between GC sites and WNS infected counties
  source("scripts/coordinate-overlap.R"),
  
  # Matching GC sites to locations where we have genetic samples
  # source("scripts/coordinate-match.R"),
  # match = match_locations(scrape_dat,presence),
  
  # List of all counties within the States/Provinces in original GC list
  counties = read.csv("data/all-counties.csv", header = T) %>%
    separate(1, c("county", "state.province", "Country"), sep = "\t") %>% distinct(),

  # Canadian `Counties` shapefile
  can.shape = readOGR("shape/lcd_000b16a_e/lcd_000b16a_e.shp"),

  # USA Counties shape file
  usa.shape = map("county", regions = counties[counties$Country == "USA", ]$state.province, fill = T),

  # Stich both countries together to create one polygon and fix names
  source("scripts/county-fix.R"),
  
  # Constructs spatial weight matricies from neighbour lists of county centroids
  source("scripts/spatial-weight-matrix.R"),

  # Constructs visit-based weights at the county level in long-format
  source("scripts/geocache-weight.R")
  
  # Not Run
  # source("scripts/mantel.R"),
  # mantel = run_mantel(),
  
  # source('examples/ORegan-2016-resources/WNS-GT-Duration.R'),
  # source('examples/ORegan-2016-resources/CountyDurationsScript.R'),
  # source("scripts/CountyDurationScript.R"),
  
  # OReagan_data = OReagan_county(),

  #Read data: US County data (uc)
  # uc=read.csv('examples/ORegan-2016-resources/us_data_dur.csv', header=TRUE),
  #
  # wns_duration = wns_duration(),
  # county_duration = county_duration(),
  
  # source("scripts/sim_runner.R"),
  # run_sim = run_sim(uc,wns_duration,county_duration),
  
  # source("scripts/GLMmodelfitccavedata.R"),
  # glm_model = glm_model(run_sim,overlap)
  )

good_config <- drake_config(plan)
vis_drake_graph(good_config, targets_only = TRUE,file = "figures/drake-plan.png",navigationButtons = F)

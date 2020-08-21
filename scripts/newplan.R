library(drake)
source("scripts/packages.R")
source("scripts/functions.R")

#Apologies things are not indented to fit script size
planthreepointzero <- drake_plan(
    gc_dat = read_csv(file_in("data/gc-list-unfiltered.csv")) %>%
        mutate_at("title", clean_url) %>%
        mutate(url=sprintf("https://www.geocaching.com/geocache/%s_%s",GC,title)),
    wns_presence = get_wns_presence(),
    filter_descriptions=filter.description(gc_dat, file_out("data/gc-list-filtered.csv")),
    #https://stackoverflow.com/questions/43991498/rselenium-server-signals-port-is-already-in-use
    #Close port after scrape
    #Does the dependancy graph show a failure because I still have the port open?
    
    #Gonna skip source("scripts/scrape_geocaches.R")
    #I'm not a selenium expert. Just gonna get the output set up
    scraped = read_csv(file_in("data/gc-scrape-results.csv")) %>%
        mutate(GC = factor(GC),
               Title = factor(Title),
               lon = as.numeric(lon),
               lat = as.numeric(lat),
               Type = factor(Type),
               User = factor(User)),
    #colClasses = c(rep("factor",2),rep("numeric",2),"Date",rep("factor",2)))),
    #I wish I was good at pipping. (There's a mutate that can fix this)
    #Theres also more mutating but I don't know if I should so that here or in the function
    
    # Where else have these users been?
    # source("scripts/user_lookup.R"),
    # rev_lookup = rev_lookup(scrape_dat),
    
    all_results_merged = all.results.merge(scraped), #difference between df and call is *merged* vs *merge*
    #geocache_loc = geocache_locs(all_results_merge),
    
    #This seems legit but says outdated and not passing the same things
    relevant.records = relevant_records(scraped,geocache.locs, presence.df, all_results_merge, presence.poly, file_out("data/relevant-records.csv")),
    # Find overlaps between GC sites and WNS infected counties
    
    #source("scripts/geocache_mapping.R") #Just making pretty pictures :) :)
    #mapping = mapping(scraped)
    
    #Shapes!!!
    #Shapes were moved into functions
    #Stich both countries together to create one polygon and fix names
    county_fix = county.fix(presence.df,presence.poly), #Unsure about this since it has 2 arguements 
                                                       #and the function takes 4. also coming from get_wns

    county.matrix = spatial.weight.matrix(relevant.records.data)    
    
)

good_config <- drake_config(planthreepointzero)
vis_drake_graph(good_config)
make(planthreepointzero)

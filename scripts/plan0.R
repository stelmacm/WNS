##### Drake Plan
library(drake)
plan <- drake_plan(
    gc_dat = { read.csv(file_in("data/gc-list-unfiltered.csv"),header=TRUE,row.names = NULL,fill = TRUE,sep = ",",na.strings = "",quote = "",comment.char = "")
        ## consider 'pins' package
        ## generate url for scraping
        gc_dat$url <- paste("https://www.geocaching.com/geocache/",gc_dat$GC,"_",gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", gc_dat$title))),sep="")
        gc_dat ## return the whole object
    },
    thing = source(file_in("scripts/filter_descriptions.R"))
)
vis_drake_graph(plan)

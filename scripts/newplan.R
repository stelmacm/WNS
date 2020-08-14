library(drake)

planthreepointzero <- drake_plan(
    gc_dat = read_csv(file_in("data/gc-list-unfiltered.csv")) %>%
        mutate_at("title", clean_url) %>%
        mutate(url=sprintf("https://www.geocaching.com/geocache/%s_%s",GC,title)),
    wns_presence = get_wns_presence(),
    filter_descriptions=filter.description(gc_dat, file_out("data/gc-list-filtered.csv"))
)

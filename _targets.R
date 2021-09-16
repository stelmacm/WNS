library(targets)
source("scripts/packages.R")
source("scripts/functions2.R")
url2 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/2"
url5 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/5"
list(
  tar_target(
    raw_data_file,
    "data/gc-list-unfiltered.csv",
    format = "file"
  ),
  tar_target(
    raw_data,
    read_csv(raw_data_file)
  ),
  
  #Reading in raw county incidence data
  tar_target(
   wnspresence,
    get_wns_presence(url2, url5)
  )
  
)

## tar_validate()
## tar_glimpse()

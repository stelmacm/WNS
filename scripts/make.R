### Source packages first
source("scripts/packages.R")

### Drake plan
source("scripts/plan.R")

### Make
make(plan,
     lock_envir = FALSE,
     verbose = 2)


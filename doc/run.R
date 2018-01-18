############################
## Include raw data
############################
setwd("~/Git-work/rreg")
devtools::use_data_raw() #create folder 'data-raw'



#############################
## To compile the package
#############################

library(devtools)
load_all("~/Git-work/rreg")

document("rreg")
check("rreg", manual = TRUE)

## To finish up
build(pkg = "rreg", path = "~/Git-work/Packages", manual = TRUE)

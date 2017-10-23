
###################################################################
## load data and save under ./data folder as .RData or .rda file ##
###################################################################


hfdata <- readRDS("~/Git-work/rreg/data-raw/hfdata.Rds")
devtools::use_data(hfdata, pkg = "rreg", overwrite = TRUE)


yrdata <- readRDS("~/Git-work/rreg/data-raw/ldata.rds")
devtools::use_data(yrdata, pkg = "rreg", overwrite = TRUE)

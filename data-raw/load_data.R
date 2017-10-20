################
## load data
################

hfdata <- readRDS("~/Git-work/rreg/data-raw/hfdata.Rds")
devtools::use_data(hfdata, pkg = "rreg", overwrite = TRUE)

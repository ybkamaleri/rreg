## To compile the package

library(devtools)
load_all("rreg")

document("rreg")
check("rreg", manual = TRUE)

## To finish up
build(pkg = "rreg", path = "~/Git-work/Packages", manual = TRUE)

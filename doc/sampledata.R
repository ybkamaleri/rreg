## Health institutions
health <- c("Tawau HF",
            "Sandakan HF",
            "Kota Kinabalu HF",
            "Semporna HF",
            "Lahad Datu HF",
            "Kunak HF",
            "Kalabakan HF",
            "Merotai HF",
            "Apas Balung HF",
            "Sri Menanti HF",
            "Keningau HF",
            "Putatan HF",
            "Tanjung Aru HF",
            "Sipadan HF",
            "Pulau Mabul HF")

id <- 1:15
v2005 <- sample(10:100, size = 15, replace = TRUE)
v2006 <- sample(10:100, size = 15, replace = TRUE)
v2007 <- sample(10:100, size = 15, replace = TRUE)
v2003 <- sample(10:100, size = 15, replace = TRUE)
v2004 <- sample(10:100, size = 15, replace = TRUE)
vcase <- rnorm(15, 60, 10)
vnorm <- c(rnorm(15, 20, 3))
vext <- round(c(22, 85, runif(12, 18, 25), 90), digits = 0)

data <- data.frame(health, id, v2003, v2004, v2005, v2006, v2007, vcase, vnorm, vext)

library(data.table)
setDT(data)

## change col names
setnames(data, 3:ncol(data), gsub("v", "", names(data)[3:ncol(data)]))
setnames(data, "health", "center")

saveRDS(data, "~/Git-work/rreg/data/hfdata.Rds")
hfdata <- readRDS("~/Git-work/rreg/doc/hfdata.Rds")

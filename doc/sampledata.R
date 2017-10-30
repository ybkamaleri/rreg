
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
            "Sabah",
            "Kuhara HF",
            "Sipadan HF",
            "Pulau Mabul HF")

id <- 1:16
v2005 <- sample(10:100, size = 16, replace = TRUE)
v2006 <- sample(10:100, size = 16, replace = TRUE)
v2007 <- sample(10:100, size = 16, replace = TRUE)
v2003 <- sample(10:100, size = 16, replace = TRUE)
v2004 <- sample(10:100, size = 16, replace = TRUE)
vcase1 <- round(rnorm(16, 60, 25), digits = 1)
vcase2 <- round(c(rnorm(16, 20, 2)), digits = 0)
vextt <- round(c(22, 85, runif(12, 18, 25), 90, 87), digits = 0)
vlim <- sample(2:8, 16, replace = TRUE)
gender <- rep(c("male", "female"), 8)
age <- sample(20:60, size = 16, replace = FALSE)


data <- data.frame(health, id, gender, age, v2003, v2004, v2005, v2006, v2007, vcase1, vextt, vcase2, vlim)
data$vll <- with(data, vcase2 - vlim)
data$vul <- with(data, vcase2 + vlim)
data$agegp <- cut(data$age, breaks = seq(10, 60, 5), include.lowest = TRUE)

library(data.table)
setDT(data)

## change col names
setnames(data, 3:ncol(data), gsub("v", "", names(data)[3:ncol(data)]))
setnames(data, "health", "inst")


saveRDS(data, "~/Git-work/rreg/data-raw/hfdata.Rds")

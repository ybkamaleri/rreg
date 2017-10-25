## run function in a folder
fpath <- "~/OUS/BDR/Filer/"
ftype <- ".csv"

## list of filenames
filelist <- list.files(path = fpath, pattern = ftype)

library(rreg)
library(ggplot2)

for (i in 1:length(filelist)){
  fname <- tools::file_path_sans_ext(filelist[i])
  data1 <- read.csv2(paste0(fpath, filelist[i]), header = TRUE, encoding = "latin1")
  var1 <- substitute(names(data1[1]))
  var2 <- substitute(names(data1[2]))
  regbar(data1, eval(var1), eval(var2), long = TRUE)

  ggplot2::ggsave(sprintf("%s.png", fname), scale = 1.1)
  dev.off()
}


##########################################
## Guna atau tidak guna quote di fungsi ##
##########################################

ab <- data.frame(a = letters[1:5], b = 1:5)
ab

testfun <- function(a, x) {

  x <- as.character(substitute(x))
  ## x <- deparse(substitute(x))

  ## if(!is.character(x)){
  ##   x <- deparse(substitute(x))
  ## } else { x = x}

  print(class(x))
  a$d <- a[x]
  c <- sum(a$d)
  return(c)
}

testfun(ab, b)


a <- as.character(substitute(t))
class(a)
b <- as.character(substitute("t"))
class(b)

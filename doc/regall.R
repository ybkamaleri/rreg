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

ab <- data.frame(a = letters[1:5], b = 1:5, c = 6:10)
names(ab)[3] <- "f"
ab

testfun <- function(a, x) {

  ## if (!is.data.frame(a)) a <- data.frame(a)

  if (typeof(x) == "list") {
    x <- names(x)
  } else {
    x <- as.character(substitute(x))
  }

  print(class(x))
  d <- grep(x, names(a))
  print(d)
  e <- sum(a[d])
  return(e)
}

testfun(ab, ab[3])
testfun(ab, ab[2])



testfun(ab, "b")
testfun(ab, f)





testfun(mtcars, cyl)

ab["b"]

test1 <- function(a, x) {
  x <- substitute(x)
  print(class(x))
  return(x)
}

test1(ab, b)
test1(mtcars, cyl)


test2 <- function(a, x) {

  x <- as.character(substitute(x))

  b <- grep(x, names(a))
  sum(a[b])


}

test2(ab, c)
test2(ab, "b")



test2(mtcars, cyl)


a <- substitute(ab[3])
class(a)


b <- ab[3]
class(b)
names(ab[3])

ab

a <- as.character(substitute(t))
class(a)
b <- as.character(substitute("t"))
class(b)

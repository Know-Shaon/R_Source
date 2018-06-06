#http://adv-r.had.co.nz/Functions.html

f <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
rm(x,y)

#--------------------------------
x <- 2
g <- function() {
  y <- 1
  c(x, y)
}
rm(x,y)

#--------------------------------
x <- 1
h <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}

#--------------------------------

f <- function(abcdef, bcde1, bcde2) {
  list(a = abcdef, b1 = bcde1, b2 = bcde2)
}
str(f(1, 2, 3))

#-------------------------------

f <- function(x) {
  10
}
f(stop("This is an error!"))


f <- function(x) {
  force(x)
  10
}
f(stop("error!"))

#-----------------------------
add <- function(x) {
  function(y) x + y
}
adders <- lapply(1:10, add)
adders[[3]](10)
#-----------------------------

hist(rbeta(10000,2,10))
hist(log(rbeta(10000,2,10)))

hist(rbeta(10000,4,14))
data=rbeta(10000,4,14)
hist(sqrt(data))
#------------------------------

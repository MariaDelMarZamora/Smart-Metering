r1 <- function(tScore, df){
  sqrt(tScore ^2 / (tScore^2 +df))
}

r2 <- function(zScore, n){
  sqrt(zScore^2 / n)
}

r3 <- function(xScore, n) {
  sqrt(xScore^2 / n)
}

VarR <- function(r, n) {
  (1-r^2)^2 / (n-1)
}

Zr5 = function(r) {
  0.5* log((1+r) / (1-r))
}

VarZr <- function(n){
  1/(n - 3)
}

Obar <- function(weight, eSize){
  sum(weight*eSize)/sum(weight)
} #lapply? across studies once weights and effect sizes have been calculated

r1(3.06, 29202)
Zr5(0.02)
VarZr(29202)

r1(3.38, 112)
Zr5(0.3042)
VarZr(112)



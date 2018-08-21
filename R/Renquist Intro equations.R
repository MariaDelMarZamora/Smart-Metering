## INTRODUCTION ####
# R-based effect Sizes ####
# Partial correlation coefficient as well as traditional. 
# Eq. 3.1 can be used to calculate the partial correlation coefficient associated with
# a regression parameter estimate in any regression model
#r1 3.1
r1 <- function(tScore, df){
  sqrt(tScore ^2 / (tScore^2 +df))
}

#r2 eq. 3.3
r2 <- function(zScore, n){
  sqrt(zScore^2 / n)
}

#r3 3.4
r3 <- function(xScore, n) {
  sqrt(xScore^2 / n)
}

#3.5
tcalc <-function(b,s){
  b/s
}
t<-tcalc(b,s)

r4 <- function(t,df){
  sqrt(t^2/(t^2 + df))
}


#VarR 3.2
VarR <- function(r, n) {
  (1-r^2)^2 / (n-1)
}



Obar <- function(weight, eSize){
  sum(weight*eSize)/sum(weight)
} #lapply? across studies once weights and effect sizes have been calculated

r1(3.06, 29202)
Zr5(0.02)
VarZr(29202)

r1(3.38, 112)
VarR(0.3042,116)

r1(3.09,29202)
VarR(r1(3.09,29202),29214)

Zr5(0.3042)
VarZr(112)

# Fisher's Z ###
# eq. 3.8
Zr5 = function(r) {
  0.5* log((1+r) / (1-r))
}

#eq. 3.9
VarZr <- function(n){
  1/(n - 3)
}
# D-based effect sizes ####

#eq 3.10 (Cohen's d)
cohenD <- function(meanA, meanB, sd){
  (meanA - meanB)/sd
}

# eq. 3.12 Pooled within group sample standard deviation
spooled <- function(nA, nB,sA,sB){
  sqrt(((nA-1)*sA^2 + (nB-1)*sB^2)/(nA + nB -2))
}

# eq. 3.11-3.15 Cohen's d for independent groups
indCohenD <- function(meanA = NA, meanB = NA, spool, type = "norm", measure = NA, nA = NA, nB = NA){
  if (type == "norm") {
    d = (meanA - meanB)/spool
  }
  else if (type == "t" | type == "Z"){
    d = measure*sqrt((nA+nB)/nA*nB)
  }
  else if (type == "F"){
    d = sqrt(measure*(nA+nB)/nA*nB)
  }
  # else if (type = "p"){
  #  d = measure 
  else { 
    print("Chose type: norm, t, Z, F, or p")
  }
  print(d) 
}

# eq. 3.17 Variance of independent group Cohen's d
vard <- function(nA, nB, d){
  ((nA+nB)/(nA*nB))+((d^2)/(2*(nA+nB)))
}



spool <- spooled(275, 239, 26.72, 29.07)

d<-indCohenD(meanA = 534.63, meanB = 525.33, spool = spool, type = "norm")
var<-vard(275,239,d)

# Hedge's g Eq. 3.21-3.22
hedgeg <- function(df,d){
  j <<- 1-(3/((4*df )-1))
  g <- j*d
  print(g)
}

hedgeVar <- function(j,v){
  j^2 * v
}
hedgeg(512,d)
hedgeVar(j,var)

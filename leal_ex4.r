this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("leal_ex3.r") 

Gaussian <- function(augCoeffMatrix){

  a = augCoeffMatrix$augcoeffmatrix
  n = length(augCoeffMatrix$variables)
  print(a)
  for (i in 1:(n-1)){
    pivotRow = row(max(abs(a[i:n,i])))
    
    for (j in (i+1):n){
      pivotEl = a[i,i]
      multiplier = a[j,i]/pivotEl
      nr = a[i,] * multiplier
      a[j,] = a[j,] - nr
    }
  }
  print(a)
}

swap <- function(pivotRow,ai){
  temp = ai
  ai = pivotRow
  pivotRow = temp
  
}

E2 <- function (x1, x2, x3) 0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4;
E1 <- function (x1, x2, x3) 3 * x1 + -0.2 * x3 + -0.1 * x2 + -7.85;
E3 <- function (x1, x2, x3) 0.1 * x1 + 7 * x2 + -0.3 * x3 + 19.3;
system <- list(E1, E2, E3);
result <- AugCoeffMatrix(system)

Gaussian(result)
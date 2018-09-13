this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("leal_ex3.r") 

findRow <- function(maxValue, i, a){
  n = nrow(a)
  for (j in 1:n){
    if (a[j,i]==maxValue){
      return (list(index = j,row = a[j,]))
    }
  }
}

backwardElimination <- function(a){
  b=a[,"RHS"]
  n = nrow(a)
  x=NULL
  
  for (i in n:1){
    x[i] = (b[i] - sum(a[i, (i+1):n] * x[(i+1):n])) / a[i,i]
  }
  return (x)
}

Gaussian <- function(augCoeffMatrix){
  a = augCoeffMatrix$augcoeffmatrix
  n = length(augCoeffMatrix$variables)
  xvalues = NULL

  for (i in 1:(n-1)){
    maxValue = (max(abs(a[i:n,i])))
    pivotRow = findRow(maxValue,i,a)
    #swap
    a[pivotRow$index,]=a[i,]
    a[i,]=pivotRow$row
    for (j in (i+1):n){
      pivotEl = a[i,i]
      multiplier = a[j,i]/pivotEl
      nr = a[i,] * multiplier
      a[j,] = a[j,] - nr
    }
  }
  x = backwardElimination(a)
  return (list(solutionSet = x, augCoeffMatrix = augCoeffMatrix$augcoeffmatrix, variables = augCoeffMatrix$variables ))
}

GaussJordan <- function(augCoeffMatrix){
  a = augCoeffMatrix$augcoeffmatrix
  n = length(augCoeffMatrix$variables)

  for (i in 1:n){
    if (i!=n){
      maxValue = (max(abs(a[i:n,i])))
      pivotRow = findRow(maxValue,i,a)
      #swap
      a[pivotRow$index,]=a[i,]
      a[i,]=pivotRow$row
    }
    a[i,] = a[i,]/a[i,i]
    for (j in 1:n){
      if (i==j){
        next
      }
      nr = a[j,i]*a[i,]
      a[j,]= a[j,] - nr
    }
  }
  x = backwardElimination(a)
  return (list(solutionSet = x, augCoeffMatrix = augCoeffMatrix$augcoeffmatrix, variables = augCoeffMatrix$variables ))
}

E1 <- function (x1, x2, x3) 144 * x1 + 12 * x2 + 1 * x3 + -279.2;
E2 <- function (x1, x2, x3) 64 * x1 + 8 * x2 + 1 * x3 + -177.2;
E3 <- function (x1, x2, x3) 25 * x1 + 5 * x2 + 1 * x3 + -106.8;
system <- list(E2, E3, E1);
result <- AugCoeffMatrix(system)

gaussianElim = Gaussian(result)
gaussJordanElim = GaussJordan(result)
print("gaussian elimination")
print(gaussianElim)
print("gauss jordan")
print(gaussJordanElim)
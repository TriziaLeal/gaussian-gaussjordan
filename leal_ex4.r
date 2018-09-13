this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("leal_ex3.r") 

Gaussian <- function(augCoeffMatrix){
  a = augCoeffMatrix$augcoeffmatrix
  n = length(augCoeffMatrix$variables)
  xvalues = NULL

  for (i in 1:(n-1)){
    maxValue = (max(abs(a[i:n,i])))
    pivotRow = findRow(maxValue,i,a)

    a[pivotRow$index,]=a[i,]
    a[i,]=pivotRow$row

    for (j in (i+1):n){
      pivotEl = a[i,i]
      multiplier = a[j,i]/pivotEl
      nr = a[i,] * multiplier
      a[j,] = a[j,] - nr
    }
  }
  b=a[,"RHS"]
  x=NULL
  print(a)
  for (i in n:1){
    x[i] = (b[i] - sum(a[i, (i+1):n] * x[(i+1):n])) / a[i,i]
  }
#  print(a)
#  print(x)
}

swap <- function(a,pivotRow,ai){
  temp = ai
  ai = pivotRow
  pivotRow = temp
}

findRow <- function(maxValue, i, a){
  n = nrow(a)
  for (j in 1:n){
    if (a[j,i]==maxValue){
      return (list(index = j,row = a[j,]))
    }
  }
}

GaussJordan <- function(augCoeffMatrix){
  a = augCoeffMatrix$augcoeffmatrix
  n = length(augCoeffMatrix$variables)

  for (i in 1:n){
    if (i!=n){
      maxValue = (max(abs(a[i:n,i])))
      pivotRow = findRow(maxValue,i,a)
      
      a[pivotRow$index,]=a[i,]
      a[i,]=pivotRow$row
    }
    a[i,] = a[i,]/a[i,i]
    for (j in 1:n){
      print(j)
      if (i==j){
        next
      }
      nr = a[j,i]*a[i,]
      a[j,]= a[j,] - nr
    }
    
    b=a[,"RHS"]
    x=NULL
    
    for (i in n:1){
      x[i] = (b[i] - sum(a[i, (i+1):n] * x[(i+1):n])) / a[i,i]
    }
  }
  print(a)
  print(x)
}

E1 <- function (x1, x2, x3) 144 * x1 + 12 * x2 + 1 * x3 + -279.2;
E2 <- function (x1, x2, x3) 64 * x1 + 8 * x2 + 1 * x3 + -177.2;
E3 <- function (x1, x2, x3) 25 * x1 + 5 * x2 + 1 * x3 + -3;
system <- list(E2, E1, E3);
result <- AugCoeffMatrix(system)

#Gaussian(result)
GaussJordan(result)
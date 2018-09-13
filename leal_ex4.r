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
  for (i in 1:(n-1)){
    if (i!=n){
      maxValue = (max(abs(a[i:n,i])))
      pivotRow = findRow(maxValue,i,a)
      
      a[pivotRow$index,]=a[i,]
      a[i,]=pivotRow$row
    }
    a[i,] = a[i,]/a[i,i]
    for (j in i:n){
      if (i==j){
        next
      }
      nr = a[j,i]*a[i,]
      a[j,]:a[j,] - nr
    }
    print(a)
    
    b=a[,"RHS"]
    x=NULL
    
    for (i in n:1){
      x[i] = (b[i] - sum(a[i, (i+1):n] * x[(i+1):n])) / a[i,i]
    }
    print(a)
    print(x)
  }
}

E1 <- function (x1, x2) 3 * x1 + 4 * x2 + -14;
E2 <- function (x1, x2) 5 * x1 + -7 * x2 + -3;
system <- list(E1, E2);
result <- AugCoeffMatrix(system)

#Gaussian(result)
GaussJordan(result)
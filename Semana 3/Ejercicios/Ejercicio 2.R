###Ejercicio 2

fact_cholesky = function(A){
  n = ncol(A)
  i = 1
  L = matrix(data = 0, nrow = n, ncol = n, byrow = TRUE) 
  #Paso 1
  L[1,1] = (A[1,1])^(0.5)
  #Paso 2
  j = i + 1
  for (j in j:n) {
    L[j,1] = A[j,1]/L[1,1]
  }
  #Paso 3
  for (i in (i+1):(n-1)) {
    #Paso 4
    sum1 = 0
    k = 1
    for (k in k:(i - 1)) {
      sum1 = sum1 + L[i,k]^2
    }
    L[i,i] = sqrt(A[i,i] - sum1)
    #Paso 5
    for (j in (i+1):n) {
      sum2 = 0
      k = 1
      for (k in k:(i - 1)) {
        sum2 = sum2 + (L[j,k]*L[i,k])
      }
      L[j,i] = (A[j,i] - sum2)/L[i,i]
    }
  }
  #Paso 6
  sum3 = 0
  k = 1
  for (k in k:(n - 1)) {
    sum3 = sum3 + (L[n,k]^2)
  }
  L[n,n] = sqrt((A[n,n] - sum3))
  #Paso 7
  return(L)
}

## A----

A <- matrix(c(2,-1,0,
              -1,2,-1,
              0,-1,2),3,3,TRUE)
A

La <- fact_cholesky(A)
Lat <- t(La)

La %*% Lat

## B -----

B <- matrix(c(6,2,1,-1,
              2,4,1,0,
              1,1,4,-1,
              -1,0,-1,3),4,4,TRUE)
B

Lb <- fact_cholesky(B)
Lbt <- t(Lb)

Lb %*% Lbt
## C ----

C<- matrix(c(4,1,1,1,
             1,3,-1,1,
             1,-1,2,0,
             1,1,0,2),4,4,TRUE)
C

Lc <- fact_cholesky(C)
Lct <- t(Lc)

Lc %*% Lct
## D ----

D <- matrix(c(1,2,4,7,2,13,23,38,4,23,77,122,7,38,122,294),4,4,TRUE)

D

Ld <- fact_cholesky(D)
Ldt <- t(Ld)

Ld %*% Ldt
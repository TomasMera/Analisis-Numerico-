##Ejercicio 4
Neville_3.1 <- function(x, y, x0) {
  
  n <- length(x)
  q <- matrix(data = 0, n, n)
  q[,1] <- y
  
  #Paso 1
  
  for (i in 2:n) {
    for (j in i:n) {
      q[j,i] <- (((x0 - x[j-i+1]) * q[j,i-1] - (x0 - x[j]) * q[j-1,i-1])) / (x[j] - x[j-i+1])
    }
  }
  
  #Paso 2
  
  res <- list('Valor aproximado'=q[n,n], 'Tabla de Iteraciones'=q)
  return(res)
  
  return(q[n,n])
}

## A ----
x =c(0,0.25,0.5,0.75)
y =c(1,1.64872,2.71828,4.48169)
x0 =  0.43

Neville_3.1(x,y,x0)

## B ----
x= c(-0.5,-0.25,0.25,0.5)
y = c(1.93750,1.33203,0.800781,0.687500)
x0 = 0

Neville_3.1(x,y,x0)

## C ----
x = c(0.1,0.2,0.3,0.4)
y = c(-0.29004986,-0.56079734,-0.81401972,-1.0526302)
x0 = 0.18

Neville_3.1(x,y,x0)

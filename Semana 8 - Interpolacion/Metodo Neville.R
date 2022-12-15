rm(list=ls())
graphics.off()

#Ejercicio a ----
# 1) Cargamos los datos ----
x <- c(0,0.25,0.5,0.75)
y <- c(1,1.64872,2.71828,4.48169)

TablaInicial<-data.frame(x,y)

# 2) Algoritmo de Neville ----
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
# 3) Resolucion ----
options(scipen = 100, digits= 7)
Neville_3.1(x,y,0.43)


##Ejercicio 4

Funcion <- function(x){
  f<- x^3 - x - 1
  return(f)  
}

g <- function(x){ # Funcion g(x) para la iteracion de punto fijo
  gn <- x^3 -1 
  return(gn)
}

Punto_Fijo <- function (P0,TOL,N){
  i=1
  while(i<=N){
    p <- g(P0)
    if(abs(p -P0) < TOL){
      return(p)
    }
    i=1+i
    P0=p
  }
  return(paste("El metodo fracaso luego de ",N," iteraciones"))
}


punto_fijo(1,10^-2,100)

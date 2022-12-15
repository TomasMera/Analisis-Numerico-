##Ejercicio 3

Funcion <- function(x){
  f<- x^4-3*x^2-3 
  return(f)  
}

g <- function(x){ # Funcion g(x) para la iteracion de punto fijo
  gn <-(3*x^2+3)^(1/4)
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


Punto_Fijo(1,10^-2,100)

##Ejercicio 3

Funcion <- function(x){
  f <- 2*x*cos(2*x) - (x+1)^2
  return(f)
}

Raiz_Biseccion <- function(a,b,TOL,N){
  i=1
  FA = polinomio(a)
  
  while(i<=N){
    p = a+(b-a)/2
    FP = polinomio(p)
    if(FP == 0 | (b-a)/2 < TOL){
      return(p)
    }
    i=i+1
    if(FA*FP > 0 ){
      a=p
      FA = FP
    }else{
      b=p
    }
  }
  return(paste("El metodo fracaso despues de ", N," iteraciones"))
}


Raiz_Biseccion(-3,-2,0.00001,100)

Raiz_Biseccion(-1,0,0.00001,100)
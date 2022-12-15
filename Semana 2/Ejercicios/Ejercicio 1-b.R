##Ejercicio 2

Funcion <- function(x){
  f <- exp(x) - x^2 +3*x - 2
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


Raiz_Biseccion(0,1,0.00001,100)


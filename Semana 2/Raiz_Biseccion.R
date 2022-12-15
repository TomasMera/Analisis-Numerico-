#Defino el polinomio al que le voy a hacer la raiz

polinomio <-function(x){
  f <- cos(x) - sqrt(x)
  f
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

##Chequeo que el valor de f(a) * f(b) <0
a=6
b=2  

polinomio(a)
polinomio(b)

Raiz_Biseccion(a,b,0.00001,100)
  
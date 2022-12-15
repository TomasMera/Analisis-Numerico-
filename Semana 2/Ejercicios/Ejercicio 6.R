##Ejercicio 6


Funcion <- function(x){
  f <- ##Funcion que queremos hacerle las raices con Newton Rawson
    -x^3 -cos(x)
  return(f)  
}

Funcion_Deriv <- function(x){
  f<- ##Escribimos la funcion Derivada, hay que derivar manualmente
    -3*x^2 - sin(x)
  return(f)    
}

f<-expression(-x^3 -cos(x))  #expresion que quiero derivar
fprima<-D(f,"x")
fprima  #derivada


Newton_Rawson <- function(P0, TOL, N){
  i=1
  while(i<=N){
    p = P0 - Funcion(PO)/Funcion_Deriv(PO)
    if(abs(p - P0) < TOL){
      return(p)
    }
    i= i + 1
    PO = p
  }
  print(paste("El metodo fracso luego de ", N," iteraciones"))
}


Newton_Rawson(-1,0.00001,10000)
Newton_Rawson(0,0.0001,10000)
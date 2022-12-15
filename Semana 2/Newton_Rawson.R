
Funcion <- function(x){
  f <- ##Funcion que queremos hacerle las raices con Newton Rawson
    exp(x)+2^-x + 2*cos(x) - 6
  return(f)  
}

Funcion_Deriv <- function(x){
  f<- ##Escribimos la funcion Derivada, hay que derivar manualmente
    exp(x)+ 2^(-x) * log(2) * sin(x)
return(f)    
}

f<-expression(exp(x) + 2^(-x) + 2*cos(x) - 6)  #expresion que quiero derivar
fprima<-D(f,"x")
fprima  #derivada


Newton_Rawson <- function(P0, TOL, N){
  i=1
  while(i<=N){
    p = P0 - Funcion(P0)/Funcion_Deriv(P0)
    if(abs(p-P0) < TOL){
      return(p)
    }
    i= i + 1
    P0 = p
  }
  print(paste("El metodo fracso luego de ", N," iteraciones"))
}


##Debo elegir un P0 masomenos cerca de la Raiz, debo graficar la funcion


Newton_Rawson(0.1,0.00001,100)
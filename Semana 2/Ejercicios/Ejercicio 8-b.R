##Ejercicio 8 - b


Funcion <- function(x){
  f <- ##Funcion que queremos hacerle las raices con Newton Rawson
    x^3 - 3*x^2 -1  
  return(f)  
}

f<-expression(x^3 - 3*x^2 -1 )  #expresion que quiero derivar
fprima<-D(f,"x")
fprima  #derivada


Funcion_Deriv <- function(x){
  f<- ##Escribimos la funcion Derivada, hay que derivar manualmente
    3 *x^2 - 3*(2*x)
  return(f)    
}

x <- seq(-3,-2, by = 0.1) 
 
plot(x, funcion(x))
abline(h=0)


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


Newton_Rawson(3.0,10^-4,100)

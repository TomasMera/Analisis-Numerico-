##Ejercicio 8 - a


Funcion <- function(x){
  f <- ##Funcion que queremos hacerle las raices con Newton Rawson
    x^3 - 2*x^2 - 5
  return(f)  
}

f<-expression(x^3 - 2*x^2 - 5)  #expresion que quiero derivar
fprima<-D(f,"x")
fprima  #derivada


Funcion_Deriv <- function(x){
  f<- ##Escribimos la funcion Derivada, hay que derivar manualmente
    3 * x^2 - 2*(2*x)
  return(f)    
}

x <- seq(1,4, by = 0.2) 
 
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


Newton_Rawson(2.0,10^-4,100)
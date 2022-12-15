##Ejercicio 9 - a
Funcion <- function(x){
  f<- x^3 - 2*x^2 -5
  return(f)
}

Secante <- function(p0, p1 , tol , N){
  i=2
  q0=Funcion(p0)
  q1=Funcion(p1)
  while(i<=N){
    p=p1 - q1*(p1 - p0)/(q1 - q0)
    if(abs(p-p1)<tol){
      return(p)
    }
    i=i+1
    q0=q1
    p1=p
    q1=Funcion(p)
  }
  return(paste("El metodo fallo luego de ",N," iteraciones"))
}

Funcion(2)
Funcion(5)

#Chequear que f(p0) * f(p1)<0
Secante(2, 5 , 10^-4 , 100)


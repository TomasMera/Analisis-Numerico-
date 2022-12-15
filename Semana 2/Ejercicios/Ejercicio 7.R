#Ejercicio 7

Funcion <- function(x){
  f<- x^2 - 6
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

Falsa_Posicion <- function(p0, p1 , tol , n){
  i=2
  q0=Funcion(p0)
  q1=Funcion(p1)
  while(i<=n){
    p=p1 - q1*(p1 - p0)/(q1 - q0)
    if(abs(p-p1) < tol){
      return(p)
    }
    i= i+1
    q = Funcion(p)
    if(q*q1 <0){
      p0=p1
      q0=q1
    }
    p1=p
    q1=q
  }
  return(paste("El metodo fallo luego de ",n," iteraciones"))
}



Secante(3, 2 , 0.001 , 10000)

Falsa_Posicion(3, 2, 0.00001,10000)

Funcion <- function(x){
  f <- exp(x) + 2^(-x) + 2*cos(x) -6
  return(f)
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


#Chequear que f(p0) * f(p1) <0
Falsa_Posicion(1 , 2 , 0.0001 , 100)

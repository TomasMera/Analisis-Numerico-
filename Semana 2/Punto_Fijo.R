funcion <- function(x){
  f<-##funcion que quiera: 
    x - x^3 - 4*x^2 +10
  return(f)  
}

#Funcion para hacer la iteracion de pto Fijo Del tipo x = g(x). Hallar la raiz f(x)=0.
# Es lo mismo que hallar el pto fijo g(x)=x.#g(x) la obtengo despejando x.
g<- function(x){
  gn <- x^3 + 4*x^2 - 10   
  return(gn)             
}                        
                          

##Formas de obtener G(x) ----
#Despejando X
#G(X) = x - f(x)
#g(x) = x - f(X)/f'(x)



##para derivar
gd <- expression(x^3 + 4*x^2 - 10)
dg <- D(gd,x)
dg                         ### En cualquier punto de [a,b] debe ser <1 (Pto Fijo unico)
                           ### Si no es menor a 1, no va a converger. Debo elegir otro G(x)
                           ### O modifico el intervalo.

Punto_Fijo <- function (P0,TOL,N){
  i=1
  while(i<=N){
    p <- g(P0)
    if(abs(p-P0) < TOL){
      return(p)
    }
    i=1+i
    P0=p
  }
  return(paste("El metodo fracaso luego de ",N," iteraciones"))
}


funcion(4)

punto_fijo(4,00001,100)

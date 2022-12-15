#Ejercicio 1 
Metodo_Euler <- function(a,b,N,alpha){
  
  h= (b-a)/N
  t= a
  
  w = matrix(rep(NA, N+1), ncol =1)
  w[1] = alpha
  
  for (i in 1:N) {
    w[i+1]= w[i] + h *f(t,w[i])
    t = a + i*h
  }
  t = matrix(seq(a,b,h), ncol = 1)
  resultado <- matrix(c(t,w), ncol = 2 , byrow = FALSE )
  colnames(resultado)= c('t', 'w')
  return(resultado)
}

#A ----

#Funcion que se va a aproximar
f <- function(t,y) {t-y + 2}

#Extremos y valor inicial
a=0
b= 1
alpha = 2  

#Cantidad de Iteraciones
N = 10

#RESPUESTA
Metodo_Euler(a,b,N,alpha)


#B ---- 
#Funcion que se va a aproximar
f <- function(t,y) {y - t^2 + 1}

#Extremos y valor inicial
a=0
b= 2
alpha = 0.5  

#Cantidad de Iteraciones
N = 10

#RESPUESTA
Metodo_Euler(a,b,N,alpha)

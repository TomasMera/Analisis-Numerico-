#Ejercicio 5

#Funcion que se va a aproximar
f <- function(t,y) {(2/t) *y + t^2 *exp(t)}

#Extremos y valor inicial
a=1
b= 2
alpha= 0
h = 0.1

#Cantidad de Iteraciones
#h = (b-a)/N
N = (b-a) / h

Metodo_Euler <- function(a,b,alpha,N){
  
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
  colnames(resultado)= c('t', 'y')
  return(resultado)
}

#Aprox de y
Aprox_de_Y <- Metodo_Euler(a,b,alpha,N)


#Sol Exacta
y <- function(t){
  t^2 *(exp(t)- exp(1))
}

t=matrix(seq(a,b,h),ncol = 1)
  
sol_exacta= matrix(y(t),ncol=1)

#sol_aprox de Y'
sol_aprox_yprima = matrix(f(t,Metodo_Euler(a,b,alpha,N)[,2]), ncol = 1)

Matriz_Comparacion = matrix(c(Aprox_de_Y, sol_aprox_yprima, sol_exacta), ncol = 4)
colnames(Matriz_Comparacion) = c("t","y", "y'", "y(t)")

Matriz_Comparacion                   

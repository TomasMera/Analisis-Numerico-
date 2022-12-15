#Funcion que se va a aproximar
f <- function(t,y) {y - t^2 +1}

#Extremos y valor inicial
a=0
b= 2
alpha = 0.5  

#Cantidad de Iteraciones
N = 10

RK4 <- function(a,b,N,alpha){
  
  h= (b-a) / N
  w = matrix(rep(NA, N+1), ncol =1)
  w[1] = alpha
  t = a
  
  for (i  in 1:N) {
    
    K1 = h * f(t , w[i])
    K2 = h * f(t + h/2 , w[i] + K1/2)
    K3 = h * f(t + h/2 , w[i] + K2/2)
    K4 = h * f(t + h , w[i] + K3)
    
    w[i+1] = w[i] + (K1 + 2*K2 + 2*K3 + K4)/6
    t = a + i*h
  }
  t = matrix(seq(a,b,h), ncol = 1)
  resultado <- matrix(c(t,w), ncol = 2 , byrow = FALSE )
  colnames(resultado)= c('t', 'w')
  return(resultado)
}

RK4(a,b,N,alpha)

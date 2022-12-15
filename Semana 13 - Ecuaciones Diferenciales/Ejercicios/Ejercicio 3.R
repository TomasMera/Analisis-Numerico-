#Ejercicio 3

#Funcion que se va a aproximar
f <- function(t,y) {exp(sin(t*y)) + sin(log(y))}

#Extremos y valor inicial
a=0
b= 4 * pi
alpha = 0.01  

#Cantidad de Iteraciones
N = 100

#A-----

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

Matriz_Resultado1 <- Metodo_Euler(a,b,N,alpha)

M1 <- data.frame(Matriz_Resultado1)

#Resultado
M1[M1$t == 3*pi,]




# B ----

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


Matriz_Resultado2 <- RK4(a,b,N,alpha)

M2 <- data.frame(Matriz_Resultado2)

#Resultado
M2[M2$t == 3*pi,]


#Grafico
plot(M1$t , M1$w,col ="black", type = "l", ylab = "y(t)", xlab = "t", xlim = c(0,4*pi))
lines(M2$t , M2$w , col="red")
points(x = M2[M2$t == 3*pi,1], y = M2[M2$t == 3*pi,2], cex = 0.7)
points(x=M1[M1$t == 3*pi,1], y =M1[M1$t == 3*pi,2], cex= 0.7 )
title("Solucion del problema : y' = exp(sin(t*y)) + sin(log(y))")
legend( 'topleft', legend = c("Euler N=100", "RK4 N = 100", "t= 3*PI"),
        col = c("black", 'red', "Black"),
        lty = c(1,1,NA),
        pch = c(NA, NA, 1))
          


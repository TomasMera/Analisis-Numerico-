#### ECUACIONES DIFERENCIALES ####
#TOMAS MERA 900071

#Ecuacion Diferencial

f <- function(t,y) {(cos(y) *  t^(4.98)) + (t * y^2)}

#Extremos y valor inicial
a=0
b= 4
alpha = 1.15

#Cantidad de Iteraciones
N = 32

#5.1 ----

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

Y_t_Euler <- Metodo_Euler(a,b,N,alpha)
Y_t_Euler


#5.2 ----
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

Y_t_RungeKutta <- RK4(a,b,N,alpha)
Y_t_RungeKutta

#5.3_ Grafico ----
df_Euler <-data.frame(Y_t_Euler)

df_RK <- data.frame(Y_t_RungeKutta)


#Grafico
plot(df_Euler$t , df_Euler$w,col ="black", type = "l", ylab = "y(t)", xlab = "t", xlim = c(a,b))
lines(df_RK$t , df_RK$w , col="red")
points(x = df_Euler$t, y = df_Euler$w, cex = 0.7)
points(x= df_RK$t, y = df_RK$w, cex= 0.7 )
title("Solucion del problema : y' = (cos(y) *  t^(4.98)) + (t * y^2)")
legend( 'bottomright', legend = c("Euler N=32", "RK4 N = 32"),
        col = c("black", 'red'),
        lty = c(1,1))

#Dan iguales

#5.4 ----
# Los métodos de Taylor de orden superior (como Runge-Kutta) tienen error de truncamiento de orden alto, lo que hace que aproximen mejor,
# pero es necesario conocer las derivadas. En cambio, el método de Euler, lo hace mas "económico" porque no es necesario conocer las derivadas
# pero esto lo hace menos exacto.




#### SIMULACION ####
#TOMAS MERA 900071
# a) ----
set.seed(900071)
P0<-52
mu<-0.16  #retorno esperado
sigma<-0.19  #volatilidad
T<-0.5  #seis meses (año seria 1)
dt<-1/252 #camino de precios DIARIO
n<-T/dt   #número de time-steps

m<- 986 #cantidad de caminos de precios (cantidad de simulaciones)
Pt<- matrix(NA,nrow=m,ncol=n+1) #matriz de camino de precios con m filas y n+1 precio (porque arranca dessde 0)
Pt[,1]<- P0 #para todas las filas, que en la primer columna tenga P0

for (i in 1:m) { #creo un bucle for que barre todas las simulaciones
  for (t in 2:(n+1)) {
    #el 2 porque arrance de la fila 2 porque 1 era P0 (precio inicial)
    Pt[i,t]<-Pt[i,t-1]*exp((mu-0.5*sigma^2)*dt+sigma*sqrt(dt)*rnorm(1))
  } 
}

View(Pt)

ggplot() +
  geom_histogram(aes(Pt[,ncol(Pt)]), binwidth = 2) +
  geom_vline(xintercept = mean(Pt[,ncol(Pt)]), colour = "darkblue") +
  xlab("Precio") + ylab("Frecuencia") +
  ggtitle("Histograma de precios finales")

Precio_esperado <- mean(Pt[,ncol(Pt)])
Precio_esperado

Desv_Estandar <- sd(Pt[,ncol(Pt)])
Desv_Estandar


#b)----
#Prob de que el Precio final Pt este entre 29 y P0

k = 0
for (i in 1:ncol(Pt)) {
  if ( Pt[i, ncol(Pt)] >= 29 &  Pt[i, ncol(Pt)]<=P0) {
    k = k + 1
  }
}
p = k / ncol(Pt)
print(paste('La probabilidad de que el precio final PT esté entre 29 y el precio inicial P0 es', p))

#c) ----
#Prob de que el precio final sea menor al precio esperado en T (E(Pt))

k = 0
for (i in 1:ncol(Pt)) {
  if ( Pt[i, ncol(Pt)] < Precio_esperado) {
    k = k + 1
  }
}
p = k / ncol(Pt)
print(paste('La probabilidad de que el precio final PT sea menor al precio esperado en T es', p))

##SIMULACION RETORNO DE UNA CARTERA PRECIOS INDEPENDIENTES

# 1) DATOS:
P0<-c(45,50,108)  #vector de los 3 precios iniciales
mu<-c(0.10,0.15,0.08) #rendimientos esperados para 3 activos
sigma<-c(0.20,0.25,0.30) #volatilidad para 3 activos
T<-0.5 #seis meses
m<-1000 #cantidad de camino de precios
Q<-c(100,150,120) #Cantidades de cada uno de los activos

# 2)Valuo mi cartera

#2.1:
z<-matrix(rnorm(3*m),nrow=m,ncol=3) #Matriz de numeros aleatorios con distribución N(0,1)
#2.2:
cor(z) #me devuelve la matriz de correlación para los precios simulados en z
#2.3:
P<-matrix(NA,m,3) #matriz de precios con 'm'filas (número de simulaciones)
#y 3 (activos) columnas 

for (i in 1:m) { #Bucle que cuenta el número de simulación
  for (k in 1:3) {  #Bucle que calcula el precio del activo 'k' en la simualción 'i'
    P[i,k]<-P0[k]*exp((mu[k]-0.5*sigma[k]^2)*T+sigma[k]*sqrt(T)*z[i,k]) #simulacion
    #k-esima y el activo i-esimo
  }
}#### cambiar el 3 en caso de cambiar la cantidad de activos

#2.4

V0<-Q*P0 #Valor de cada activo
VI<-sum(V0) #Valor inicial de la cartera
V0
VI

#2.5
P0.m<-matrix(rep(P0,m),m,3,byrow=T)  #creo una matriz con vector P0, repetido 'm' veces
P0.m

RL<-log(P/P0.m) #calculo rendimientos logarítmicos de los activos
RL
cor(RL)

# 3) Calculo el retorno de la cartera
Q.m<-matrix(rep(Q,m),nrow = m,ncol = 3,byrow = T)
# 4) Mult la matriz generada q.m por el vetor de precios aleatorios generado
VT<-Q.m*P   #Matriz de valores en el horizonte T de los activos- 
#SE LEE COMO:
# VT tiene en cada columna la cantidad de activos. en 1-1 esta la valuacion final 
#de ese activo 1, 4500 valia al momento final y luego valdran (el valor que este)
VF<-matrix(rowSums(VT),m,1) #sumo por filas, obtengo una matriz de valores finales
#la sumatoria de los VT para la fila 1, 2, .. i es el vf
# 4)  
#Rendimiento log de la cartera  
RL.V<-log(VF/VI) #Calculo rendimiento de la cartera
RL.V
#para cada simulacion, cuanto rendimiento tendré aprtriendo de mi valor
#original de la cartera 

#Grafico
par(mfrow= c(1,1))
tmp <- hist(RL.V, xlim = c(-0.5,1), ylim = c(0, 300), axes = F)
axis(side = 1, at = seq (-0.5, 1, by = 0.1))
axis(side = 2)

#Grafico de dispersion 
par(mfrow=c(3,1))
hist(P[,1])
hist(P[,2])
hist(P[,3])

par(mfrow=c(1,3))
plot(P[,1],P[,2])
plot(P[,2],P[,3])
plot(P[,3],P[,1])
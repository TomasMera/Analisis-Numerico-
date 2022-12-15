##SIMULACION RETORNO DE UNA CARTERA PRECIOS CORRELACIONADOS

# 1) DATOS:
P0<-c(45,50,108)  #vector de los 3 precios iniciales
mu<-c(0.10,0.15,0.08) #rendimientos esperados para 3 activos
sigma<-c(0.20,0.25,0.30) #volatilidad para 3 activos
T<-0.5 #seis meses
m<-1000 #cantidad de camino de precios
Q<-c(100,150,120) #Cantidades de cada uno de los activos

# 2.1Valuo mi cartera

V0<-Q*P0 #Valor de cada activo
VI<-sum(V0) #Valor inicial de la cartera
V0
VI
#2.2
z<-matrix(rnorm(3*m),nrow=m,ncol=3) #Matriz de numeros aleatorios con distribución N(0,1)
#2.3

#Genero la matríz Rho, la diag ppal tiene uno y fuera :
Rho<-diag(3)  #genera matriz identidad
Rho[1,2]<-Rho[2,1]<-0.90 #Corr entre el rendimineto del act 1 y 2 
Rho[1,3]<-Rho[3,1]<-0.70 # corr entre el rend del act 1 y 3
Rho[2,3]<-Rho[3,2]<-0.60 #corr entre el rend del activo 2 y 3

Rho


#2.4

CH<-chol(Rho) #calculo la factorización de choleky de la matriz de correlaciones
CH
t(CH)%*%CH  #como R brinda una matriz triangular SUPERIOR primero multiplcio la traspuesta para verificar

#generamos matriz "e" que tiene numeros epsilon correlacionados
e<-z%*%CH
cor(e)

#2.5
Pc<-matrix(NA,m,3) #Se crea matriz de precios correlacionados con "m" 
#(número de simulaciones) filas 
#y 3 (activos) columnas

for (i in 1:m) { #Bucle que cuenta el número de simulación
  for (k in 1:3) {  #Bucle que calcula el precio del activo 'k' en la simualción 'i'
    Pc[i,k]<-P0[k]*exp((mu[k]-0.5*sigma[k]^2)*T+sigma[k]*sqrt(T)*e[i,k])
  }
}

#2.6
P0.m<-matrix(rep(P0,m),m,3,byrow=T)  #creo una matriz con vector P0, repetido 'm' veces
RLc<-log(Pc/P0.m) #calculo rendimientos logarítmicos de los activos
cor(RLc) #tiene que ser igual a cor(e)
#2.7
cor(e)  #verifico
cor(RLc)-cor(e)

# 3) Calculo el retorno de la cartera
Q.m<-matrix(rep(Q,m),
            nrow = m,ncol = 3,
            byrow = T)
#repito m veces el vector q incial
Q.m

# 4) Mult la matriz generada q.m por el vetor de precios aleatorios generado
VTc<-Q.m*Pc   #Matriz de valores en el horizonte T de los activos

VFc<-matrix(rowSums(VTc),m,1)

#Rendimiento log de la cartera
RLc.V<-log(VFc/VI) #Calculo rendimiento de la cartera

RLc.V

#Grafico histograma
hist(RLc.V, xlim = c(-0.5, 1), ylim = c(0, 300), axes = F)
axis (side = 1, at = seq (-0.5, 1, by = 0.1))
axis(side = 2)
#los retornos estan mas dispersos 
#la cola es mas grande 


#grafico dispersion puntos
par(mfrow=c(1,3))
plot(Pc[,1],Pc[,2])
plot(Pc[,1],Pc[,3])
plot(Pc[,3],Pc[,2])
axis(side = 2)
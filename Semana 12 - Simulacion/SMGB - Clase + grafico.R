rm(list = ls())
graphics.off()

##################SIMULACION DE PRECIOS DE ACTIVOS FINANCIEROS CORRELACIONADOS O NO

####MODELO GEOMETRICO BROWNEANO

###SIMULACION CAMINO DE PRECIOS PARA 3 ACTIVOS (no hacemos camino, sino que vamos de 0 a T)

#COMPARACION ENTRE AMBOS METODOS 
#ACTIVOS INDEPENDIENTES ENTRE SI

#Parámetros
P0<-c(45,50,108)  #vector de los 3 precios iniciales
mu<-c(0.10,0.15,0.08) #rendimientos esperados para 3 activos
sigma<-c(0.20,0.25,0.30) #volatilidad para 3 activos
T<-0.5 #seis meses
m<-1000 #cantidad de camino de precios 

z<-matrix(rnorm(3*m),nrow=m,ncol=3) #Matriz de numeros aleatorios con distribución N(0,1)
cor(z) #me devuelve la matriz de correlación para los precios simulados en z
#como no estan correlacionados, la diagonal principal obviamente es está compuesa por 1
#y el resto por valores cercanos a 0 (por que existe error muestral al calcular numeros aleatorios)

#Simulación precios independientes (a partir de los precios de z)

P<-matrix(NA,m,3) #matriz de precios con 'm'filas (número de simulaciones)
#y 3 (activos) columnas


for (i in 1:m) { #Bucle que cuenta el número de simulación
  for (k in 1:3) {  #Bucle que calcula el precio del activo 'k' en la simualción 'i'
    P[i,k]<-P0[k]*exp((mu[k]-0.5*sigma[k]^2)*T+sigma[k]*sqrt(T)*z[i,k])
  }
}

P0.m<-matrix(rep(P0,m),m,3,byrow=T)  #creo una matriz con vector P0, repetido 'm' veces

RL<-log(P/P0.m) #calculo rendimientos logarítmicos de los activos


#ACTIVOS CORRELACIONADOS ENTRE SI

#Genero la matríz Rho

Rho<-diag(3)  #genera matriz identidad
Rho[1,2]<-Rho[2,1]<-0.90
Rho[1,3]<-Rho[3,1]<-0.70
Rho[2,3]<-Rho[3,2]<-0.60

Rho

#Calculo Cholesky

CH<-chol(Rho) #calculo la factorización de choleky de la matriz de correlaciones
CH

t(CH)%*%CH  #como R brinda una matriz triangular SUPERIOR primero multiplcio la traspuesta para verificar

#generamos matriz "e" que tiene numeros epsilon correlacionados

e<-z%*%CH #MATRIZ E de mil filas, por tres columnas pero con correclacion, sera la matriz de cholesku

cor(e) #correalción de los números aleatorios con distr N(0,1). Vemos que estan correlacionados
#la correlación de "e" debería ser muy parecida a Rho, por el error muestral no es exactamente igual

Pc<-matrix(NA,m,3) #Se crea matriz de precios correlacionados con "m" (número de simulaciones) filas 
#y 3 (activos) columnas

for (i in 1:m) { #Bucle que cuenta el número de simulación
  for (k in 1:3) {  #Bucle que calcula el precio del activo 'k' en la simualción 'i'
    Pc[i,k]<-P0[k]*exp((mu[k]-0.5*sigma[k]^2)*T+sigma[k]*sqrt(T)*e[i,k])
  }
}

P0.m <- matrix(rep(P0,m),m,3, byrow = T) #matriz del vector p0 repetido m veces
RLc<-log(Pc/P0.m) #calculo rendimientos logarítmicos de los activos

#Comparo correlaciones logarítmicas

cor(RL)
cor(RLc) #tiene que ser igual a cor(e)

#Gráficos
par(mfrow=c(2,3))
plot(P[,1],P[,2])
plot(P[,1],P[,3])
plot(P[,3],P[,2])
plot(Pc[,1],Pc[,2])
plot(Pc[,1],Pc[,3])
plot(Pc[,3],Pc[,2])


### COMPARACION DE RENDIMIENTOS

##SIMULACION RETORNO DE UNA CARTERA PRECIOS INDEPENDIENTES

Q<-c(100,150,120) #Cantidades de cada uno de los activos
V0<-Q*P0 #Valor de cada activo
VI<-sum(V0) #Valor inicial de la cartera
V0
VI

#repeto m veces el vector q0 mil veces
Q.m<-matrix(rep(Q,m),nrow = m,ncol = 3,byrow = T)

#cuanto es la valuacion final de cada uno de los activos
VT<-Q.m*P   #Matriz de valores en el horizonte T de los activos
VF<-matrix(rowSums(VT),m,1) #obtenemos una matriz de valores finales


RL.V<-log(VF/VI) #Calculo rendimiento de la cartera
hist(RL.V)



##SIMULACION RETORNO DE UNA CARTERA PRECIOS CORRELACIONADOS

Q<-c(100,150,120) #Cantidades de cada uno de los activos
V0<-Q*P0 #Valor de cada activo
VI<-sum(V0) #Valor inicial de la cartera
V0
VI

Q.m<-matrix(rep(Q,m),nrow = m,ncol = 3,byrow = T)

VTc<-Q.m*Pc   #Matriz de valores en el horizonte T de los activos
VFc<-matrix(rowSums(VTc),m,1)


RLc.V<-log(VFc/VI) #Calculo rendimiento de la cartera


#Gráficos (se puede ver la dispersión)

par(mfrow=c(2,1))
tmp<-hist(RL.V,xlim = c(-0.5,1), ylim = c(0,300), axes = F)
axis(side = 1, at=seq(-0.5,1,by=0.1))
axis(side = 2)
tmp<-hist(RLc.V,xlim = c(-0.5,1), ylim = c(0,300), axes = F)
axis(side = 1, at=seq(-0.5,1,by=0.1))
axis(side = 2)
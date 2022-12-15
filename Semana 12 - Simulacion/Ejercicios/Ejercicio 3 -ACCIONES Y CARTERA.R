#EJERCICIO 3

#Para el precio de las acciones utilizo la libreria "quantmod"
#install.packages("quantmod")
#library(quantmod)

#Traigo el DataFrame con los precios de YPF y elijo el AJUSTADO al 6-11-2020
getSymbols("YPFD.BA",auto.assign = TRUE, src = "yahoo")

PrecioYpf = 555.40

#Traigo el DataFrame con los precios de MELI y elijo el AJUSTADO al 6-11-2020
getSymbols("MELI.BA",auto.assign = TRUE, src = "yahoo")

PrecioMELI = 3704.0
##### A Y B ####
#PARAMETROS
P0 = c(PrecioYpf,PrecioMELI)
mu = c(0.15,0.12)
sigma = c(0.20,0.19)
T<- 1 #1 año
n<- 10000 #Simulo 10000 precios de cada activo

#Calculo las e

Z <- matrix(rnorm(2*n), nrow = n, ncol = 2)
cor(Z)

#SIMULO LOS PRECIOS
P<- matrix(NA,n,2)

for (i in 1:n) {
  for (k in 1:2) {
    P[i,k] <- P0[k] * exp((mu[k] - 0.5 * sigma[k]^2)*T + sigma[k] * sqrt(T) * Z[i,k])
  }
}


#METRICAS
#EstadisticasYPF
PrecioEsperadoYPF <- mean(P[,1])
LSYPF <- quantile(P[,1],0.975)
LIYPF<-quantile(P[,1],0.025)

#Estadisticas MELI
PrecioEsperadoMELI <- mean(PC[,2])
LSMELI <- quantile(P[,2],0.975)
LIMELI<-quantile(P[,2],0.025)


#Rendimientos logaritmicos
P0.m <- matrix(rep(P0,n),n,2,byrow = TRUE)
RL <- log(P / P0.m)

mean(RL[,1])
mean(RL[,2])  #Rendimientos esperados de cada activo


#### C, D, E ####
#Nueva accion
getSymbols("LOMA.BA",auto.assign=TRUE,src="yahoo")
PrecioLOMA = 139.0500
uLOMA = 0.3
sigmaLOMA = 0.42

#Parametros
P0 <- c(555.40,3704,139.0500)
mu <- c(0.15,0.12,0.3)
sigma <- c(0.2,0.19,0.42)
T = 1
n=10000

Rho <-diag(3)
Rho[1,2]<-Rho[2,1]<-0.90 #Corr entre el rendimineto del act 1 y 2 
Rho[1,3]<-Rho[3,1]<-0.70 # corr entre el rend del act 1 y 3
Rho[2,3]<-Rho[3,2]<-0.60 #corr entre el rend del activo 2 y 3

#Matriz de Cholesku
CH <- chol(Rho)

Z<- matrix(rnorm(3*n),nrow = n, ncol=3)

e<-Z%*%CH

PC <- matrix(NA, n , 3)

for (i in 1:n) {
  for(k in 1:3){
    PC[i,k] <- P0[k] *exp((mu[k] - 0.5* sigma[k]^2)*T +sigma[k] * sqrt(T) * e[i,k]) 
  }
}
#EstadisticasYPF
PrecioEsperadoYPF <- mean(PC[,1])
LSYPF <- quantile(PC[,1],0.975)
LIYPF<-quantile(PC[,1],0.025)

#Estadisticas MELI
PrecioEsperadoMELI <- mean(PC[,2])
LSMELI <- quantile(PC[,2],0.975)
LIMELI<-quantile(PC[,2],0.025)

#Estadisticas LOMA
PrecioEsperadoLOMA <- mean(PC[,3])
LSLOMA <- quantile(PC[,3],0.975)
LILOMA<-quantile(PC[,3],0.025)


#Rendimientos logaritmicos
P0.m <- matrix(rep(P0,n),n,3,byrow = TRUE)
RLc <- log(PC / P0.m)

mean(RLc[,1])
mean(RLc[,2])  #Rendimientos esperados de cada activo
mean(RLc[,3])


#GRAFICOS
par(mfrow=c(2,3))
plot(PC[,1],PC[,2])
plot(PC[,1],PC[,3])
plot(PC[,3],PC[,2])

plot(P[,1],P[,2])

par(mfrow=c(1,3))
hist(RLc[,1])
hist(RLc[,2])
hist(RLc[,3])


#### F ####
P0 = c(555.40,3704)
mu = c(0.15,0.12)
sigma = c(0.20,0.19)
T<- 1 #1 año
n<- 10000

Q <- c(200,120)
V0 <- Q * P0
Vi <- sum(V0)

#Valor de activos y valor total de la cartera
Q.n <- matrix(rep(Q,n), nrow = n, ncol = 2, byrow = T)
VT <- Q.n * P
VF <- matrix(rowSums(VT),n,1)
mean(VF) #Valor Esperado de la Cartera

mean(VT[,1]) #Valor esperado de la Cartera con las acciones de YPF
mean(VT[,2]) #Valor esperado de la Cartera con las acciones de MELI

#Rendimiento Esperado
RL.V <- log(VF/Vi)
mean(RL.V)


#Con acciones correlacionadas
VTc <- Q.n * PC[,1:2] #Solo tomo las primeras dos columnas porque mi cartera no tiene acciones de LOMA
VFc <- matrix(rowSums(VTc), n ,1)
mean(VFc)   #Valor Esperado de la Cartera

mean(VTc[,1]) #Valor esperado de la Cartera con las acciones de YPF
mean(VTc[,2]) #Valor esperado de la Cartera con las acciones de MELI


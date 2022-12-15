#EJERCICIO 2

#Para hacerlo para una muestra de 10000
M <- 10000

resultado <- matrix(NA , nrow = M, ncol = 2)

for (m in 1:M) {
  #numero de siniestros se distribuye Binomial n=1200, p = 0.7984
  N <- rbinom(1,1200,0.7984)
  resultado[m,1] <- N
  #Monto de los siniestros se distribuye Chi-Cuadrado con 2 grados de libertad
  xi <- rchisq(n = N, df = 2)
  S <- sum(xi)
  resultado[m,2] <- S
}

N.E <- mean(resultado[,1]) #Esperanza del numero de siniestros
N.Var <- var(resultado[,1])#Varianza del numero de siniestros

ES <- mean(resultado[,2])#Esperanza de S
Des <- sd(resultado[,2])#Desvio Estandar de S

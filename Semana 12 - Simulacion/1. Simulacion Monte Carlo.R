## METODO DE MONTE CARLO

#EJEMPLO 1
#Funcion que se desea integrar

f1 <- function(x){x^2}

#Tamaño de la muestra
n <- 100

#Generacion de numeros uniformes
U <- runif(n)

#Estimacion de la integral
Integral.MC <- 1/n*sum(f1(U))

#Me calcula la integral de acuerdo al método de Montecarlo que será la suma de todos 
#los valores de la función en cada uno de los valores U que simulé.
#Luego lo multiplica por 1/n para calcular el promedio que es mi estimación del valor esperado

#Estimacion del sigma evaluado (f)
sf.MC <- sqrt(1/(n-1))*sum((f1(U)-Integral.MC)^2)
  

Error.MC <- sf.MC/sqrt(n)

Integral.MC #0.3356721
Error.MC #0.1025405


##EJEMPLO 2 - La intergral no está entre 0 y 1

#Error en la integral MC
f1 <- function(x){x^2}
n <- 10000
a <- 10
b <- 15
U <- a+(b-a)*runif(n)
U

# Valor esperado E[f(U)] da la altura media.
Altura.Promedio <- 1/n * sum(f1(U))
Altura.Promedio
Ancho.Base <- b-a
Ancho.Base

Integral.MC.BA <- Altura.Promedio*Ancho.Base
Integral.MC.BA


#Calculo el error estimado
sf.MC.BA <- sqrt(1/(n-1))*sum((f1(U)*(b-a)-Integral.MC)^2)
Error.MC.BA <- sf.MC/sqrt(n)
Error.MC.BA 


## EJEMPLO 3 NORMAL

f2 <- function(x){1/ sqrt((2*pi*o^2)) * exp(-(x-mu)^2/(2*o))}
mu = 5
o = 10 

#Calculo la integral por Monte Carlo
n = 100
a = 10
b = 20
U <- a+(b-a)*runif(n)


Altura.Promedio <- 1/n * sum(f2(U))
Altura.Promedio
Ancho.Base <- b-a
Ancho.Base

Integral.MC.BA <- Altura.Promedio*Ancho.Base
Integral.MC.BA

#error estimado
sf.MC.BA <- sqrt(1/(n-1)*sum((f2(U)*(b-a)-Integral.MC.BA)^2))
Error.MC.BA <- sf.MC.BA/sqrt(n)
Error.MC.BA 

#SOlucion analitica con R
pnorm(20,5,10)-pnorm(10,5,10)

INT.INF <- Integral.MC.BA+2*Error.MC.BA
INT.SUP <- Integral.MC.BA-2*Error.MC.BA
INT.INF
INT.SUP
INT.INF-INT.SUP



# La simulacion de Monte Carlo tiene su origen en la estadistica probabilistica
# la definicion clasica de Probabilidad me dice que si yo tengo un experimento aleatorio, 
#si el mismo lo repite una cantidad suficientemente alta, la frecuencia relativa tiende
# a estabilizarse. Converge en particular a la probabilidad en sentido clasico.
# COn la simulacion de Monte Carlo uno logra generar una muestra simulada a partir de la 
#cual va a realizarse analisis estadistico.


# esto tiene sentido cuando no se conoce como se distribuye la VA  entonces 
#simula la muestra y a partir de esta hace als inferencias estadisticas.


#Generacion de numeros aleatorios no uniformes
#Probability Integral Transform: permite transformar cualquier variable aleatoria
#en una uniforme y viceversa.
# Si x tiene funcion de densidad f y funcion de distribucion F 
# F(x) int de menos infinito a x = f(t) dt
#Si llamamos U=F(X) entonces U es una variable con distribucion uniforme
#en el int cero uno 


#ejemplo diapo donde tenemos una sumatoria de siniestro que se distribuyen con
# distribucion gamma pero esa sumatoria es una distribucion de Poisson

#La distribucion de Poisson con lanmda = 50
N <- rpois(n=1, lambda=50)
#Genera N numeros aleatorios con distribucion Gamma
#Si shape = a y scale = s, entonces al densidad es:
# f(x) = 1 / (s^a Gamma (a)) x^(a-1) e^-(x/s)
Xi <- rgamma(n = N, shape = 10, scale = 5)
S <- sum(Xi)

#######################################################################
#Ejemplo repitiendo el experimento varias veces

#Ingresamos el tamaño de la muestra 
M <- 10000

#Creo la matriz donde se almacenará la salida
Resultado <- matrix(NA, nrow = M, ncol = 2)
for (m in 1:M){
  #Genera un numero aleatorio con distribucion
  #de poisson lambda = 50
  N <- rpois(n=1, lambda = 50)
  Resultado[m,1] <- N
  #Genera N numeros aleatorios con dist gamma
  ##Si shape = a y scale = s, entonces al densidad es:
  # f(x) = 1 / (s^a Gamma (a)) x^(a-1) e^-(x/s)
  Xi <- rgamma(n = N, shape = 10, scale = 5)
  S <- sum(Xi)
  Resultado[m,2] <- S
}

#la varianza y esperanza de una poisson coincide con la media
N.E <- mean(Resultado[,1])
N.Var <- var(Resultado[,1])

ES <- mean(Resultado[,2])
S.de<- sd(Resultado[,2])

hist(Resultado[,2])
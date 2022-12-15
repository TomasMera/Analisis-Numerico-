#### INTEGRACION NUMERICA ####
#a) ----
#gamma(9.22)
gamma <- function(t){
  val <- t^(x - 1) * exp(-t)
  return(val)
}

n = 100

simpson.compuesto = function(a, b, n, fn){
  if((n %% 2) == 0){
    #Paso 1
    h = (b-a)/n
    
    #Paso 2
    xio = fn(a) + fn(b)
    xi1 = 0 #Suma impar
    xi2 = 0 #Suma par
    
    #Paso 3
    for (i in 1:(n-1)) {
      #Paso 4
      x = a + i*h
      #Paso 5 
      if((i %% 2) == 0){
        xi2 = xi2 + fn(x)
      }else{
        xi1 = xi1 + fn(x)
      }
    }
    #Paso 6
    val = h*(xio + 2*xi2 + 4*xi1)/3
    return(val)
  }
  else return("n debe ser par")
}

x = 9.22
Gamma_9.22 <- simpson.compuesto(0,1000, n, gamma) # de 0 a 1000 aprox de 0 a infinito
Gamma_9.22


#b) ----
trapecio.compuesto = function(a, b, n, fn){
h = (b-a)/n

xio = fn(a) + fn(b)
xi = 0
for (i in 1:(n-1)) {
  x = a + i*h
  xi = xi + fn(x)
}
val = h*(xio + 2*xi)/2
return(val)
}

alpha = 9.22
beta  = 2
n = 50

x = alpha
Gamma_alpha = trapecio.compuesto(0,1000,1000, gamma)

x = beta
Gamma_beta = trapecio.compuesto(0,1000,1000, gamma)

x = alpha + beta
Gamma_alpha_beta = trapecio.compuesto(0,1000,1000, gamma)

fn <- function(x){
  val <- (Gamma_alpha_beta/(Gamma_alpha * Gamma_beta)) * x ^(alpha-1) *(1-x)^(beta-1)
  return(val)
}

# Prob de que Y<0.6, INTEGRO ENTRE 0 Y 0.6

Y_0.6 <- trapecio.compuesto(0,0.6,n,fn)
Y_0.6

#ERROR EN LA APROXIMACION
alim<-0  #cargar límite inferior intervalo
blim<-0.6  #cargar límite superior intervalo

d1tc<-D(expression((Gamma_alpha_beta/(Gamma_alpha * Gamma_beta)) * x ^(alpha-1) *(1-x)^(beta-1)),"x") #cargar función que desea derivar
d2tc<-D(d1tc,"x")

d2tc

derivadatc2<-function(x){
  f<-(Gamma_alpha_beta/(Gamma_alpha * Gamma_beta)) * (x^(((alpha - 
                                                             1) - 1) - 1) * ((alpha - 1) - 1) * (alpha - 1)) * (1 - x)^(beta - 
                                                                                                                          1) - (Gamma_alpha_beta/(Gamma_alpha * Gamma_beta)) * (x^((alpha - 
                                                                                                                                                                                      1) - 1) * (alpha - 1)) * ((1 - x)^((beta - 1) - 1) * (beta - 
                                                                                                                                                                                                                                              1)) - ((Gamma_alpha_beta/(Gamma_alpha * Gamma_beta)) * (x^((alpha - 
                                                                                                                                                                                                                                                                                                            1) - 1) * (alpha - 1)) * ((1 - x)^((beta - 1) - 1) * (beta - 
                                                                                                                                                                                                                                                                                                                                                                    1)) - (Gamma_alpha_beta/(Gamma_alpha * Gamma_beta)) * x^(alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                               1) * ((1 - x)^(((beta - 1) - 1) - 1) * ((beta - 1) - 1) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                       (beta - 1)))
  return(f)
}

optimotc<-optimize(derivadatc2,c(alim,blim),maximum = T)
opttc<-abs(optimotc$objective)
opttc

error<-((blim-alim)/12)*(((blim-alim)/n)^2)*opttc
error

#OTRA OPCION
x = seq(0, 0.6, length.out = 1000)
fx = eval(D(D(expression( (Gamma_alpha_beta/(Gamma_alpha*Gamma_beta) * x^(alpha - 1) * (1-x)^(beta - 1)) ), "x"),"x"), list(x = x))

limiteSuperior = 0.6
limiteInferior = 0
n = 50

h = (limiteSuperior - limiteInferior)/n

error = ((limiteSuperior - limiteInferior)/12) * (h^2) * max(abs(fx))
error


#c) ----
g =  function(x){ ifelse(x>0.5, x-0.5, 0)}
fx = function(x){ g(x) * (Gamma_alpha_beta/(Gamma_alpha*Gamma_beta) * x^(alpha - 1) * (1-x)^(beta - 1))}
simpson.compuesto(0, 1, 1000, fx)



#### SIMULACION ####
# a) ----
P0<-75
mu<-0.15  #retorno esperado
sigma<-0.20  #volatilidad
T<-0.5  #seis meses (año seria 1)
dt<-1/252 #camino de precios DIARIO
n<-T/dt   #número de time-steps

m<- 1000 #cantidad de caminos de precios (cantidad de simulaciones)
Pt<- matrix(NA,nrow=m,ncol=n+1) #matriz de camino de precios con m filas y n+1 precio (porque arranca dessde 0)
Pt[,1]<- P0 #para todas las filas, que en la primer columna tenga P0

for (i in 1:m) { #creo un bucle for que barre todas las simulaciones
  for (t in 2:(n+1)) {
    #el 2 porque arrance de la fila 2 porque 1 era P0 (precio inicial)
    Pt[i,t]<-Pt[i,t-1]*exp((mu-0.5*sigma^2)*dt+sigma*sqrt(dt)*rnorm(1))
  } 
}

View(Pt)

hist(Pt[,ncol(Pt)], main = "Histograma de los precios finales", xlab = "Pt")

#b)----
#Prob de que el Precio final Pt este entre 65 y 75
ncol(Pt) #127

k = 0
for (i in 1:ncol(Pt)) {
  if ( Pt[i, ncol(Pt)] >= 65 &  Pt[i, ncol(Pt)]<=P0) {
    k = k + 1
  }
}
p = k / ncol(Pt)
print(paste('La probabilidad de que el precio final PT esté entre 65 y el precio inicial P0 es', p))

#c) ----
#Prob de que el precio final sea menor al precio esperado en T (E(Pt))
E_PT <- mean(Pt[,ncol(Pt)])
E_PT

k = 0
for (i in 1:ncol(Pt)) {
  if ( Pt[i, ncol(Pt)] < E_PT) {
    k = k + 1
  }
}
p = k / ncol(Pt)
print(paste('La probabilidad de que el precio final PT sea menor al precio esperado en T es', p))



### INTERPOLACION ####
### a)----

#Aunque existe un único polinomio interpolante para un conjunto de puntos, 
# hay varias formas de calcularlo. La ventaja del método de Newton sobre 
# el polinomio de Lagrange es que es mas sencillo agregar un nuevo conjunto
# de puntos al polinomio, mientras que para agregar un nuevo par con el
# Polinomio de Lagrange se debería empezar de cero.


### b) ----
L <- c(9.4812,9.5244,10.0952,11.1160,12.8725,13.4483,14.1008,14.4795,15.2031,19.6600)
PA <- seq(0.1,1,0.1)

Lagrange<-function(x,x_dado,f_dado){ ###
  Pol<-0
  n<-length(x_dado)
  Aux<-c(rep(1,n))
  for (k in 1:n){
    for(i in 1:n){
      if(k != i){
        Aux[k]<-Aux[k]*((x-x_dado[i])/(x_dado[k]-x_dado[i]))
      }
    }
    
    Pol<- Pol+f_dado[k]*Aux[k]
  }
  return(Pol)
}

P_L14 <- Lagrange(14,L,PA)
P_L14
#El valor estimado para P(L<14) esta dentro de los rangos esperados ya que 
#Según la tabla P(L< 13.4483) = 0.6 y P(L< 14.1008) = 0.7, y 
#0.6818837 se encuentra entre esos valores

P_L18 <- Lagrange(18,L,PA)
P_L18

# El valor estimado para P(L< 18) es menor que cero, algo imposible en 
#una función de densidad. Esto se debe a que 18 es un extremo de la función 
#y los polinomios de Lagrange no son buenos para estimar extremos. 

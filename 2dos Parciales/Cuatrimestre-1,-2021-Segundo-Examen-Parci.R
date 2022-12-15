## 2do Parcial, 1er Cuatri 2021

##### Ejercicio 1 ####
#1.1 ----
# Función a integrar.
fn = function(x){
  val = 1/(x*sigma * sqrt( 2 * pi)) * exp(-(log(x) - u)^2 / (2*sigma^2))
  return(val)
}

sigma = 0.22 * sqrt(0.75)
u = 2.77


int_newton.cotes = function(a, b, n){
  h = (b - a)/n
  
  if(n == 1){
    xo = a
    x1 = b
    
    result = (h/2)*(fn(xo) + fn(x1))
    return(result)
  }
  else if(n == 2){
    xo = a
    x2 = b
    
    x1 = (xo + x2)/2
    #x1a = a + (b -a)/2
    
    result = (h/3)*(fn(xo) + 4*fn(x1) + fn(x2))
    return(result)
  }
  else if(n == 3){# Regla de 3 octavos de Simpson
    xo = a
    x3 = b
    
    x1 = xo + h
    x2 = xo + 2*h
    
    result = ((3/8)*h)*(fn(xo) + 3*fn(x1) + 3*fn(x2) + fn(x3))
    return(result)
  }
  else print("Grado del polinomio no programado")
}

## Con regla del Trapecio
int_newton.cotes(a = 12.68 , b = 20.42 , n=1)
print("Nodos utilizados en la regla del trapecio: x0, x1")

#Con la regla de Simpson
int_newton.cotes(a = 12.68 , b = 20.42 , n=2)
print("Nodos utilizados en la regla de Simpson: x0, x1, x2")

#Con la regla de Tres Octavos de Simpson
int_newton.cotes(a = 12.68 , b = 20.42 , n=3)
print("Nodos utilizados en la regla de Tres Octavos de Simpson: x0, x1, x2 ,x3")


#Grafico
x <- seq(from= 0.1, to = 50 , by=0.1)

plot(x, fn(x), type="l")



#1.2 ----
simpson.compuesto = function(a, b, n){
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

# Aproximando la Integral con n = 1000
simpson.compuesto(12.68 , 20.42 , 1000)

#Cota del ERROR

alim<-12.68   #cargar límite inferior intervalo
blim<-20.42  #cargar límite superior intervalo

X <- seq(from = alim, to = blim , by = 0.001)
fx <- eval(D(D(D(D(expression( 1/(x*sigma * sqrt( 2 * pi)) * exp(-(log(x) - u)^2 / (2*sigma^2))), "x"),"x"), "x"), "x"))

n <- 1000
h <- (blim - alim) / n

ERROR <- ((blim - alim)/180) * (h^4) * max(abs(fx))
ERROR




#1.3 ----
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

#Esperanza matematica de una funcion de densidad es la integral en el dominio de x * f(x)
E_s = trapecio.compuesto(12.68, 20.42 , n = 1000 , fn = expression(x* (1/(x*sigma * sqrt( 2 * pi)) * exp(-(log(x) - u)^2 / (2*sigma^2)))) )
E_s

#COTA DEL ERROR

alim<-12.68   #cargar límite inferior intervalo
blim<-20.42  #cargar límite superior intervalo

d1tc<-D(expression(x* (1/(x*sigma * sqrt( 2 * pi)) * exp(-(log(x) - u)^2 / (2*sigma^2)))),"x") #cargar función que desea derivar
d2tc<-D(d1tc,"x")

d2tc

derivadatc2<-function(x){
  f<--(1/(x * sigma * sqrt(2 * pi)) * (exp(-(log(x) - u)^2/(2 * sigma^2)) * 
                                         (2 * (1/x * (log(x) - u))/(2 * sigma^2))) + sigma * sqrt(2 * 
                                                                                                    pi)/(x * sigma * sqrt(2 * pi))^2 * exp(-(log(x) - u)^2/(2 * 
                                                                                                                                                              sigma^2)) + ((1/(x * sigma * sqrt(2 * pi)) * (exp(-(log(x) - 
                                                                                                                                                                                                                    u)^2/(2 * sigma^2)) * (2 * (1/x * (log(x) - u))/(2 * sigma^2))) + 
                                                                                                                                                                              sigma * sqrt(2 * pi)/(x * sigma * sqrt(2 * pi))^2 * exp(-(log(x) - 
                                                                                                                                                                                                                                          u)^2/(2 * sigma^2))) + x * (1/(x * sigma * sqrt(2 * pi)) * 
                                                                                                                                                                                                                                                                        (exp(-(log(x) - u)^2/(2 * sigma^2)) * (2 * (1/x * (1/x) - 
                                                                                                                                                                                                                                                                                                                      1/x^2 * (log(x) - u))/(2 * sigma^2)) - exp(-(log(x) - 
                                                                                                                                                                                                                                                                                                                                                                     u)^2/(2 * sigma^2)) * (2 * (1/x * (log(x) - u))/(2 * 
                                                                                                                                                                                                                                                                                                                                                                                                                        sigma^2)) * (2 * (1/x * (log(x) - u))/(2 * sigma^2))) - 
                                                                                                                                                                                                                                                                        sigma * sqrt(2 * pi)/(x * sigma * sqrt(2 * pi))^2 * (exp(-(log(x) - 
                                                                                                                                                                                                                                                                                                                                     u)^2/(2 * sigma^2)) * (2 * (1/x * (log(x) - u))/(2 * 
                                                                                                                                                                                                                                                                                                                                                                                        sigma^2))) - (sigma * sqrt(2 * pi)/(x * sigma * sqrt(2 * 
                                                                                                                                                                                                                                                                                                                                                                                                                                               pi))^2 * (exp(-(log(x) - u)^2/(2 * sigma^2)) * (2 * (1/x * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (log(x) - u))/(2 * sigma^2))) + sigma * sqrt(2 * pi) * (2 * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (sigma * sqrt(2 * pi) * (x * sigma * sqrt(2 * pi))))/((x * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         sigma * sqrt(2 * pi))^2)^2 * exp(-(log(x) - u)^2/(2 * sigma^2))))))
  return(f)
}

optimotc<-optimize(derivadatc2,c(alim,blim),maximum = T)
opttc<-abs(optimotc$objective)
opttc

#Error sin n deseado
n<-1000
error<-((blim-alim)/12)*(((blim-alim)/n)^2)*opttc
error


#1.4 ----
#Funcion condicional

U <- function(x){
  if(x>40){
    return(x-40)
  } else {
    return(0)
  }
}

#Funcion de esperanza condicional a integrar
fn = function(x){
  val = U(x) * (1/(x*sigma * sqrt( 2 * pi)) * exp(-(log(x) - u)^2 / (2*sigma^2)))
  return(val)
}

Esp_Condicional <- simpson.compuesto(10^-10, 1000, 1000)
Esp_Condicional


#### Ejercicio 2 ####
#2.1 ----
P0 = 300
T = 0.75
mu = 0.1
sigma = 0.18

N <- 15000 #Cantidad de simulaciones
e <- rnorm(N)

PT <- matrix(NA, nrow = N, ncol=1)

PT[,1] <- P0 * exp((mu-0.5*sigma^2) * T + sigma * sqrt(T) * e)  #Matriz con los precios finales simulados

PT

#Precio esperado
mean(PT)

#Desvio Estandar
sd(PT)

#Histograma de Precios
ggplot()+
  geom_histogram(aes(PT[,1])) + 
  geom_vline(xintercept = mean(PT), colour = "darkblue") +
  xlab("Precio") + ylab("Frecuencia")

#2.2 ----
P0 = 300
mu = 0.1
sigma = 0.18

set.seed(895096)


T = 0.75
dt = 1/250
n = T /dt #Numero de Time-steps

m = 2000 #Cantidad de Caminos de Precios

P0aT<- matrix(NA,nrow=m,ncol=n+1) #matriz de camino de precios con m filas y n+1 precios (porque arranca desde 0)
P0aT[,1]<- P0 #Precio inicial es igual en todas

for (i in 1:m) {
  for (t in 2:(n+1)) {
    P0aT[i,t]<-P0aT[i,t-1]*exp((mu-0.5*sigma^2)*dt+sigma*sqrt(dt)*rnorm(1))
  }
}

P0aT

ggplot() +
  geom_histogram(aes(P0aT[,ncol(P0aT)]), binwidth = 5) +
  geom_vline(xintercept = mean(P0aT[,ncol(P0aT)]), colour = "darkblue") +
  xlab("Price") + ylab("Times")

#Valor Esperado y Desvio Estandar
mean(P0aT[,ncol(P0aT)])
sd(P0aT[,ncol(P0aT)])

#2.3 ----
#Grafico del Camino de Precios

t<-rep(0:n,m)   #crea un vector de '0' a 'n' y lo repito 'm' veces
t<-matrix(t,nrow=m,ncol=n+1,byrow = T) #Ordena el vector anterior en una matriz
#primer fila y barro todas las columnas del vector tiempo, y 
#luego primer file ay barro col del ector precios
plot(t[1,],P0aT[1,],type="l",ylim=c(min(P0aT),max(P0aT)), xlab = "t", ylab = "Precios") #grafica el primer camino
for (i in 2:m) {
  lines(t[i,],P0aT[i,],col=trunc(runif(1)*m))
}

#Vector de Promedios de cada uno de los momentos 
M<-matrix(NA,nrow=1,ncol = n+1)
for (i in 1:(n+1)) {
  M[i]<-mean(P0aT[,i]) #la media por cada una de las columnas
}

#Vector de Percentiles
prob<-0.99   #estimación de intervalos de confianza de los precios para cada momento al 90% (porque deja 5% arriba y 5% abajo)

#limite superior que acumula ese valor
LS<-matrix(NA,nrow = 1,ncol=n+1)
for (i in 1:(n+1)) {
  LS[i]<-quantile(P0aT[,i],prob) #la funcion quantil de R para calc percentiles
}
#limite inferior que acumula ese valor
LI<-matrix(NA,nrow = 1,ncol=n+1)
for (i in 1:(n+1)) {
  LI[i]<-quantile(P0aT[,i],1-prob)
}

#Agrego al grafico, con lines, la media y perc.

#Grafico del limite superior
lines(t[1,],LS, col='blue',lwd=5) #line width(lwd)
#Grafico del limite inferior
lines(t[1,],LI, col='blue',lwd=5) #line width(lwd)
#grafico de la media
lines(t[1,],M, col='black',lwd=5)#line width(lwd)

legend("topleft", legend = c("Limite Superior", "Valor Esperado", "Limite Inferior"),
       col = c("blue", "Black", "blue"),
       lty = c(1,1,1))

#2.4 ----
df_pt <- data.frame(PT)
df_p0at <- data.frame(P0aT)

#Probabilidad de que el precio final sea menor a 268.95

length(df_pt[df_pt <268.95]) / length(PT[,1]) # 0.1349333

length(df_p0at[df_p0at$X188 < 268.95, "X188"]) / length(P0aT[,ncol(P0aT)]) # 0.1295

#Probabilidad de que el precio final sea mayor a 364.79

length(df_pt[df_pt > 364.79]) / length(PT[,1]) # 0.1972667

length(df_p0at[df_p0at$X188 > 364.79, "X188" ]) / length(P0aT[,ncol(P0aT)]) #0.1835


#### Ejercicio 3 ####
#3.1----

alpha = 3
beta = 1

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


gamma = function(t){
  val = t^(x-1) * exp(-t)
  return(val)
}

x=alpha
Gamma_alpha = simpson.compuesto(0,1000,1000,gamma)

x=beta
Gamma_beta = simpson.compuesto(0,1000,1000,gamma)

x=alpha + beta 
Gamma_alpha_beta = simpson.compuesto(0,1000,1000,gamma)

#Cargo la funcion de la Esperanza de Y
fn <- function(x){
  return(x * (Gamma_alpha_beta / (Gamma_alpha * Gamma_beta)) * x^(alpha-1) * (1-x)^(beta -1))
}

E_Y <- simpson.compuesto(0,1,100,fn)
E_Y


#3.2 ----
#Derivada respecto a alfa
alpha = 3 + 10^-10 #Variacion infinitesimal
beta = 1

gamma = function(t){
  val = t^(x-1) * exp(-t)
  return(val)
}

x=alpha
Gamma_alpha = simpson.compuesto(0,1000,1000,gamma)

x=beta
Gamma_beta = simpson.compuesto(0,1000,1000,gamma)

x=alpha + beta 
Gamma_alpha_beta = simpson.compuesto(0,1000,1000,gamma)

fn <- function(x){
  return(x * (Gamma_alpha_beta / (Gamma_alpha * Gamma_beta)) * x^(alpha-1) * (1-x)^(beta -1))
}

E_Y_h <- simpson.compuesto(0,1,100,fn)
E_Y_h

Derivada_respecto_a_alpha = (E_Y_h - E_Y) / (10^-10)
Derivada_respecto_a_alpha

#3.2 ----
#Derivada respecto a B
alpha = 3 
beta = 1 + 10^-10 #Variacion infinitesimal

gamma = function(t){
  val = t^(x-1) * exp(-t)
  return(val)
}

x=alpha
Gamma_alpha = simpson.compuesto(0,1000,1000,gamma)

x=beta
Gamma_beta = simpson.compuesto(0,1000,1000,gamma)
Gamma_beta

x=alpha + beta 
Gamma_alpha_beta = simpson.compuesto(0,1000,1000,gamma)

fn <- function(x){
  return(x * (Gamma_alpha_beta / (Gamma_alpha * Gamma_beta)) * x^(alpha-1) * (1-x)^(beta -1))
}

E_Y_h <- simpson.compuesto(0,1,100,fn)
E_Y_h

Derivada_respecto_a_Beta = (E_Y_h - E_Y) / (10^-10)
Derivada_respecto_a_Beta

print("Gamma_beta no puede ser <1 porque si no la funcion no converge a ningun numero")

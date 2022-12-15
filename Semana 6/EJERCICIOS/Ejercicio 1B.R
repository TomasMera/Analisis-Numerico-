
# Resolución numérica de sistemas no lineales.
# Alg. Newton - Matriz 3x3

# 1) Cargar algoritmo de Norma ----
norma <- function(y, metodo){
  if (metodo==2){
    return(sqrt(sum(y^2)))
  }
  if (metodo==Inf){
    return(max(abs(y)))
  }
  return("El metodo debe ser 2 o Inf")
}
# 2) Carga Algoritmo de SENOL ----
Sist_Ec_NoLineal_Newton <- function(n,x,TOL,N){
  #Paso 1
  k <- 1
  #Paso 2
  while(k<=N){
    #Paso 3
    fx <- Fx(x)
    J <- Jacobiano(x[1],x[2],x[3])
    #Paso 4
    y = solve(J)%*%-fx
    #Paso 5
    x <- x + t(y)
    #Paso 6
    if (norma(y,2) < TOL){
      return(x)
    }
    #Paso 7
    k <- k+1
  }
  #Paso 8
  return(paste('Numero max de iteraciones excedido'))
}
# 3) Calculo de la derivada ----
f<-expression(20*x3 + (10*pi-3/3)+exp(-x1*x2))
fprima<-D(f,"x3")
fprima

# 4) Defino fn y dfn ----

#         i)Funcion 1 y sus derivadas ----
f1 <- function(x1,x2,x3){
  return(3*x1 - cos(x2*x3)-1/2)
}

#Derivada primera de f1
df11 <- function(x1,x2,x3){
  return(3)
}
#Derivada segunda de f1
df12 <- function(x1,x2,x3){
  return(sin(x2 * x3) * x3)
}
#Derivada tercera de f1
df13 <- function(x1,x2,x3){
  return(sin(x2 * x3) * x2)
}

#         ii)Funcion 2 y sus derivadas----
f2 <- function(x1,x2,x3){
  return (4*x1^2 - 625*x2^2+2*x2-1)
}
#Derivada primera de f2
df21 <- function(x1,x2,x3){
  return(4 * (2 * x1))
}
#Derivada segunda de f2
df22 <- function(x1,x2,x3){
  return(2 - 625 * (2 * x2))
}
#Derivada tercera de f2
df23 <- function(x1,x2,x3){
  return(0)
}

#         iii)Funcion 3 y sus derivadas ----
f3 <- function(x1,x2,x3){
  return(20*x3 + (10*pi-3/3)+exp(-x1*x2))
}
#Derivada primera de f3
df31 <- function(x1,x2,x3){
  return(-(exp(-x1 * x2) * x2))
}
#Derivada segunda de f3
df32 <- function(x1,x2,x3){
  return(-(exp(-x1 * x2) * x1))
}
#Derivada tercera de f3
df33 <- function(x1,x2,x3){
  return(20)
}
# 5) Matriz Jacobiana y Fx ----
###Defino como será la Matriz jacobiana

Jacobiano <- function(x1,x2,x3){
  col1 <- 
  c(df11(x1,x2,x3),df12(x1,x2,x3),df13(x1,x2,x3))
  
  col2 <- 
  c(df21(x1,x2,x3),df22(x1,x2,x3),df23(x1,x2,x3))
  
  col3 <-
  c(df31(x1,x2,x3),df32(x1,x2,x3),df33(x1,x2,x3))
  
  J <- rbind(col1,col2, col3) #con esta ultima armamos la matrix ampliada
  return(J)
}
#Definimos ahora Fx
Fx <- function(x){
  Fx <- rbind(f1(x[1],x[2],x[3]), f2(x[1],x[2],x[3]), f3(x[1],x[2],x[3]))
  return(Fx)
} #sera una matriz ampliada con las funciones definadas antes

# 6) Defino los puntos ----
x <- c(0.1, 0.1, -0.1)
n=3
Resultado <- Sist_Ec_NoLineal_Newton(n, x, 10^-6, 1000)
Resultado

# 7) Chequeo el resultado ----
#Asigno los rdos del algoritmo a las variables x1,x2 y x3
x1 <- Sist_Ec_NoLineal_Newton(n,x, 10^-6, 100)[1] #posicion, osea mult por posicion 1
x2 <- Sist_Ec_NoLineal_Newton(n,x, 10^-6, 100)[2]
x3 <- Sist_Ec_NoLineal_Newton(n,x, 10^-6, 100)[3]

# 8) Imprimo resultados ----
f1(x1, x2, x3)
f2(x1, x2, x3)
f3(x1, x2, x3)
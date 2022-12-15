# Resolución numérica de sistemas no lineales.
# Alg. Newton - Matriz 2x2


# 1) Norma ----

norma <- function(y, metodo){
  if (metodo==2){
    return(sqrt(sum(y^2)))
  }
  if (metodo==Inf){
    return(max(abs(y)))
  }
  return("El metodo debe ser 2 o Inf")
}

# 2) Sistema Ec. No lineal ----

Sist_Ec_NoLineal_Newton <- function(n,x,TOL,N){
  #Paso 1
  k <- 1
  #Paso 2
  while(k<=N){
    #Paso 3
    fx <- Fx(x)
    J <- Jacobiano(x[1],x[2])
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


# 3) Calculo Aux para derivada----

mi.derivada <- function(funcion.matematica, var) {
  temp <- substitute(funcion.matematica)
  name <- deparse(substitute(var))
  D(temp, name)
}
##Ejemplo
mi.derivada(6*x1 - 2*cos(x2*x3) - 1, x1)
# Resultado: 6

f<-expression(3*x1*x2^2-(x1^3)-1)
fprima<-D(f,"x1")
fprima

# 4) Agregar fn y calcular las derivadas ----
# Funcion 1 y sus derivadas
f1 <- function(x1,x2){
  return(3*x1^2 - x2^2)
}
#Derivada primera de f1
df11 <- function(x1,x2){
  return(3 * (2 * x1))
}
#Derivada segunda de f1
df12 <- function(x1,x2){
  return(-(2 * x2))
}

# Funcion 2 y sus derivadas
f2 <- function(x1,x2){
  return (3*x1*x2^2-(x1^3)-1)
}
#Derivada primera de f2
df21 <- function(x1,x2){
  return(3 * x2^2 - 3 * x1^2)
}
#Derivada segunda de f2
df22 <- function(x1,x2){
  return(3 * x1 * (2 * x2))
}


# 5) Defino como será la Matriz jacobiana----

Jacobiano <- function(x1,x2){
  col1 <- 
    c(df11(x1,x2),df12(x1,x2))
  
  col2 <- 
    c(df21(x1,x2),df22(x1,x2))
  
  
  J <- rbind(col1,col2) #con esta ultima armamos la matrix ampliada
  return(J)
}
#Definimos ahora Fx
Fx <- function(x){
  Fx <- rbind(f1(x[1],x[2]), f2(x[1],x[2]))
  return(Fx)
} #sera una matriz ampliada con las funciones definadas antes

# 6) Elegimos los puntos donde se evaluan la fn y el jacobiano ----
x <- c(0.1, 0.1)
n=2
Sist_Ec_NoLineal_Newton(n, x, 10^-6, 100)
paste("X1 sera:",Sist_Ec_NoLineal_Newton(n, x, 10^-6, 100)[1],"y x2 sera",
Sist_Ec_NoLineal_Newton(n, x, 10^-6, 100)[2])
# 7) CHEQUEO CON R ----
#Asigno los rdos del algoritmo a las variables x1,x2
x1 <- Sist_Ec_NoLineal_Newton(n,x, 10^-16, 10000)[1] #posicion, osea mult por posicion 1
x2 <- Sist_Ec_NoLineal_Newton(n,x, 10^-16, 10000)[2]

# 8) imprimo resultados ----
a <-f1(x1, x2)

b <- f2(x1, x2)

#Broyden para 2x2

# 1) Definimos la Norma ----
Norma <- function(y, metodo){
  if (metodo==2){
    return(sqrt(sum(y^2)))
  }
  if (metodo==Inf){
    return(max(abs(y)))
  }
  return("El metodo debe ser 2 o Inf")
}

# 2) Definimos el algoritmo 10.2 Metodo de Broyden  ----

SEnoLBroyden_10.2 <- function(n,x,Tol,N){ #una función del número de ecuaciones, un vector x traspuesto, la tolerancia y el numero de iteraciones 
  
  #Paso 1
  Jx = matrix(c(df11(x1,x2),df12(x1,x2),
                df21(x1,x2),df22(x1,x2)),ncol=2,byrow=T)
  
  Fx = matrix(c(f1(x[1],x[2]),f2(x[1],x[2])),ncol=1)
  v = Fx
  
  #Paso 2
  A= solve(Jx) #usar eliminacion gausiana, solve me calcula la inversa
  
  #Paso 3
  s <- -A%*%v
  x<- x + s
  k<- 2
  
  #Paso 4
  for(k in 2:N) {
    
    #Paso 5 
    w<- v
    v<- matrix(c(f1(x[1],x[2]),f2(x[1],x[2])),ncol=1)
    y<- v - w
    
    #Paso 6
    z<- -A%*%y
    #Paso 7
    p<- (-1)%*%t(s)%*%z
    p<- p[1]
    #Paso 8
    u <- t(s)%*%A
    #Paso 9
    r = (s+z)%*%(u/p)
    A = A + r
    #Paso 10
    s<- -A%*%v
    #Paso 11
    x<- x + s
    #Paso 12
    if(Norma(s,2) < Tol){
      return(x)
    } 
    #Paso 13
    k<- k + 1 
  }
  
  #Paso 14
  
  return(paste('Numero maximo de iteraciones excedido'))
}
# 3) FUNCIONES, DERIVADAS Y APLICACION BROYDEN----
#        i)  Defino los puntos----
x1<-0
x2<- 0


#       ii)  Funcion 1 y sus derivadas ---- 

f1 <- function(x1,x2){
  return(4*x1^2-20*x1+1/4*x2^2+8)
}

f<-expression(x2^2-2*x1*x3)
fprima<-D(f,"x3")
fprima

#Derivada primera de f1
df11 <- function(x1,x2){
  return(4 * (2 * x1) - 20)
}
#Derivada segunda de f1
df12 <- function(x1,x2){
  return(1/4 * (2 * x2))
}


#       iii) Funcion 2 y sus derivadas ----
f2 <- function(x1,x2){
  return (1/2*x1*x2^2 + 2*x1- 5*x2+8)
}

f<-expression(x2^2-2*x1*x3)
fprima<-D(f,"x3")
fprima

#Derivada primera de f2
df21 <- function(x1,x2){
  return(1/2 * x2^2 + 2)
}
#Derivada segunda de f2
df22 <- function(x1,x2){
  return(1/2 * x1 * (2 * x2) - 5)
}
# 4) Matriz jacobiana ----

Jacobiano <- function(x1,x2){
  col1 <- c(df11(x1,x2),df12(x1,x2))
  col2 <- c(df21(x1,x2),df22(x1,x2))
  J <- rbind(col1,col2)
  return(J)
}

# 5) Aplicacion del metodo ---- 

Fx <- function(x){
  Fx <- rbind(f1(x[1],x[2]), f2(x[1],x[2]))
  return(Fx)
}

x <- c(x1, x2)

SEnoLBroyden_10.2(3, x, 10^-6, 100)

# 6) Compruebo ----
x1 <- SEnoLBroyden_10.2(2, x, 10^-6, 100)[1]
x2 <- SEnoLBroyden_10.2(2, x, 10^-6, 100)[2]

f1(x1,x2)
f2(x1,x2)

#Broyden para 3x3

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

SEnoLBroyden_10.2 <- function(n,x,Tol,N) { #una funci?n del n?mero de ecuaciones, un vector x traspuesto, la tolerancia y el numero de iteraciones 
  
  #Paso 1
  Jx = matrix(c(df11(x1,x2,x3),df12(x1,x2,x3),df13(x1,x2,x3),
                df21(x1,x2,x3),df22(x1,x2,x3),df23(x1,x2,x3),
                df31(x1,x2,x3),df32(x1,x2,x3),df33(x1,x2,x3)),ncol=3,byrow=T)
  Fx = matrix(c(f1(x[1],x[2],x[3]),f2(x[1],x[2],x[3]),f3(x[1],x[2],x[3])),ncol=1)
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
    v<- matrix(c(f1(x[1],x[2],x[3]),f2(x[1],x[2],x[3]),f3(x[1],x[2],x[3])),ncol=1)
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

# #) FUNCIONES, DERIVADAS Y APLICACION BROYDEN----
#        i)  Defino los puntos----
x1<- 0
x2<- 0
x3<- 0

#       ii)  Funcion 1 y sus derivadas ---- 

f1 <- function(x1,x2,x3){
  return(6*x1 - 2*cos(x2*x3) -1)
}


f<-expression(6*x1 - 2*cos(x2*x3) -1)
fprima<-D(f,"x3")
fprima



df11 <- function(x1,x2,x3){
  return(6)
}

df11(x1,x2,x3)


df12 <- function(x1,x2,x3){
  return(2 * (sin(x2 * x3) * x3))
}

df12(x1,x2,x3)

df13 <- function(x1,x2,x3){
  return(2 * (sin(x2 * x3) * x2))
}

df13(x1,x2,x3)

#       iii) Funcion 2 y sus derivadas ----
f2 <- function(x1,x2,x3){
  return(9*x2 + sqrt(x1^2 + sin(x3) +1.06) + 0.9)
}


f<-expression(9*x2 + sqrt(x1^2 + sin(x3) +1.06) + 0.9)
fprima<-D(f,"x3")
fprima


df21 <- function(x1,x2,x3){
  return(0.5 * (2 * x1 * (x1^2 + sin(x3) + 1.06)^-0.5))
}

df21(x1,x2,x3)

df22 <- function(x1,x2,x3){
  return(9)
}

df22(x1,x2,x3)

df23 <- function(x1,x2,x3){
  return(0.5 * (cos(x3) * (x1^2 + sin(x3) + 1.06)^-0.5))
}

df23(x1,x2,x3)

#       vi)  Funcion 3 y sus derivadas ----
f3 <- function(x1,x2,x3){
  return(60*x3 + 3*exp(-x1*x2) + 10*pi - 3)
}


f<-expression(60*x3 + 3*exp(-x1*x2) + 10*pi - 3)
fprima<-D(f,"x3")
fprima


df31 <- function(x1,x2,x3){
  return(-(3 * (exp(-x1 * x2) * x2)))
}

df31(x1,x2,x3)

df32 <- function(x1,x2,x3){
  return(-(3 * (exp(-x1 * x2) * x1)))
}

df32(x1,x2,x3)

df33 <- function(x1,x2,x3){
  return(60)
}

df33(x1,x2,x3)

# 3) Matriz jacobiana ----

Jacobiano <- function(x1,x2,x3){
  col1 <- c(df11(x1,x2,x3),df12(x1,x2,x3),df13(x1,x2,x3))
  col2 <- c(df21(x1,x2,x3),df22(x1,x2,x3),df23(x1,x2,x3))
  col3 <- c(df31(x1,x2,x3),df32(x1,x2,x3),df33(x1,x2,x3))
  J <- rbind(col1,col2, col3)
  return(J)
}

# 4) Aplicacion del metodo ---- 

Fx <- function(x){
  Fx <- rbind(f1(x[1],x[2],x[3]), f2(x[1],x[2],x[3]), f3(x[1],x[2],x[3]))
  return(Fx)
}

x <- c(x1, x2, x3)

SEnoLBroyden_10.2(3, x, 10^-6, 100)

# 5) Compruebo ----
x1 <- SEnoLBroyden_10.2(3, x, 10^-6, 100)[1]
x2 <- SEnoLBroyden_10.2(3, x, 10^-6, 100)[2]
x3 <- SEnoLBroyden_10.2(3, x, 10^-6, 100)[3]

f1(x1,x2,x3)
f2(x1,x2,x3)
f3(x1,x2,x3)
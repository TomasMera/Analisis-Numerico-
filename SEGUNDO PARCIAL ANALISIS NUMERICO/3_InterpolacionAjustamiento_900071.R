#### INTERPOLACION Y AJUSTAMIENTO ####
#TOMAS MERA 900071

S = seq(30,50,2.5)

c <- c(0.0097,0.0514,0.1902,0.5320,1.1938,2.2548,3.7250,5.5505,7.6442)

#3.1 ----
#algoritmo#
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

F_48.75_Aprox <- Lagrange(48.75,S,c)
F_48.75_Aprox

#Da un resultado bastante confiable ya que F(47.5) = 5.5505, F(50) = 7.6442
# Y esta aproximacion de F(48.75) esta entre los dos


S2 <- c(S[6], S[7], S[8], S[9])
c2 <- c(c[6],c[7], c[8],c[9])

F48.75_Aprox2 <- Lagrange(48.75,S2,c2)
F_48.75_Aprox2

#Tambien da una aproximacion bastante parecida, ya que el punto que se quiere 
#aproximar esta entre el conjunto de puntos dados


S3 <- c(S[8], S[9])
c3 <- c(c[8],c[9])

F_48.75_Aprox3 <- Lagrange(48.75,S3,c3)
F_48.75_Aprox3

#Por ultimo este algoritmo tambien aproxima bastante parecido, y suponemos que bien,
#Ya que se ingresan los puntos que contienen al que queremos aproximar en el medio



#3.2 ----

#Construyo trazador
Natural_Cubic_Spline_SoloPol = function(A){
  #Variables iniciales 
  x = A[,1]
  fx = A[,2]
  if(length(x)!=length(fx)){
    return("La cantidad de argumentos no coincide")
  }
  n = length(A[,1]); b = c(rep(NA,n)); c = c(rep(NA,n)); d = c(rep(NA,n)); h = c(rep(NA,n))
  a = c(rep(NA,n)); l = c(rep(NA,n)); u = c(rep(NA,n)); z = c(rep(NA,n))
  #Paso 1 y 2 ----
  for(i in 1:(n-1)){
    h[i] = x[i+1]-x[i]
  }
  for(i in 2:(n-1)){
    a[i] = ((3/(h[i])) * (fx[i+1]- fx[i])) - ((3/(h[i-1])) * (fx[i]- fx[i-1]))
  }
  #Paso 3 
  l[1] = 1;u[1] = 0;z[1] = 0
  #Paso 4 
  for(i in 2:(n-1)){
    l[i] = 2*(x[i+1] - x[i-1]) - (h[i-1]*u[i-1])
    u[i] = h[i]/l[i]
    z[i] = (a[i]- h[i-1]*z[i-1])/l[i] 
  }
  #Paso 5 
  l[n] = 1 ;z[n] = 0;c[n] = 0
  #Paso 6 
  for(j in (n-1):1){
    c[j] = z[j] - (u[j]*c[j+1])
    b[j] = ((fx[j+1] - fx[j])/h[j]) - h[j]*(c[j+1] + 2*c[j])/3
    d[j] = (c[j+1] - c[j])/(3*h[j])
  }
  Pol = NULL
  for(i in 1:(n-1)){
    Pol = c(Pol,paste("",round(fx[i],4),"+(",round(b[i],4),"*(x-",round(x[i],4),"))+(",round(c[i],4),"*((x-",round(x[i],4),")^2))+(",round(d[i],4),"*((x-",round(x[i],4),")^3))   para todo x perteneciente a [",x[i],",",x[i+1],"]",sep = ""))
  }
  Pol = matrix(Pol,nrow = (n-1), ncol = 1, byrow = T)
  e = NULL
  for(i in 1:(n-1)){
    e = c(e,paste("S.",i,"(x) = ",sep = ""))
  }
  dimnames(Pol) = list(e,NULL)
  return(Pol)
}

A = cbind(S,c)

Construye_Trazador = Natural_Cubic_Spline_SoloPol(A)  
Construye_Trazador


# S.1(x) =  "0.0097+(0.0103*(x-30))+(0*((x-30)^2))+(0.001*((x-30)^3))   para todo x perteneciente a [30,32.5]"             
# S.2(x) =  "0.0514+(0.0295*(x-32.5))+(0.0077*((x-32.5)^2))+(0.0011*((x-32.5)^3))   para todo x perteneciente a [32.5,35]" 
# S.3(x) =  "0.1902+(0.0884*(x-35))+(0.0159*((x-35)^2))+(0.0014*((x-35)^3))   para todo x perteneciente a [35,37.5]"       
# S.4(x) =  "0.532+(0.1936*(x-37.5))+(0.0262*((x-37.5)^2))+(9e-04*((x-37.5)^3))   para todo x perteneciente a [37.5,40]"   
# S.5(x) =  "1.1938+(0.3413*(x-40))+(0.0329*((x-40)^2))+(1e-04*((x-40)^3))   para todo x perteneciente a [40,42.5]"        
# S.6(x) =  "2.2548+(0.5083*(x-42.5))+(0.0339*((x-42.5)^2))+(-8e-04*((x-42.5)^3))   para todo x perteneciente a [42.5,45]" 
# S.7(x) =  "3.725+(0.6628*(x-45))+(0.0278*((x-45)^2))+(-4e-04*((x-45)^3))   para todo x perteneciente a [45,47.5]"        
# S.8(x) =  "5.5505+(0.7954*(x-47.5))+(0.0252*((x-47.5)^2))+(-0.0034*((x-47.5)^3))   para todo x perteneciente a [47.5,50]"




Natural_Cubic_Spline_InterpolaEnPto = function(A,xk){
  #Variables iniciales 
  x = A[,1]
  fx = A[,2]
  if(length(x)!=length(fx)){
    return("La cantidad de argumentos no coincide")
  }
  n = length(A[,1]); b = c(rep(NA,n)); c = c(rep(NA,n)); d = c(rep(NA,n)); h = c(rep(NA,n))
  a = c(rep(NA,n)); l = c(rep(NA,n)); u = c(rep(NA,n)); z = c(rep(NA,n))
  #Paso 1 y 2 ----
  for(i in 1:(n-1)){
    h[i] = x[i+1]-x[i]
  }
  for(i in 2:(n-1)){
    a[i] = ((3/(h[i])) * (fx[i+1]- fx[i])) - ((3/(h[i-1])) * (fx[i]- fx[i-1]))
  }
  #Paso 3 ----
  l[1] = 1;u[1] = 0;z[1] = 0
  #Paso 4 ----
  for(i in 2:(n-1)){
    l[i] = 2*(x[i+1] - x[i-1]) - (h[i-1]*u[i-1])
    u[i] = h[i]/l[i]
    z[i] = (a[i]- h[i-1]*z[i-1])/l[i] 
  }
  #Paso 5 ----
  l[n] = 1 ;z[n] = 0;c[n] = 0
  #Paso 6 ----
  for(j in (n-1):1){
    c[j] = z[j] - (u[j]*c[j+1])
    b[j] = ((fx[j+1] - fx[j])/h[j]) - h[j]*(c[j+1] + 2*c[j])/3
    d[j] = (c[j+1] - c[j])/(3*h[j])
  }
  #interpolacion en el punto ----
  for(i in 1:(n-1)) {
    if(x[i]<= xk & x[i+1]>= xk){
      s = i}
  } #Nos fijamos a que polinomio pertenece el valor a interpolar
  yk = fx[s]+b[s]*(xk-x[s])+c[s]*(xk-x[s])^2+d[s]*(xk-x[s])^3 #Lo valuamos en el polinomio correspondiente
  return(yk)
}


F_48.75_Interpolado <- Natural_Cubic_Spline_InterpolaEnPto(A,48.75)
F_48.75_Interpolado

#Da un resultado bastante parecido al obtenido con los polinomios de Lagrange, Es una buena aproximacion



#3.3 ----
# GRAFICO #
library(ggplot2)

Tabla_1 <- data.frame(S,c)
#para añadir al grafico el trazador voy a interpolar en 1000 puntos, usando como base la funcion que tenia programada
x = A[,1]
fx = A[,2]
if(length(x)!=length(fx)){
  return("La cantidad de argumentos no coincide")
}
n = length(A[,1]); b = c(rep(NA,n)); c = c(rep(NA,n)); d = c(rep(NA,n)); h = c(rep(NA,n))
a = c(rep(NA,n)); l = c(rep(NA,n)); u = c(rep(NA,n)); z = c(rep(NA,n))
#Paso 1 y 2 
for(i in 1:(n-1)){
  h[i] = x[i+1]-x[i]
}
for(i in 2:(n-1)){
  a[i] = ((3/(h[i])) * (fx[i+1]- fx[i])) - ((3/(h[i-1])) * (fx[i]- fx[i-1]))
}
#Paso 3 
l[1] = 1;u[1] = 0;z[1] = 0
#Paso 4 
for(i in 2:(n-1)){
  l[i] = 2*(x[i+1] - x[i-1]) - (h[i-1]*u[i-1])
  u[i] = h[i]/l[i]
  z[i] = (a[i]- h[i-1]*z[i-1])/l[i] 
}
#Paso 5 
l[n] = 1 ;z[n] = 0;c[n] = 0
#Paso 6 
for(j in (n-1):1){
  c[j] = z[j] - (u[j]*c[j+1])
  b[j] = ((fx[j+1] - fx[j])/h[j]) - h[j]*(c[j+1] + 2*c[j])/3
  d[j] = (c[j+1] - c[j])/(3*h[j])
}
B = seq(x[1],x[length(x)],length.out = 1000)
B2 = NULL
for(r in 1:1000){
  for(i in 1:(n-1)){
    if(x[i]<=B[r] & x[i+1]>= B[r]){
      g = i
    }
  }
  rk = fx[g]+b[g]*(B[r]-x[g])+c[g]*(B[r]-x[g])^2+d[g]*(B[r]-x[g])^3
  B2 = c(B2,rk)
} #Interpolamos 1000 numeros para poder grafica

DF_Cubic <- data.frame(B,B2)


##Armo los puntos para graficar lagrange
xi <- seq(from=min(S),to=max(S),length.out=1000)
Px <- rep(NA,1000)
for(i in 1:1000){
  Px[i]<-Lagrange(xi[i],S,c)
}

DF_Lagrange <- data.frame(xi,Px)

Table_Graf2 <-merge(DF_Lagrange, Tabla_1, by.x =  "xi",by.y = "S", all =T)




Grafico <- ggplot(data = DF_Cubic, aes(x = B, y = B2))+
  geom_line(colour = "orange", size = 1)+
  geom_point(data= Tabla_1, aes(x =S,y=c), col = "Pink" , pch = 3)+
  geom_point(data = DF_Cubic, aes(x = 48.75, y = F_48.75_Interpolado), pch = 15, col = "orange")+
  geom_line(data = Table_Graf2, aes(x = xi, y = Px), col = "Blue" )+
  geom_point(data = Table_Graf2, aes(x = 48.75, y = F_48.75_Aprox), col = "Blue", pch =15)+
  scale_x_continuous(name = "x", limits = c(30,50)) +
  ggtitle("Comparacion Lagrange, Cubic Splines")+
  xlab("Eje x")+ 
  ylab("Eje y")+
  theme_light()

Grafico

#### EJERCICIO 1 ####
# 1.1 ----
Metodo_Trapecio <- function(a, b){
  h = (b - a)
  xo = a
  x1 = b
    
  result = (h/2)*(fn(xo) + fn(x1))
  return(result)
}

Metodo_Simpson <- function(a,b){
  h = (b - a)/2
  xo = a
  x2 = b
  
  x1 = (xo + x2)/2
  #x1a = a + (b -a)/2
  
  result = (h/3)*(fn(xo) + 4*fn(x1) + fn(x2))
  return(result)
}

Metodo_TresOctavos_Simpson <- function(a,b){
  h = (b - a)/3
  xo = a
  x3 = b
  
  x1 = xo + h
  x2 = xo + 2*h
  
  result = ((3/8)*h)*(fn(xo) + 3*fn(x1) + 3*fn(x2) + fn(x3))
  return(result)
}

#Parametros
alpha = 4.36
theeta = 2.52

fn <- function (x){
  val <- (alpha^2 *(x/theeta)^alpha)/ (x*(1+(x/theeta)^alpha)^(alpha+1))
  return(val)
}

#Probabilidad de que Y este entre 1.15 y 4.99
a = 1.15
b = 4.99

#Con el metodo del trapecio
print("Nodos utilizados: y0, y1")
Probabilidad_Con_Trapecio = Metodo_Trapecio(a,b)
Probabilidad_Con_Trapecio

#Con el metodo de Simpson}
print("Nodos utilizados: y0, y1, y2")
Probabilidad_Con_Simpson = Metodo_Simpson(a,b)
Probabilidad_Con_Simpson

#Con el metodo de Tres Octavos de Simpson
print("Nodos utilizados: y0,y1,y2,y3")
Probabilidad_Con_Simpson3OCtavos = Metodo_TresOctavos_Simpson(a,b)
Probabilidad_Con_Simpson3OCtavos


#1.2 ----
trapecio.compuesto = function(a, b, n){
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

n=17
#Probabilidad
Probabilidad_Con_TrapecioCompuesto = trapecio.compuesto(a,b,n)
Probabilidad_Con_TrapecioCompuesto


#COTA DEL ERROR
alim<-1.15   #cargar límite inferior intervalo
blim<-4.99  #cargar límite superior intervalo

d1tc<-D(expression((alpha^2 *(x/theeta)^alpha)/ (x*(1+(x/theeta)^alpha)^(alpha+1))),"x") #cargar función que desea derivar
d2tc<-D(d1tc,"x")

d2tc

derivadatc2<-function(x){
  f<-alpha^2 * ((x/theeta)^((alpha - 1) - 1) * ((alpha - 1) * (1/theeta)) * 
                  (alpha * (1/theeta)))/(x * (1 + (x/theeta)^alpha)^(alpha + 
                                                                       1)) - alpha^2 * ((x/theeta)^(alpha - 1) * (alpha * (1/theeta))) * 
    ((1 + (x/theeta)^alpha)^(alpha + 1) + x * ((1 + (x/theeta)^alpha)^((alpha + 
                                                                          1) - 1) * ((alpha + 1) * ((x/theeta)^(alpha - 1) * (alpha * 
                                                                                                                                (1/theeta))))))/(x * (1 + (x/theeta)^alpha)^(alpha + 
                                                                                                                                                                               1))^2 - ((alpha^2 * ((x/theeta)^(alpha - 1) * (alpha * (1/theeta))) * 
                                                                                                                                                                                           ((1 + (x/theeta)^alpha)^(alpha + 1) + x * ((1 + (x/theeta)^alpha)^((alpha + 
                                                                                                                                                                                                                                                                 1) - 1) * ((alpha + 1) * ((x/theeta)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                                                                       (1/theeta)))))) + (alpha^2 * (x/theeta)^alpha) * ((1 + 
                                                                                                                                                                                                                                                                                                                                                                            (x/theeta)^alpha)^((alpha + 1) - 1) * ((alpha + 1) * ((x/theeta)^(alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                1) * (alpha * (1/theeta)))) + (((1 + (x/theeta)^alpha)^((alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           1) - 1) * ((alpha + 1) * ((x/theeta)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (1/theeta))))) + x * ((1 + (x/theeta)^alpha)^(((alpha + 1) - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1) - 1) * (((alpha + 1) - 1) * ((x/theeta)^(alpha - 1) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (alpha * (1/theeta)))) * ((alpha + 1) * ((x/theeta)^(alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           1) * (alpha * (1/theeta)))) + (1 + (x/theeta)^alpha)^((alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    1) - 1) * ((alpha + 1) * ((x/theeta)^((alpha - 1) - 1) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta))))))))/(x * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           (1 + (x/theeta)^alpha)^(alpha + 1))^2 - (alpha^2 * (x/theeta)^alpha) * 
                                                                                                                                                                                          ((1 + (x/theeta)^alpha)^(alpha + 1) + x * ((1 + (x/theeta)^alpha)^((alpha + 
                                                                                                                                                                                                                                                                1) - 1) * ((alpha + 1) * ((x/theeta)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                                                                      (1/theeta)))))) * (2 * (((1 + (x/theeta)^alpha)^(alpha + 
                                                                                                                                                                                                                                                                                                                                                                         1) + x * ((1 + (x/theeta)^alpha)^((alpha + 1) - 1) * ((alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                                  1) * ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)))))) * 
                                                                                                                                                                                                                                                                                                                                                (x * (1 + (x/theeta)^alpha)^(alpha + 1))))/((x * (1 + (x/theeta)^alpha)^(alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                           1))^2)^2)
  return(f)
}

optimotc<-optimize(derivadatc2,c(alim,blim),maximum = T)
opttc<-abs(optimotc$objective)
opttc

error<-((blim-alim)/12)*(((blim-alim)/n)^2)*opttc
error

#RELACION CON EL PUNTO 1.1
# El resultado es mas exacto porque trapecio compuesto genera cierta cantidad de intervalos, en este caso 17,
# Y genera un trapecio por intervalo, por consecuencia, la cota del error es menor.


#1.3 ----
#La esperanza se calcula como la integral en el dominio de Y * F(Y)

fn <- function (x){
  val <- x * ((alpha^2 *(x/theeta)^alpha)/ (x*(1+(x/theeta)^alpha)^(alpha+1)))
  return(val)
}

n= 474

Esperanza_y = trapecio.compuesto(a,b,n)
Esperanza_y

#COTA DEL ERROR
d1tc<-D(expression(x * ((alpha^2 *(x/theeta)^alpha)/ (x*(1+(x/theeta)^alpha)^(alpha+1)))),"x") #cargar función que desea derivar
d2tc<-D(d1tc,"x")

d2tc

derivadatc2<-function(x){
  f<-alpha^2 * ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)))/(x * 
                                                                  (1 + (x/theeta)^alpha)^(alpha + 1)) - (alpha^2 * (x/theeta)^alpha) * 
    ((1 + (x/theeta)^alpha)^(alpha + 1) + x * ((1 + (x/theeta)^alpha)^((alpha + 
                                                                          1) - 1) * ((alpha + 1) * ((x/theeta)^(alpha - 1) * (alpha * 
                                                                                                                                (1/theeta))))))/(x * (1 + (x/theeta)^alpha)^(alpha + 
                                                                                                                                                                               1))^2 + ((alpha^2 * ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)))/(x * 
                                                                                                                                                                                                                                                      (1 + (x/theeta)^alpha)^(alpha + 1)) - (alpha^2 * (x/theeta)^alpha) * 
                                                                                                                                                                                           ((1 + (x/theeta)^alpha)^(alpha + 1) + x * ((1 + (x/theeta)^alpha)^((alpha + 
                                                                                                                                                                                                                                                                 1) - 1) * ((alpha + 1) * ((x/theeta)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                                                                       (1/theeta))))))/(x * (1 + (x/theeta)^alpha)^(alpha + 
                                                                                                                                                                                                                                                                                                                                                                      1))^2) + x * (alpha^2 * ((x/theeta)^((alpha - 1) - 1) * ((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                  1) * (1/theeta)) * (alpha * (1/theeta)))/(x * (1 + (x/theeta)^alpha)^(alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          1)) - alpha^2 * ((x/theeta)^(alpha - 1) * (alpha * (1/theeta))) * 
                                                                                                                                                                                                                                                                                                                                                                                      ((1 + (x/theeta)^alpha)^(alpha + 1) + x * ((1 + (x/theeta)^alpha)^((alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                            1) - 1) * ((alpha + 1) * ((x/theeta)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (1/theeta))))))/(x * (1 + (x/theeta)^alpha)^(alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1))^2 - ((alpha^2 * ((x/theeta)^(alpha - 1) * (alpha * (1/theeta))) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ((1 + (x/theeta)^alpha)^(alpha + 1) + x * ((1 + (x/theeta)^alpha)^((alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   1) - 1) * ((alpha + 1) * ((x/theeta)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         (1/theeta)))))) + (alpha^2 * (x/theeta)^alpha) * ((1 + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (x/theeta)^alpha)^((alpha + 1) - 1) * ((alpha + 1) * ((x/theeta)^(alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1) * (alpha * (1/theeta)))) + (((1 + (x/theeta)^alpha)^((alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1) - 1) * ((alpha + 1) * ((x/theeta)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   (1/theeta))))) + x * ((1 + (x/theeta)^alpha)^(((alpha + 1) - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    1) - 1) * (((alpha + 1) - 1) * ((x/theeta)^(alpha - 1) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (alpha * (1/theeta)))) * ((alpha + 1) * ((x/theeta)^(alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1) * (alpha * (1/theeta)))) + (1 + (x/theeta)^alpha)^((alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      1) - 1) * ((alpha + 1) * ((x/theeta)^((alpha - 1) - 1) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta))))))))/(x * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             (1 + (x/theeta)^alpha)^(alpha + 1))^2 - (alpha^2 * (x/theeta)^alpha) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ((1 + (x/theeta)^alpha)^(alpha + 1) + x * ((1 + (x/theeta)^alpha)^((alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1) - 1) * ((alpha + 1) * ((x/theeta)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (1/theeta)))))) * (2 * (((1 + (x/theeta)^alpha)^(alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           1) + x * ((1 + (x/theeta)^alpha)^((alpha + 1) - 1) * ((alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    1) * ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)))))) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (x * (1 + (x/theeta)^alpha)^(alpha + 1))))/((x * (1 + (x/theeta)^alpha)^(alpha + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1))^2)^2)))
  return(f)
}

optimotc<-optimize(derivadatc2,c(alim,blim),maximum = T)
opttc<-abs(optimotc$objective)
opttc

error<-((b-a)/12)*(((b-a)/n)^2)*opttc
error

#1.4----
#Calculamos E(Y^2)
fn <- function (x){
  val <- x^2 * ((alpha^2 *(x/theeta)^alpha)/ (x*(1+(x/theeta)^alpha)^(alpha+1)))
  return(val)
}

Esperanza_y2 = trapecio.compuesto(a,b,n)
Esperanza_y2

Varianza_Y = Esperanza_y2 - Esperanza_y^2
Varianza_Y


#### EJERCICIO 2 #####
r <- seq(0,0.12, by=0.01)

Pr <- c(115.3371, 109.7031,104.8099,100.0653,95.3516,91.1998,87.2676,83.5825,79.9116,76.8435,73.9473,70.0320,67.7022)



derivada3ptos_Progresiva_Endpoint <- function(x,y,xindice){
  n = length(y)
  yprima = c(rep(NA,n))
  h = diff(x)[1]
  yprima[xindice] = (-y[xindice-1] + y[xindice+1])/(2*h)
  return(yprima[xindice])
}




# 2.1 Derivada Primera ----
Derivada_0.03 = derivada3ptos_Progresiva_Endpoint(r,Pr,4) #Indice del 0.03 es 4
Derivada_0.03

Derivada_0.04 = derivada3ptos_Progresiva_Endpoint(r,Pr,5) #Indice del 0.04 es 5
Derivada_0.04

#2.2 Derivada Segunda ----
derivada_segunda = function(x, y, xindice){
  h = diff(x)[1]
  n = length(y)
  yprima = c(rep(NA,n))
  
  if(missing(xindice)){
    for (i in 2:(n - 1)) {
      yprima[i] = (y[i-1] - 2*y[i] + y[i+1])/(h^2)
    }
    
    tabla = cbind.data.frame(x, y, yprima)
    return(tabla)
  }
  else{
    if(xindice == 1 || xindice == n){
      return("Este valor no se puede calcular")
    }
    else{
      yprima[xindice] = (y[xindice-1] - 2*y[xindice] + y[xindice+1])/(h^2)
      return(yprima[xindice])
    }
  }
}

Derivada_Segunda_0.03 = derivada_segunda(r,Pr,4)
Derivada_Segunda_0.03

Derivada_Segunda_0.04 = derivada_segunda(r,Pr,5)
Derivada_Segunda_0.04



#### EJERCICIO 3 ####

x<- c(1.6579,3.8119,4.1155,5.1111,6.0537,6.2298,11.6898,11.7199,12.7634)

Fx <- c(0.1111,0.2222,0.3333,0.4444,0.5556,0.6667,0.7778,0.8889,1)

#3.1 ----
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

F12.2416 <- Lagrange(12.2416,x,Fx)
F12.2416
# El polinomio de Lagrange aproxima mal los valores extremos, por eso no es de extrañar el resultado.
# Esto se debe a que aproxima con un polinomio de grado n-1, en este caso un polinomio de grado 8.


x2 <-c(x[7],x[8],x[9])
x2

Fx2 <- c(Fx[7],Fx[8],Fx[9])
Fx2

Lagrange(12.2416,x2,Fx2)
# En este caso es un polinomio de grado 2, pero sigue aproximando "mal" un valor extremo

x3 <-c(x[8],x[9])
x3

Fx3<-c(Fx[8],Fx[9])
Fx3

Lagrange(12.2416,x3,Fx3)

# En el último caso aproxima mejor porque es un polinomio de grado 1, una recta.
# Esto impide que tome valores muy altos en los extremos por el comportamiendo de la función.



#3.2 ----
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

A <- cbind(x,Fx)

Polinomios = Natural_Cubic_Spline_SoloPol(A)  
Polinomios

#Los trazadores Cubicos Sj(x) son:
#S.1(x) =  "0.1111+(-0.0982*(x-1.6579))+(0*((x-1.6579)^2))+(0.0323*((x-1.6579)^3))   para todo x perteneciente a [1.6579,3.8119]"          
#S.2(x) =  "0.2222+(0.3512*(x-3.8119))+(0.2086*((x-3.8119)^2))+(-0.527*((x-3.8119)^3))   para todo x perteneciente a [3.8119,4.1155]"      
#S.3(x) =  "0.3333+(0.3321*(x-4.1155))+(-0.2713*((x-4.1155)^2))+(0.05*((x-4.1155)^3))   para todo x perteneciente a [4.1155,5.1111]"       
#S.4(x) =  "0.4444+(-0.0594*(x-5.1111))+(-0.1219*((x-5.1111)^2))+(0.329*((x-5.1111)^3))   para todo x perteneciente a [5.1111,6.0537]"     
#S.5(x) =  "0.5556+(0.5876*(x-6.0537))+(0.8083*((x-6.0537)^2))+(-3.1946*((x-6.0537)^3))   para todo x perteneciente a [6.0537,6.2298]"     
#S.6(x) =  "0.6667+(0.5751*(x-6.2298))+(-0.8794*((x-6.2298)^2))+(0.1425*((x-6.2298)^3))   para todo x perteneciente a [6.2298,11.6898]"    
#S.7(x) =  "0.7778+(3.7123*(x-11.6898))+(1.454*((x-11.6898)^2))+(-71.7894*((x-11.6898)^3))   para todo x perteneciente a [11.6898,11.7199]"
#S.8(x) =  "0.8889+(3.6047*(x-11.7199))+(-5.0286*((x-11.7199)^2))+(1.6063*((x-11.7199)^3))   para todo x perteneciente a [11.7199,12.7634]"

#Como 12.2416 esta entre 11.7199 y 12.7634 Utilizo el polinomio 8 para aproximar F(12.2416)

Natural_Cubic_Spline_Interpola_en_Punto = function(A,xk){
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

Interpolacion_en_Punto <- Natural_Cubic_Spline_Interpola_en_Punto(A,12.7634)
Interpolacion_en_Punto

Funcion_que_interpola <- function(x){
  f <- 0.8889+(3.6047*(x-11.7199))+(-5.0286*((x-11.7199)^2))+(1.6063*((x-11.7199)^3))
  return(f)
}

Interpolacion_en_Pto <- Funcion_que_interpola(12.7634)
Interpolacion_en_Pto

# Cubic Spline utiliza polinomios de grado 3 para aproximar en cada intervalo.
# Tiene la ventaja que da resultados mas suaves en los valores extremos que los polinomios de Lagrange

#3.3----
DATOS <- data.frame(x,Fx)


#3.4 ----
#GRAFICO
Tabla = data.frame(x,Fx)

xi <- seq(from=min(x),to=max(x),length.out=1000)
Px <- rep(NA,1000)
for(i in 1:1000){
  Px[i]<-Lagrange(xi[i],x,Fx)
}

DF_Lagrange <- data.frame(xi,Px)

Table_Graf2 <-merge(DF_Lagrange, Tabla, by.x =  "xi",by.y = "x", all =T)

# Evaluo el Spline
x_sp1 <- seq(from = 1.6579, to = 3.8119, by = 0.01)
fx_sp1 <- eval(parse(text = "0.1111+(-0.0982*(x-1.6579))+(0*((x-1.6579)^2))+(0.0323*((x-1.6579)^3))"), list(x = x_sp1))

x_sp2 <- seq(from = 3.8119, to = 4.1155, by = 0.01)
fx_sp2 <- eval(parse(text = "0.2222+(0.3512*(x-3.8119))+(0.2086*((x-3.8119)^2))+(-0.527*((x-3.8119)^3))"), list(x = x_sp2))

x_sp3 <- seq(from = 4.1155, to = 5.1111, by = 0.01)
fx_sp3 <- eval(parse(text = "0.3333+(0.3321*(x-4.1155))+(-0.2713*((x-4.1155)^2))+(0.05*((x-4.1155)^3))" ), list(x = x_sp3))

x_sp4 <- seq(from = 5.1111, to = 6.0537, by = 0.01)
fx_sp4 <- eval(parse(text = "0.4444+(-0.0594*(x-5.1111))+(-0.1219*((x-5.1111)^2))+(0.329*((x-5.1111)^3))"), list(x = x_sp4))

x_sp5 <- seq(from = 6.0537, to = 6.2298 , by = 0.01)
fx_sp5 <- eval(parse(text = "0.5556+(0.5876*(x-6.0537))+(0.8083*((x-6.0537)^2))+(-3.1946*((x-6.0537)^3))"), list(x = x_sp5))

x_sp6 <- seq(from = 6.2298 , to = 11.6898, by = 0.01)
fx_sp6 <- eval(parse(text = "0.6667+(0.5751*(x-6.2298))+(-0.8794*((x-6.2298)^2))+(0.1425*((x-6.2298)^3))"), list(x = x_sp6))

x_sp7 <- seq(from = 11.6898, to = 11.7199 , by = 0.01)
fx_sp7 <- eval(parse(text = "0.7778+(3.7123*(x-11.6898))+(1.454*((x-11.6898)^2))+(-71.7894*((x-11.6898)^3))"), list(x = x_sp7))

x_sp8 <- seq(from = 11.7199 , to = 12.7634 , by = 0.01)
fx_sp8 <- eval(parse(text = "0.8889+(3.6047*(x-11.7199))+(-5.0286*((x-11.7199)^2))+(1.6063*((x-11.7199)^3))"), list(x = x_sp8))


Graf_2 <- ggplot(data = Table_Graf2,aes(x = xi, y = Px))+
  geom_line(colour = "pink", size = 1)+
  geom_point(aes(xi,Fx),colour = "Green", pch = 17 )+
  geom_point(aes(x= 12.2416 , y=Lagrange(12.2416,x3,Fx3)) , colour = "Pink", shape = 15) +
  geom_line(aes(x = x_sp1, y = fx_sp1), colour = 'blue') +
  geom_line(aes(x = x_sp2, y = fx_sp2), colour = 'blue') +
  geom_line(aes(x = x_sp3, y = fx_sp3), colour = 'blue') +
  geom_line(aes(x = x_sp4, y = fx_sp4), colour = 'blue') +
  geom_line(aes(x = x_sp5, y = fx_sp5), colour = 'blue') +
  geom_line(aes(x = x_sp6, y = fx_sp6), colour = 'blue') +
  geom_line(aes(x = x_sp7, y = fx_sp7), colour = 'blue') +
  geom_line(aes(x = x_sp8, y = fx_sp8), colour = 'blue') +
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  geom_point(aes(x = 12.2416, Interpolacion_en_Pto), colour = "blue", shape = 10) +
  scale_x_continuous(name = "x", limits = c(0,13)) +
  ggtitle("Comparacion")+
  xlab("Eje x")+ 
  ylab("Eje y")+
  theme_light()

Graf_2


#### EJERCICIO 4 ####
set.seed(900071)

#4.1 ----
#Parámetros

P0<-73
mu<-0.13  #retorno esperado
sigma<-0.1  #volatilidad
T<- 5/12  #seis meses (año seria 1)
n<- T / dt  #número de time-steps
dt<-1/252   #tamaño de cada time-step (delta t)

m<- 1233 #cantidad de caminos de precios (cantidad de simulaciones)
Pt<- matrix(NA,nrow=m,ncol=n+1) #matriz de camino de precios con m filas y n+1 precios (porque arranca de 0)
Pt[,1]<- P0 #para todas las filas, que en la primer columna tenga P0

for (i in 1:m) { #creo un bucle for que barre todas las simulaciones
  for (t in 2:(n+1)) {
    #el 2 porque arrance de la fila 2 porque 1 era P0 (precio inicial)
    Pt[i,t]<-Pt[i,t-1]*exp((mu-0.5*sigma^2)*dt+sigma*sqrt(dt)*rnorm(1))
  } #el precio en el momento T es igual al preico en t-1 : y toda la formula
}#Para generar el epsilon de la formula hago Rnorm que genera un numero aleatorio (uno), porque
#quiero solo UN numero aleatorio

Pt

#Esperanza
E_Pt <- mean(Pt[,ncol(Pt)])
E_Pt
#Desvio Estandar
SD_Pt <- sd(Pt[,ncol(Pt)])
SD_Pt

#Histogrma
hist(Pt[,ncol(Pt)])
title("Histograma de los precios Finales, Pt")

#4.2 y 4.3 ----
#Probabilidad de que Pt este entre 38 y 73

df_pt <- data.frame(Pt)
ncol(Pt)

(length(df_pt[df_pt$X106 > 38, "X106" ]) / length(Pt[,ncol(Pt)])) - (length(df_pt[df_pt$X106 < 73, "X106" ]) / length(Pt[,ncol(Pt)]))

#Probabilidad de que Pt sea mayor a E(Pt) = 77.02

length(df_pt[df_pt$X106 > E_Pt, "X106" ]) / length(Pt[,ncol(Pt)])



#### EJERCICIO 5 ####

#Funcion que se va a aproximar
f <- function(t,y) {(cos(y)/ t^(1.69)) + t/y^3}

#Extremos y valor inicial
a=4
b= 5
alpha = 0.46

#Cantidad de Iteraciones
N = 10

#5.1 ----

Metodo_Euler <- function(a,b,N,alpha){
  
  h= (b-a)/N
  t= a
  
  w = matrix(rep(NA, N+1), ncol =1)
  w[1] = alpha
  
  for (i in 1:N) {
    w[i+1]= w[i] + h *f(t,w[i])
    t = a + i*h
  }
  t = matrix(seq(a,b,h), ncol = 1)
  resultado <- matrix(c(t,w), ncol = 2 , byrow = FALSE )
  colnames(resultado)= c('t', 'w')
  return(resultado)
}

Y_t_Euler <- Metodo_Euler(a,b,N,alpha)
Y_t_Euler


#5.2 ----
RK4 <- function(a,b,N,alpha){
  
  h= (b-a) / N
  w = matrix(rep(NA, N+1), ncol =1)
  w[1] = alpha
  t = a
  
  for (i  in 1:N) {
    
    K1 = h * f(t , w[i])
    K2 = h * f(t + h/2 , w[i] + K1/2)
    K3 = h * f(t + h/2 , w[i] + K2/2)
    K4 = h * f(t + h , w[i] + K3)
    
    w[i+1] = w[i] + (K1 + 2*K2 + 2*K3 + K4)/6
    t = a + i*h
  }
  t = matrix(seq(a,b,h), ncol = 1)
  resultado <- matrix(c(t,w), ncol = 2 , byrow = FALSE )
  colnames(resultado)= c('t', 'w')
  return(resultado)
}

Y_t_RungeKutta <- RK4(a,b,N,alpha)
Y_t_RungeKutta

#5.3_ Grafico ----
df_Euler <-data.frame(Y_t_Euler)

df_RK <- data.frame(Y_t_RungeKutta)


#Grafico
plot(df_Euler$t , df_Euler$w,col ="black", type = "l", ylab = "y(t)", xlab = "t", xlim = c(4,5))
lines(df_RK$t , df_RK$w , col="red")
points(x = df_Euler$t, y = df_Euler$w, cex = 0.7)
points(x= df_RK$t, y = df_RK$w, cex= 0.7 )
title("Solucion del problema : y' = (cos(y)/ (t^1.69)) + t/(y^3)")
legend( 'bottomright', legend = c("Euler N=45", "RK4 N = 45"),
        col = c("black", 'red'),
        lty = c(1,1))

#5.4 ----
# Los métodos de Taylor de orden superior (como Runge-Kutta) tienen error de truncamiento de orden alto, lo que hace que aproximen mejor,
# pero es necesario conocer las derivadas. En cambio, el método de Euler, lo hace mas "económico" porque no es necesario conocer las derivadas
# pero esto lo hace menos exacto.




knitr::opts_chunk$set(echo = FALSE)
write_matex2 <- function(x) {
  begin = "\\begin{bmatrix}"
  end = "\\end{bmatrix}"
  X =
    apply(x, 1, function(x) {
      paste(
        paste(x, collapse = "&"),
        "\\\\"
      )
    })
  paste(c(begin, X, end), collapse = "")
}

NroReg = 900071
d1 = floor( NroReg/10^5 )
d2 = floor( (NroReg - d1*10^5)/10^4 )
d3 = floor( (NroReg - d1*10^5 - d2*10^4)/10^3 )
d4 = floor( (NroReg - d1*10^5 - d2*10^4 - d3*10^3)/10^2 )
d5 = floor( (NroReg - d1*10^5 - d2*10^4 - d3*10^3 - d4*10^2)/10^1)
d6 = floor( (NroReg - d1*10^5 - d2*10^4 - d3*10^3 - d4*10^2 - d5*10^1)/10^0)


# Resolución de Ecuaciones: Secante. (24 puntos)
#Considere la siguiente ecuación: $2cos(x)=e^{-10/x}$.

# Graficar función

#Plantee la ecuación de la forma $f(x)=0$ y grafique la función en el intervalo $[0;20]$ de manera tal que pueda identificar todas las soluciones de la ecuación en el intervalo.

f<- function(x){
  return(2*cos(x)-exp(-10/x))
}

x<- seq(0,20, by= 0.1)
fx <- f(x)
df <- data.frame(x,fx)
gg_fx <- ggplot(data = df, aes(x=x, y=fx))+
  geom_line(linetype=1,colour="darkblue") + #Agrego la geometria
  geom_hline(yintercept = 0,linetype=1) + #Agrego linea que cruza en y=0
  geom_vline(xintercept = 0,linetype=1)+
  scale_x_continuous(name = "x", breaks = seq(0,20, by = 1)) # Cambio ticks en eje X

gg_fx


### Hallar raíces

#Utilizando el algoritmo del punto 1.1, halle todas las raíces identificadas en el punto 1.2.

Secante <- function(f,p0,p1,TOL,N){
  i <- 2
  q0 <- f(p0)
  q1 <- f(p1)
  while (i <= N){ #Cambio el == por <=
    p = p1 - q1*(p1-p0)/(q1-q0) #Modifico (q1-q0)/(p1-p0)
    if (abs(p-p1) < TOL){ #q-q1
      return(p)
    }
    i = i + 1
    p0 = p1 #p
    q0 = q1
    p1 = p #p0
    q1 = f(p) #f(q0)
  }
  return(paste('El metodo fallo luego de ', N, ' iteraciones')) #n por N
} 

print(paste("Raiz 1: ",Secante(f,1,2,10^-3,100),
            "Raiz 2: ", Secante(f,4.5,5,10^-3,100),
            "Raiz 3: ", Secante(f,7.5,8,10^-3,100),
            "Raiz 4: ", Secante(f,11,11.5,10^-3,100),
            "Raiz 5: ",Secante(f,13.5,14,10^-3,100),
            "Raiz 6: ", Secante(f,17,18,10^-3,100)))



## Iteraciones
#Tome el algoritmo del punto 1.1 (copie y pegue) y agregue las líneas de código que considere necesarias para poder visualizar (imprimir) cada iteración del algoritmo. Una vez editado el algoritmo, imprima `r nit` iteraciones del algoritmo iniciando en $x_0=`r x0`$ y $x_1=`r x1`$. ¿A cuál de las raíces convergeria el algortimo en este caso?
set.seed(NroReg)
nit = sample(c(5,7,9), size =1)
x0 = 20-round(rnorm(1,5,2),2)
x1 = x0 + 1

Secante <- function(f,p0,p1,TOL,N){
  i <- 2
  q0 <- f(p0)
  q1 <- f(p1)
  while (i <= N){ #Cambio el == por <=
    p = p1 - q1*(p1-p0)/(q1-q0) #Modifico (q1-q0)/(p1-p0)
    print(paste("N: ", i , "| p: " ,p))
    if (abs(p-p1) < TOL){ #q-q1
      return(p)
    }
    i = i + 1
    p0 = p1 #p
    q0 = q1
    p1 = p #p0
    q1 = f(p) #f(q0)
  }
  return(paste('El metodo fallo luego de ', N, ' iteraciones')) #n por N
} 

Secante(f,x0,x1,10^-3,nit)


# Resolución de Ecuaciones: Punto Fijo. (24 puntos)

#Para este ejercicio, considere la función $h(x)=x^2 \times cos(x)$.

#El metodo del punto fijo consiste en redefinir una funcion igualada a cero(busqueda de raices), en una en la que la variable quede
#despejada. De esta forma el metodo utiliza una aproximacion inicial (po), le aplica la nueva funcion y chequea si esta (f(po))
#difiere en un valor tolerable con p0 (o la iteracion anterior). Si son distintos, se toma como p1 = f(po) y se busca f(p1). Y asi 
#sucesivamente hasta que |f(pn) - pn| < tolerancia. Va construyendo una especie de telaraña en el grafico


f<- function(x){
  return(x^2 *cos(x))
}

g <- function(X){
  return(x - x^2 *cos(x))
}

x<- seq(-5,5, by=0.5)
gx <- g(x)
df <- data.frame(x,gx)

gg_gx = ggplot(data = df)+
  geom_line(linetype= 1, colour ="blue")+
  aes(x=x, y =gx)+
  geom_vline(xintercept = 0, linetype = 1) +
  geom_hline(yintercept = 0, linetype = 1)+
  geom_abline(intercept = 0,slope=1,linetype=2)+
  scale_x_continuous(name ="x", breaks = seq(-5,5,by=0.5))

gg_gx
                 

##para derivar
gd <- expression(x - x^2 *cos(x))
dg <- D(gd,"x")
dg

gprima <- function(x){
  return(1 - (2 * x * cos(x) - x^2 * sin(x)))
}

# Intervalo [-5;-4.5]
x <- seq(-5, -4.5, by =0.1)
#Genero los puntos
gprimax <- gprima(x)
#Creo un data frame con los x e y
df <- data.frame(x, gprimax)
#Lo imprimo
df
#No cumple con |g´(x)|<1

# Intervalo [-2.5;-1.5]
x <- seq(-2, -1.5, by =0.1)
#Genero los puntos
gprimax <- gprima(x)
#Creo un data frame con los x e y
df <- data.frame(x, gprimax)
#Lo imprimo
df
#No cumple con |g´(x)|<1
 
# Intervalo [-0.1;0.1]
x <- seq(-0.1, 0.1, by =0.01)
#Genero los puntos
gprimax <- gprima(x)
#Creo un data frame con los x e y
df <- data.frame(x, gprimax)
#Lo imprimo
df

#SI cumple con |g´(x)|<1

# Intervalo [1.5;2]
x <- seq(1.5, 2, by =0.1)
#Genero los puntos
gprimax <- gprima(x)
#Creo un data frame con los x e y
df <- data.grame(x, gprimax)
#Lo imprimo
df
#No cumple con |g´(x)|<1

# Intervalo [4.5;5]
x <- seq(4.5, 5, by =0.1)
#Genero los puntos
gprimax <- gprima(x)
#Creo un data frame con los x e y
df <- data.frame(x, gprimax)
#Lo imprimo
df
#No cumple con |g´(x)|<1

#Método de punto fijo
PuntoFijo <- function(p0, n = 100, tol){
  #Donde p0 es la aproximación inicial
  #El número máximo de iteraciones n viene por default en 100
  #Y tol es la toleranacia al error
  
  #Instancio las listas vacias
  lista_p <- c(NULL)
  lista_gp <- c(NULL)
  
  for (i in 1:n) {
    #Calculo p
    p <- g(p0)
    
    lista_p[i] <- p0
    lista_gp[i] <- p
    
    if(abs(p-p0) <= tol){
      #Creo un data frame con las listas
      datos <- data.frame(lista_p, lista_gp)
      colnames(datos) <- c("P", "G(P)")
      print(datos)
      return(p)
    }
    
    p0 <- p
  }
  
  #En el caso de que falle el método
  return(paste('El método falla luego de: ', n, ' iteraciones'))
}

raiz <- PuntoFijo(p0 = 0.1, tol = 0.00001)
print(paste("La raiz es: ", raiz))



##Ejercicio 2

h <- function(x){
  return(x^2 * cos(x))
}

#2.1



#2.2  -----
x <- seq(1,4*pi, by= 0.5)
fx <- h(x)
df <- data.frame(x,fx)

grafico = ggplot(data=df) + 
  geom_line(linetype= 1, colour ="blue")+
  aes(x=x, y =fx)+
  geom_vline(xintercept = 0, linetype = 1) +
  geom_hline(yintercept = 0, linetype = 1) 
#tiene 4 raices, agrego escala para diferenciar donde estan
grafico = grafico +
  scale_x_continuous(name = "x", breaks = seq(1,4*pi, by = 1))
##Raiz 2 entre 4.5 y 5, Raiz 3 entre 7.5 y 8, Raiz 4 entre 10.5 y 11.5(en 11)

#Hago zoom al grafico para ver la primer Raiz
grafico = grafico +
  scale_x_continuous(name = "x", limits = c(1,2), breaks = seq(0,2, by = 0.1)) +
  scale_y_continuous(name = "h(x)", limits = c(-5,5), breaks = seq(-5,5, by = 0.5))
#Raiz 1 entre 1.5 y 1.6

#Saco las raices por el metodo

Funcion <- function(x){
  return(x^2 * cos(x))
}

Falsa_Posicion <- function(p0, p1 , tol , n){
  i=2
  q0=Funcion(p0)
  q1=Funcion(p1)
  while(i<=n){
    p=p1 - q1*(p1 - p0)/(q1 - q0)
    if(abs(p-p1) < tol){
      return(p)
    }
    i= i+1
    q = Funcion(p)
    if(q*q1 <0){
      p0=p1
      q0=q1
    }
    p1=p
    q1=q
  }
  return(paste("El metodo fallo luego de ",n," iteraciones"))
}

##Raiz 1: 1.570793
Falsa_Posicion(1.5,1.6,10^-4,100)
#Raiz 2: 4.712387
Falsa_Posicion(4.5,5,10^-4,100)
#Raiz 3: 7.853981
Falsa_Posicion(7.5,8,10^-4,100)
#Raiz 4: 10.99557
Falsa_Posicion(10.5,11.5,10^-4,100)

#2.3 ----
x0 =4
x1= 6
iteraciones = 5
Falsa_Posicion(x0,x1,10^-2,iteraciones)

#El metodo converge a la Raiz 4.71506, Pero para que pueda converger es necesario reducir
#el nivel de tolerancia al error. Se ve que esta raiz difiere por muy poco de la segunda raiz calculada en el ejercicio anterior


#2.4 ----
grafico = ggplot(data=df) + 
  geom_line(linetype= 1, colour ="blue")+
  aes(x=x, y =fx)+
  geom_vline(xintercept = 0, linetype = 1) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_point(aes(x =Falsa_Posicion(1.5,1.6,10^-4,100),
                 y = 0),pch = 16, col = "red")+
  geom_point(aes(x =Falsa_Posicion(4.5,5,10^-4,100),
                 y = 0),pch = 16, col = "red")+
  geom_point(aes(x =Falsa_Posicion(7.5,8,10^-4,100),
                 y = 0),pch = 16, col = "red")+
  geom_point(aes(x =Falsa_Posicion(10.5,11.5,10^-4,100),
                 y = 0),pch = 16, col = "red")

  
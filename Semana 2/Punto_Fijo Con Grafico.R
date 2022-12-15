Funcion <- function(x){
  f<- x^3 - x - 1
  return(f)  
}

g <- function(x){ # Funcion g(x) para la iteracion de punto fijo
  gn <- x^3 -1 
  return(gn)
}

gprima <- function(x){
  gn<- 3*x^2
  return(gn)
}

gprima(1)  #Pendiente > 1, no se puede hacer el metodo. Buscar otro g(x)


Punto_Fijo <- function (P0,TOL,N){
  i=1
  while(i<=N){
    p <- g(P0)
    if(abs(p -P0) < TOL){
      return(p)
    }
    i=1+i
    P0=p
  }
  return(paste("El metodo fracaso luego de ",N," iteraciones"))
}


x <- seq(0,5,by= 0.1)
gx <- g(x)
df <- data.frame(x,gx)

plot(x,gx)

grafico = ggplot(data=df) + 
  geom_line(linetype= 1, colour ="blue")+
  aes(x=x, y =gx)+
  geom_vline(xintercept = 0, linetype = 1) +
  geom_hline(yintercept = 0, linetype = 1)+
  geom_abline(intercept = 0,slope=1,linetype=2)+
  scale_x_continuous(name = "x", limits = c(1,2), breaks = seq(1,2, by = 0.1))+
  scale_y_continuous(name = "g(x)", limits = c(1,2), breaks = seq(1,2, by = 0.1))
#Voy achicando o agrandando la escala hasta que "Mapee" la funcion

grafico

Punto_Fijo(1.3,10^-2,100)

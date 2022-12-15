##EJERCICIO 1
#A -----

#funcion a integrar
f1 <- function(x){
  sqrt(x+5) * sin(x)
}

a = 2
b = 6
n <- 10000

U <- a +(b-a) * runif(n)

Altura.Promedio <- 1/n * sum(Fa(U))

Ancho.Base <- b - a

Integral.MC <- Altura.Promedio * Ancho.Base
Integral.MC

Sf.MC <- sqrt(1/(n-1) * sum((Fa(U)*(b-a) - Integral.MC)^2))

Error.MC <- Sf.MC / sqrt(n)
Error.MC


# Grafico Funcion
xi <- seq(from=0,to=10,length.out=100)
Px <- rep(NA,100)
for(i in 1:100){
  Px[i]<-f1(xi[i])
}

DF_Table <- data.frame(xi,Px)


#Area a sombrear:
xii <- seq(from=2,to=6,length.out=100)
Pxx <- rep(NA,100)
for(i in 1:100){
  Pxx[i]<-f1(xii[i])
}
DF_Table_sombreado <- data.frame(xii,Pxx)

Graf <- ggplot(data = DF_Table,aes(x = xi, y = Px))+
  geom_line(colour = "green", size = 1)+
  geom_area(data = DF_Table_sombreado, aes(x=xii, y=Pxx))+
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  ggtitle("Integral.MC")+
  xlab("Eje x")+ 
  ylab("Eje y")+
  theme_light()

Graf


#B ----

#Funcion de densidad de la normal
mu = 7
o = 5
f2 <- function(x){1/ sqrt((2*pi*o^2)) * exp(-(x-mu)^2/(2*o))}

n<-10000
a=5
b=8

U<- a + (b-a) * runif(n)

Altura.Promedio <- 1/n * sum(f2(U))
Ancho.Base <- b - a

Integral.MC <- Altura.Promedio * Ancho.Base
Integral.MC

Sf.MC <- sqrt(1/(n-1) * sum((f2(U)*(b-a) - Integral.MC)^2))

Error.MC <- Sf.MC / sqrt(n)
Error.MC

# Grafico Funcion
xi <- seq(from=0,to=15,length.out=100)
Px <- rep(NA,100)
for(i in 1:100){
  Px[i]<-f2(xi[i])
}

DF_Table <- data.frame(xi,Px)


#Area a sombrear:
xii <- seq(from=5,to=8,length.out=100)
Pxx <- rep(NA,100)
for(i in 1:100){
  Pxx[i]<-f2(xii[i])
}
DF_Table_sombreado <- data.frame(xii,Pxx)

Graf_2 <- ggplot(data = DF_Table,aes(x = xi, y = Px))+
  geom_line(colour = "green", size = 1)+
  geom_area(data = DF_Table_sombreado, aes(x=xii, y=Pxx))+
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  scale_x_continuous(name = "xi", limits = c(0,15), breaks = seq(0,15, by = 1))+
  ggtitle("Integral.MC")+
  xlab("Eje x")+ 
  ylab("Eje y")+
  theme_light()

Graf_2



#C ----

f3 <- function(x){x^3 + 4*x^2 + 2}

n<-10000
a=-2
b=5

U<- a + (b-a) * runif(n)

Altura.Promedio <- 1/n * sum(f3(U))
Ancho.Base <- b - a

Integral.MC <- Altura.Promedio * Ancho.Base
Integral.MC

Sf.MC <- sqrt(1/(n-1) * sum((f3(U)*(b-a) - Integral.MC)^2))

Error.MC <- Sf.MC / sqrt(n)
Error.MC

# Grafico Funcion
xi <- seq(from=-10,to=10,length.out=100)
Px <- rep(NA,100)
for(i in 1:100){
  Px[i]<-f3(xi[i])
}

DF_Table <- data.frame(xi,Px)


#Area a sombrear:
xii <- seq(from=-2,to=5,length.out=100)
Pxx <- rep(NA,100)
for(i in 1:100){
  Pxx[i]<-f3(xii[i])
}
DF_Table_sombreado <- data.frame(xii,Pxx)

Graf_3 <- ggplot(data = DF_Table,aes(x = xi, y = Px))+
  geom_line(colour = "green", size = 1)+
  geom_area(data = DF_Table_sombreado, aes(x=xii, y=Pxx))+
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  scale_x_continuous(name = "xi", limits = c(-5,7), breaks = seq(-5,7, by = 1))+
  scale_y_continuous(name = "Px", limits = c(-100,500), breaks = seq(-100,500, by = 50))+
  ggtitle("Integral.MC")+
  xlab("Eje x")+ 
  ylab("Eje y")+
  theme_light()

Graf_3


#D ----
f4 <- function(x){x * log(x^3) + 12 *cos(x)}

n<-10000
a=12
b=20

U<- a + (b-a) * runif(n)

Altura.Promedio <- 1/n * sum(f4(U))
Ancho.Base <- b - a

Integral.MC <- Altura.Promedio * Ancho.Base
Integral.MC

Sf.MC <- sqrt(1/(n-1) * sum((f4(U)*(b-a) - Integral.MC)^2))

Error.MC <- Sf.MC / sqrt(n)
Error.MC

# Grafico Funcion
xi <- seq(from=5,to=25,length.out=100)
Px <- rep(NA,100)
for(i in 1:100){
  Px[i]<-f4(xi[i])
}

DF_Table <- data.frame(xi,Px)


#Area a sombrear:
xii <- seq(from=12,to=20,length.out=100)
Pxx <- rep(NA,100)
for(i in 1:100){
  Pxx[i]<-f4(xii[i])
}
DF_Table_sombreado <- data.frame(xii,Pxx)

Graf_4 <- ggplot(data = DF_Table,aes(x = xi, y = Px))+
  geom_line(colour = "green", size = 1)+
  geom_area(data = DF_Table_sombreado, aes(x=xii, y=Pxx))+
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  ggtitle("Integral.MC")+
  xlab("Eje x")+ 
  ylab("Eje y")+
  theme_light()

Graf_4

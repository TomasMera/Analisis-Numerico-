### Ejercicio 1

##Funcion : nt=n0*exp(a*t)+v/a *(exp(a*t - 1))
#n0 = 1000
#t=2
#n2= 1768
#v=350

#reemplazo los valores e igualo a 0

f <- function(x){
  return(1000*exp(x*2) + (350/x)*(exp(x*2)-1) -1768)
}


##1.1 ----
x<- seq(-1,1, by=0.01)
fx <- f(x)
df <- data.frame(x,fx)


graf_a = ggplot(data = df)+
  aes(x=x, y=fx)+
  geom_line(linetype= 1, colour ="blue")+
  geom_vline(xintercept = 0, linetype = 1) +
  geom_hline(yintercept = 0, linetype = 1)+
  scale_y_continuous(name = "fx", limits = c(-1000,1000))+
  scale_x_continuous(name = "x",limits= c(-1,1), breaks = seq(-1,1, by= 0.1))
graf_a

#Observo que entre 0 y 0.1 hay una raiz.

##

#1.2----
polinomio = function (x){
  f = 1000*exp(x*2) + (350/x)*(exp(x*2)-1) -1768
  return (f)
}


Raiz_Biseccion <- function(a,b,TOL,N){
  i=1
  FA = polinomio(a)
  
  while(i<=N){
    p = a+(b-a)/2
    FP = polinomio(p)
    if(FP == 0 | (b-a)/2 < TOL){
      return(p)
    }
    i=i+1
    if(FA*FP > 0 ){
      a=p
      FA = FP
    }else{
      b=p
    }
  }
  return(paste("El metodo fracaso despues de ", N," iteraciones"))
}
  

a<- Raiz_Biseccion(0,0.1,10^-4,100)
a

#1.3----

#n(100)=800
#t=100
#v=350

polinomio <- function(x){
  return(1000*exp(x*100) + (350/x)*(exp(x*100)-1) -800)
}

x<- seq(-1,1, by=0.01)
fx <- polinomio(x)
df <- data.frame(x,fx)


graf_a = ggplot(data = df)+
  aes(x=x, y=fx)+
  geom_line(linetype= 1, colour ="blue")+
  geom_vline(xintercept = 0, linetype = 1) +
  geom_hline(yintercept = 0, linetype = 1)+
  scale_y_continuous(name = "fx", limits = c(-1000,1000))+
  scale_x_continuous(name = "x",limits= c(-1,1), breaks = seq(-1,1, by= 0.1))
graf_a
#Veo que la raiz esta entre -0.5 y -0.4

a<- Raiz_Biseccion(-0.4,-0.5,10^-4,100)
a


#1.4 ----
Raiz_Biseccion <- function(a,b,TOL,N){
  i=1
  FA = polinomio(a)
  
  while(i<=N){
    p = a+(b-a)/2
    FP = polinomio(p)
    print(paste("N: ", i , "| p: " ,p))
    if(FP == 0 | (b-a)/2 < TOL){
      return(p)
    }
    i=i+1
    if(FA*FP > 0 ){
      a=p
      FA = FP
    }else{
      b=p
    }
  }
  return(paste("El metodo fracaso despues de ", N," iteraciones"))
}

a<- Raiz_Biseccion(-0.4,-0.5,10^-10,100)
a

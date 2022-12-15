#COTAS DE ERROR INTEGRALES

######Newton Cotes Cerradas

##Trapecio n=1
# h=(b-a)/n
# ((h^3)/12)*f(2)(max)

##Simpson n=2
# h=(b-a)/n
# ((h^5)/90)*f(4)(max)

##Simpson Tres Octavos n=3
# h=(b-a)/n
# ((3*h^5)/80)*f(4)(max)

##Newton Cotes Cerrada n=4
# h=(b-a)/n
# ((8*h^7)/945)*f(5)(max)


######Newton Cotes Abiertas

##Punto Medio n=0
#h=(b-a)/(n+2)
# ((h^3)/3)*f(2)(max)

##Newton Cotes n=1
#h=(b-a)/(n+2)
# ((3*h^3)/4)*f(2)(max)

##Newton Cotes n=2
#h=(b-a)/(n+2)
# ((14*h^5)/45)*f(4)(max)

##Newton Cotes n=3
#h=(b-a)/(n+2)
# ((95*h^5)/144)*f(4)(max)


#INTEGRACION COMPUESTA

#####SIMPSON COMPUESTO

#Algoritmo

alim<-5   #cargar límite inferior intervalo
blim<-15  #cargar límite superior intervalo

d1sc<-D(expression("FUNCION EXPRESADA"),"x") #cargar función que desea derivar
d2sc<-D(d1sc,"x")
d3sc<-D(d2sc,"x")
d4sc<-D(d3sc,"x")

d4sc

derivadasc4<-function(x){
  f<-"COPIAR Y PEGAR LA FUNCION d4"
  return(f)
}

optimosc<-optimize(derivadasc4,c(alim,blim),maximum = T)
optsc<-abs(optimosc$objective)
optsc

#sacar n

errordeseadosc<-0.001 #cargar el error que pida

cant.itersc<-(blim-alim)/((errordeseadosc*180)/((blim-alim)*optsc))^(1/4)
cant.itersc

#Error con n deseado

error.con.nsc<-((blim-alim)/180)*(((blim-alim)/cant.itersc)^4)*optsc
error.con.nsc

#Error sin n deseado
nsc<-100
error.sin.nsc<-((blim-alim)/180)*(((blim-alim)/nsc)^4)*optsc
error.sin.nsc



#Cota del ERROR SIMPSON COMPUESTO

alim<-12.68   #cargar límite inferior intervalo
blim<-20.42  #cargar límite superior intervalo

X <- seq(from = alim, to = blim , by = 0.001)
fx <- eval(D(D(D(D(expression( "Funcion a derivar"), "x"),"x"), "x"), "x"))

n <- 1000
h <- (blim - alim) / n

ERROR <- ((blim - alim)/180) * (h^4) * max(abs(fx))
ERROR





#####TRAPECIO COMPUESTO

#Algoritmo

alim<-5   #cargar límite inferior intervalo
blim<-15  #cargar límite superior intervalo

d1tc<-D(expression("FUNCION EXPRESADA"),"x") #cargar función que desea derivar
d2tc<-D(d1tc,"x")

d2tc

derivadatc2<-function(x){
  f<-"COPIAR Y PEGAR LA FUNCION d4"
  return(f)
}

optimotc<-optimize(derivadatc2,c(alim,blim),maximum = T)
opttc<-abs(optimotc$objective)
opttc

#sacar n

errordeseadotc<-0.001 #cargar el error que pida

cant.itertc<-(blim-alim)/((errordeseadotc*12)/((blim-alim)*opttc))^(1/2)
cant.itertc

#Error con n deseado

error.con.ntc<-((blim-alim)/12)*(((blim-alim)/cant.itertc)^2)*opttc
error.con.ntc

#Error sin n deseado
ntc<-100
error.sin.ntc<-((blim-alim)/12)*(((blim-alim)/ntc)^2)*opttc
error.sin.ntc


### Cota del errror TRAPECIO COMPUESTO ###

alim<-12.68   #cargar límite inferior intervalo
blim<-20.42  #cargar límite superior intervalo

X <- seq(from = alim, to = blim , by = 0.001)
fx <- eval(D(D(expression("Funcion a derivar"), "x"),"x"))

n <- 1000
h <- (blim - alim) / n

ERROR <- ((blim - alim)/12) * (h^2) * max(abs(fx))
ERROR



#####PUNTO MEDIO COMPUESTO

#Algoritmo

alim<-5   #cargar límite inferior intervalo
blim<-15  #cargar límite superior intervalo

d1pm<-D(expression("FUNCION EXPRESADA"),"x") #cargar función que desea derivar
d2pm<-D(d1pm,"x")

d2pm

derivadapm2<-function(x){
  f<-"COPIAR Y PEGAR LA FUNCION d4"
  return(f)
}

optimopm<-optimize(derivadapm2,c(alim,blim),maximum = T)
optpm<-abs(optimopm$objective)
optpm

#sacar n

errordeseadopm<-0.001 #cargar el error que pida

cant.iterpm<-((blim-alim)/((errordeseadopm*6)/((blim-alim)*optpm))^(1/2))+2
cant.iterpm

#Error con n deseado

error.con.npm<-((blim-alim)/6)*(((blim-alim)/(cant.iterpm+2))^2)*optpm
error.con.npm

#Error sin n deseado
npm<-100
error.sin.npm<-((blim-alim)/6)*(((blim-alim)/(npm+2))^2)*optpm
error.sin.npm

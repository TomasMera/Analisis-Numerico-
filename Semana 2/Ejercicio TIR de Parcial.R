rm(list=ls())
options(scipen = 999, digits = 10)
graphics.off()


#1) Inputs ----
cupon = 0.16 #Mi TNA
Amortizaciones = matrix(c(0.5,1,1.5,2, #momentos expresados en años cuando ocurres los ff
                          8,8,8,106),ncol=2,dimnames=list(NULL,c("t","Amort"))) 
m = 2 #numero de pagos de interes por anio.
#View(Amortizaciones)

#2) Marcha Progresiva ----
#Numero de flujos
n = max(Amortizaciones[,1]*m) #el n de flujos de efectivos es el max (tomo primer colunma de la matriz de amortizaciones)
#Tendre 8 flujos de efecctivo
Marcha = matrix(rep(NA,(n+1)*5),ncol=5)
#(n+1)*5 la cantidad de veces que quiero NA
colnames(Marcha)=c("t","Saldo","Amort","Int","C.F.")

#Completo las columnas
#Columna "t"
Marcha[,"t"] = seq(from=0, to=n/m, by= 1/m)

#Columna "Amort" y "Saldo"
k=1
Marcha[1,"Saldo"] = sum(Amortizaciones[,"Amort"])
for (i in 1:(n+1)){
  if(Marcha[i,"t"]== Amortizaciones[k,"t"]){
    Marcha[i,"Amort"]=Amortizaciones[k,"Amort"]
    if(i>1){Marcha[i,"Saldo"] = Marcha[i-1,"Saldo"] - Amortizaciones[k,"Amort"]}
  k=k+1
  }else{ #si no concide en el momento del tiempo
    Marcha[i,"Amort"] = 0
    if(i>1){Marcha[i,"Saldo"]= Marcha[i-1,"Saldo"]}
  } #el saldo del momento i coincide con el saldo de i-1
}

#Columna "Int"
Marcha[1,"Int"] = 0
for(i in 2:(n+1)){
  Marcha[i,"Int"] = Marcha[i-1,"Saldo"]*cupon/m
}
Marcha[,"C.F."] = Marcha[,"Amort"]+Marcha[,"Int"]

View(Marcha)

#3) Funcion de Precio ----
Precio <- function(tasa,t,CF){
  n = length(CF)
  P = 0
  for (i in 1:n){
    P = P + CF[i]*(1+tasa)^-t[i]
  }
  return(P)
}

Precio(cupon, Marcha[,"t"], Marcha[,"C.F."])
tasa1 = (1+cupon/m)^m-1
Precio(tasa1,Marcha[,"t"],Marcha[,"C.F."])


tasas <- seq(from = 0, to = 1, by = 0.001)
P <-  Precio(tasas, Marcha[,"t"], Marcha[,"C.F."])

#Para graficar: 
DF_Graph1 <- data.frame(tasas,P)

ggplot(DF_Graph1, aes(tasas,P))+
  geom_line(colour="red") + 
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  theme_minimal()+
  theme(text = element_text(size = 16), element_line(size = 0.4))

#4) Calculo de la TIR ----
PM = 67.90
#Cual es la tasa que hace que el precio sea 
#suponiendo que el precio es 80, la tasa será del 20%
f <- function(r){
  return(-PM+Precio(r,Marcha[,"t"], Marcha[,"C.F."]))}

DF_Graph2 <- data.frame(tasas,f(tasas))

ggplot(DF_Graph2, aes(tasas,f(tasas)))+
  geom_line(colour="red") + 
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  theme_minimal()+
  theme(text = element_text(size = 16), element_line(size = 0.4))


# 5) Metodo de Biseccion para la resolucion del Ejerc TIR ----
Raiz_Biseccion = function(a,b,Tol,N){ #Tol is tolerance level and N number of iterations
  #Step 1 
  i = 1 
  FA = polinomio(a)
  #Step 2
  while (1 <= N){ #la condicion esta entre parentesis.Se ejecuta mientras 1 sea menor o igual que n.
    #Step 3
    p = a + (b-a)/2 #punto medio
    FP = polinomio (p) #calcular el polinomio en ese puntp p
    #Step 4
    if (FP==0 | (b-a)/2 < Tol){
      p<- c(p,i) #Tira la cantidad de iteraciones
      return (p)
    }
    #Step 5
    i = i + 1
    #Step 6
    if (FP * FA> 0){
      a = p #a pasa a ser el punto medio
      FA = FP #F(a) pasa a ser la imagen de p
    } else { #si no se cumple
      b = p #b es el punto medio del intervalo
    }
  }
  #Step 7
  return (paste("El metodo falló luego de ", N, "iteraciones"))
}

polinomio = function (r){
  f = -PM+Precio(r,Marcha[,"t"], Marcha[,"C.F."])
  return (f)
}

Raiz_Biseccion(0.7,0.9,0.000001,100)
#0.1580806



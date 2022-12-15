rm(list = ls())  ##borro todo el ambiente

## 1. Inputs, Datos necesarios para el problema ---- 
cupon =0.05 ##cupon del bono es 5%
Amortizaciones = matrix(c(1,2,3,4,
                          25,25,25,25),ncol = 2, dimnames = list(NULL,c("t","Amort")))
##los 4 valores(del 1 al 4) son los m,omentos (en años) en los que se producen los flujos d fondos
## los 25s son caunto voy a cobrar de amortizacion en cada momento
##ncol dice que esos 8 datos que ingrese van a ir en dos columnas
##dimnames son los nombres que le pongo a las filas y columnas, si o si tengo que asignar una lista. Pirmero va el nombre de las filas (null porque no tiene) y luego las columnas

m = 2 #Numero de pagos de intereses por anio

View(Amortizaciones) ##Permite ver la matriz, abre una solapa aparte. Es mas elegante que escribirlo en la consola


##2. Marcha progresiva -----
#numero de flujos de efectivo
n=max(Amortizaciones[ ,1])*m 
#con corchetes llamo elementos de la matriz, como en el numero de filas lo dejo en blanco toma todas las filas.Entonces toma la primera columna de la matriz de Amortizaciones. EL maximo de esta columna
#Me da en que momento es el ultimo flujo de efectivo (4). Lo multiplico por la cantidad de veces que se pagan intereses por año (2)

#creo matriz de marcha con las siguientes columnas: t, Saldo, Amort, Interes, Flujo
#va a tener tantas filas como cantidad de flujos
Marcha = matrix(rep(NA,(n+1)*5), ncol = 5). #rep repite un valor una determionada cant de veces. Repito NA, (n+1)*5 veces
#5 Columnas porque incluyo el año 0
colnames(Marcha) = c("t","Saldo","Amort","Int","C.F.") #Le asigna nombres a las columnas

#Ahora Relleno la matriz Marcha(que tiene valores NA)

#Columna "t" tiempo
Marcha[,"t"] = seq(from=0,to n/m, by= 1/m)  #le asigno la secuencia de numeros que va desde 0 hasta el numero de flujos total divido la cantidad de flujos por año y que salte de 0,5

#Columnas "Amort" y "Saldo"

k =1
Marcha[1,"Saldo"] = sum(Amortizaciones[,"Amort"]) #En la columna saldo sumo las amortizaciones

for (i in 1:(n+1)){ #hace bucle, barre los elementos de la matriz Marcha(n+1 filas)
  if(Marcha[i,"t"] == Amortizaciones[k,"t"]){ #barre todas las filas de la columna, los compara con los momentos en los que ocurren amortizaciones
    Marcha[i,"Amort"] = Amortizaciones[k,"Amort"]
    if(i>1){Marcha[i,"Saldo"] = Marcha[i-1,"Saldo"] - Amortizaciones[k,"Amort"]} #El saldo es el saldo anterior, menos lo que amortice
    k=k+1
  }else{ ##Si el momento no coincide con uno donde ocurren amortizaciones, pone 0
    Marcha[i,"Amort"] = 0
    if(i>1){Marcha[i,"Saldo"] = Marcha[i-1,"saldo"]}
  }
} 

#Columna "Int"
Marcha[1,"Int"] = 0

for(i in 2:(n+1)){
  Marcha[i,"int"] = Marcha[i-1,"Saldo"] *cupon/m  #en cada momento del tiempo el interes es el saldo del momento anterior multiplicado por el cupon(tasa de interes)/2(porque es nominal anual)
}
  
Marcha[,"C.F."] = Marcha[,"Amort"] + Marcha[,"int"]



##3. Armado de la Funcion de Precio ----
#Debo pasarle la tasa de interes, El vector de flujos de efectivo y el vector de los momentos en el que ocurren esos momentos
Precio <- function(tasa,t,CF){
  n=lenght(CF) #n es la longitud del vecto de cashflows, el numero de cashflows que hay
  P=0
  for(i in 1:n){
    P = P +CF[i]*(1+tasa)^-t[i]
  }
  return(p)
}

Precio(0.05, Marcha[,"t"], Marcha[, "C.F."])
tasa1 = (1+0.05/2)^2-1 ##tasa igual al cupon
Precio(tasa1,Marcha[,"t"],Marcha[,"C.F."])


tasas=seq(from=0, to=1,by=0.001)  #recorre todas las tasas desde 0 a 1 aumentando en 0.001. SOn 1000 valores
P= Precio(tasas,Marcha[,"t"],Marcha[,"C.F."])  #Me calcula los miles de valores de precio
plot(tasas,P,type="b")  ##grafico de las tasas en relacion al precio, el grafico "b" devuelve tanto la linea como los circulitos en el grafico


##4. Calculo de la TIR ----

f <- function(r){
  return(-80 + Precio(r,Marcha[,"t"],Marcha[,"C.F."])) #a la funcion precio le resto el precio de mercado
}

plot(tasas, f(tasas),type="l", col= "red")  #Grafica la funcion f(tasas) en funcion de las tasas
abline(h=0) ##Grafica linea horizontal h quiere decir horizontal en 0

Raiz_Biseccion2(f,0,0.20,10^6,100)  #Resuelvo el problema por le metodo de biseccion
#el primer argumento es la funcion, el segundo el liomite inferior, el tercero el superior, el error tolerado y la cantidad de iteraciones
#devuelve el valor de tasa que hace que esta funcion sea una raiz. Para ese valor de tasa el precio teorico va a ser 80 (se anula)
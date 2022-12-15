#Ejercicio 4 i -----

x = c(6.41,6.76,7.11,7.46,7.81,8.16,8.51)
y = c(4.0951,5.1989,6.1303,6.7893,7.1079,7.0591,6.6598)

##A

derivada2ptos(x, y, H = T)  #Diferencia Progresiva

##B

derivada2ptos(x, y, H = F) #Diferencia Regresiva

##C

derivada3ptos(x, y, H = T, endpoint = F) #Middlepoint de 3 puntos

##D

derivada5ptos(x, y, H = T, endpoint = T) #Endpoint de 5 puntos
derivada5ptos(x, y, H = F, endpoint = T) #Hago progresiva y regresiva para calcular los valores extremos inferiores y superiores


##Ejercicio 4 ii)----

x = c(2.31,2.91,3.51,4.11,4.71,5.31,5.91)
y = c(3.8915,2.8249,1.4308,0.3549,0.0994,0.8591,2.4595)

##A

derivada2ptos(x, y, H = T)  #Diferencia Progresiva

##B 

derivada2ptos(x, y, H = F) #Diferencia Regresiva

##C

derivada3ptos(x, y, H = T, endpoint = F) #Middlepoint de 3 puntos

##D

derivada5ptos(x, y, H = T, endpoint = T) #Endpoint de 5 puntos
derivada5ptos(x, y, H = F, endpoint = T) #Hago progresiva y regresiva para calcular los valores extremos inferiores y superiores



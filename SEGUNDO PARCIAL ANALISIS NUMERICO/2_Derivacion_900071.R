#### DERIVACION ####
#TOMAS MERA 900071

S = seq(30,50,2.5)

c <- c(0.0097,0.0514,0.1902,0.5320,1.1938,2.2548,3.7250,5.5505,7.6442)
#2.1#
#ALGORITMO#

derivada5ptos_Endpoint_Regresiva<-function(x, y, xindice){
  
  n = length(y)
  yprima = c(rep(NA,n))
  h = -diff(x)[1]
  yprima[xindice] = (-25*y[xindice] + 48*y[xindice-1] - 36*y[xindice-2] + 16*y[xindice-3] - 3*y[xindice-4])/(12*h)
  return(yprima[xindice])
  
}

Aprox_C_Prima_30 <- derivada5ptos_Endpoint_Regresiva(S,c,1) #El 30 ocupa el primer indice
Aprox_C_Prima_30

#No se puede calcular ya que al ser regresiva calcula el punto extremo Final, 
#Va desde el final hacia el inicio y le estoy pidiendo el primer punto. Deberia calcularlo 
#con un ENDPOINT PROGRESIVO


Aprox_C_Prima_42.5 <- derivada5ptos_Endpoint_Regresiva(S,c,6) #El 42.5 acupa el sexto indice
Aprox_C_Prima_42.5


#2.2#
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

Aprox_C_Prima2_30 <- derivada_segunda(S,c,1) #El 30 ocupa el primer indice
Aprox_C_Prima2_30

#Este valor no se puede calcular ya que el metodo no sirve para calcular derivadas en los
#puntos limites. Solo se puede aplicar para derivadas intermedias.

Aprox_C_Prima2_42.5 <- derivada_segunda(S,c,6) #El 42.5 acupa el sexto indice
Aprox_C_Prima2_42.5


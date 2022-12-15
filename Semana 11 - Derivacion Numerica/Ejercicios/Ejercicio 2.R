#Ejercicio 2
#a----------------------------------------------------------

x = c(2.9,3,3.1,3.2)
y = c(-4.827866,-4.240058,-3.496909,-2.596792)

derivada3ptos<-function(x, y, xindice, H = T, endpoint = F){
  
  n = length(y)
  yprima = c(rep(NA,n))
  
  if(H == T){ #If para fórmulas progresivas
    h = diff(x)[1]
    if(endpoint == F){
      if(missing(xindice)){
        for (i in 2:(n - 1)) {
          yprima[i] = (-y[i-1] + y[i+1])/(2*h)
        }
        tabla = cbind.data.frame(x, y, yprima)
        return(tabla)
      }
      else{
        if(xindice == 1 || xindice == n){
          return("Este valor no se puede calcular")
        }
        else{
          yprima[xindice] = (-y[xindice-1] + y[xindice+1])/(2*h)
          return(yprima[xindice])
        }
      }
    }else{
      if(missing(xindice)){
        for (i in 1:(n - 2)) {
          yprima[i] = (-3*y[i] + 4*y[i+1] - y[i+2])/(2*h)
        }
        tabla = cbind.data.frame(x, y, yprima)
        return(tabla)
      }
      else{
        if(xindice == n || xindice == n-1){
          return("Este valor no se puede calcular")
        }
        else{
          yprima[xindice] = (-3*y[xindice] + 4*y[xindice+1] - y[xindice+2])/(2*h)
          return(yprima[xindice])
        }
      }
    }
  }else{#Formulas regresivas ----
    h = -diff(x)[1] 
    if(endpoint == F){
      if(missing(xindice)){
        for (i in 2:(n - 1)) {
          yprima[i] = (-y[i+1] + y[i-1])/(2*h)
        }
        tabla = cbind.data.frame(x, y, yprima)
        return(tabla)
      }
      else{
        if(xindice == 1 || xindice == n){
          return("Este valor no se puede calcular")
        }
        else{
          yprima[xindice] = (-y[xindice+1] + y[xindice-1])/(2*h)
          return(yprima[xindice])
        }
      }
    }else{
      if(missing(xindice)){
        for (i in 3:n) {
          yprima[i] = (-3*y[i] + 4*y[i-1] - y[i-2])/(2*h)
        }
        tabla = cbind.data.frame(x, y, yprima)
        return(tabla)
      }
      else{
        if(xindice == 1 || xindice == 2){
          return("Este valor no se puede calcular")
        }
        else{
          yprima[xindice] = (-3*y[xindice] + 4*y[xindice-1] - y[xindice-2])/(2*h)
          return(yprima[xindice])
        }
      }
    }  
  }
}

derivada3ptos(x, y, H = T, endpoint = F) #Progresiva, middlepoint --> Saco los dos puntos del medio

derivada3ptos(x, y, H = T, endpoint = T) #Progresiva, Endpoint --> Saco el primer punto

derivada3ptos(x, y, H = F, endpoint = T) #Regresiva, Endpoint --> Saco el ultimo punto

#-------------------------------------------------------------------------------
#b

x = c(8.1,8.3,8.5,8.7)
y = c(16.94410,17.56492,18.19056,18.82091)

derivada3ptos(x, y, H = T, endpoint = F) #Progresiva, middlepoint --> Saco los dos puntos del medio

derivada3ptos(x, y, 1, H = T, endpoint = T) #Progresiva, Endpoint --> Saco el primer punto

derivada3ptos(x, y,4, H = F, endpoint = T) #Regresiva, Endpoint --> Saco el ultimo punto


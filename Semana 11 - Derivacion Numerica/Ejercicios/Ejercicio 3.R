#Ejercicio 3

x = c(0.2,0.4,0.6,0.8,1)
y = c(0.9798652, 0.9177710,0.8080348,0.6386093,0.3843735)

derivada5ptos<-function(x, y, xindice, H = T, endpoint = F){
  
  n = length(y)
  yprima = c(rep(NA,n))
  
  if(H == T){#If para fórmulas progresivas
    h = diff(x)[1]
    
    if(endpoint == F){
      if(missing(xindice)){
        for (i in 3:(n - 2)) {
          yprima[i] = (y[i-2] - 8*y[i-1] + 8*y[i+1] - y[i+2])/(12*h)
        }
        tabla = cbind.data.frame(x, y, yprima)
        return(tabla)
      }
      else{
        if(xindice == n || xindice == n-1 || xindice == 1 || xindice == 2){
          return("Este valor no se puede calcular")
        }
        else{
          yprima[xindice] = (y[xindice-2] - 8*y[xindice-1] + 8*y[xindice+1] - y[xindice+2])/(12*h)
          return(yprima[xindice])
        }
      }
    }else{
      if(missing(xindice)){
        for (i in 1:(n - 4)) {
          yprima[i] = (-25*y[i] + 48*y[i+1] - 36*y[i+2] + 16*y[i+3] - 3*y[i+4])/(12*h)
        }
        tabla = cbind.data.frame(x, y, yprima)
        return(tabla)
      }
      else{
        if(xindice == n || xindice == n-1 || xindice == n-2 || xindice == n-3){
          return("Este valor no se puede calcular")
        }
        else{
          yprima[xindice] = (-25*y[xindice] + 48*y[xindice+1] - 36*y[xindice+2] + 16*y[xindice+3] - 3*y[xindice+4])/(12*h)
          return(yprima[xindice])
        }
      }
    }
  }
  else{#If para fórmulas regresivas
    h = -diff(x)[1]
    if(endpoint == F){
      if(missing(xindice)){
        for (i in 3:(n - 2)) {
          yprima[i] = (y[i+2] - 8*y[i+1] + 8*y[i-1] - y[i-2])/(12*h)
        }
        tabla = cbind.data.frame(x, y, yprima)
        return(tabla)
      }
      else{
        if(xindice == n || xindice == n-1 || xindice == 1 || xindice == 2){
          return("Este valor no se puede calcular")
        }
        else{
          yprima[xindice] = (y[xindice+2] - 8*y[xindice+1] + 8*y[xindice-1] - y[xindice-2])/(12*h)
          return(yprima[xindice])
        }
      }
    }else{
      if(missing(xindice)){
        for (i in 5:n) {
          yprima[i] = (-25*y[i] + 48*y[i-1] - 36*y[i-2] + 16*y[i-3] - 3*y[i-4])/(12*h)
        }
        tabla = cbind.data.frame(x, y, yprima)
        return(tabla)
      }
      else{
        if(xindice == 1 || xindice == 2 || xindice == 3 || xindice == 4){
          return("Este valor no se puede calcular")
        }
        else{
          yprima[xindice] = (-25*y[xindice] + 48*y[xindice-1] - 36*y[xindice-2] + 16*y[xindice-3] - 3*y[xindice-4])/(12*h)
          return(yprima[xindice])
        }
      }
    }
  }
  
}

#A------

derivada5ptos(x, y , 1 , H = T, endpoint = T)  #El primer punto lo calculo con una progresiva

#B

derivada5ptos(x, y , 5 , H = F, endpoint = T) #El ultimo punto lo calculo con una regresiva

#C

derivada5ptos(x, y, 3 , H = F, endpoint = F) #El punto del medio lo calculo con middlepoint, es lo mismo si es 
                                            #progresiva o regresiva, solo calcula el punto del medio y ninguno mas



##Ejercicio 1) a-----

x = c(0.5,0.6,0.7)
y = c(0.4794,0.5646,0.6442)

derivada2ptos<-function(x, y, xindice, H = T){ #será progresivo por H = True
  n = length(y)
  yprima = c(rep(NA,n))
  
  if(H == T){#Progresivo
    h = diff(x)[1]
    
    if(missing(xindice)){
      for (i in 1:(n - 1)) {
        yprima[i] = (y[i+1] - y[i])/h
      }
      tabla = cbind.data.frame(x, y, yprima)
      return(tabla)
    }
    else{
      if(xindice == n){
        return("Este valor no se puede calcular")
      }
      else{
        yprima[xindice] = (y[xindice+1] - y[xindice])/h
        return(yprima[xindice])
      }
    }
  }
  else{#Regresivo
    h = -diff(x)[1]
    if(missing(xindice)){
      for (i in 2:n) {
        yprima[i] = (y[i-1] - y[i])/h
      }
      tabla = cbind.data.frame(x, y, yprima)
      return(tabla)
    }
    else{
      if(xindice == 1){
        return("Este valor no se puede calcular")
      }
      else{
        yprima[xindice] = (y[xindice-1] - y[xindice])/h
        return(yprima[xindice])
      }
    }
  }
}


derivada2ptos(x, y, H = T)  ## Progresiva

derivada2ptos(x, y, H = F) ##Regresiva

####-----------------------------------------------------
#Ejercicio 1)b

x = c(0,0.2,0.4)
y = c(0,0.74140,1.3718)

derivada2ptos(x, y, H = T)  ## Progresiva

derivada2ptos(x, y, H = F) ##Regresiva

rm(list = ls())
graphics.off()
x = c(0, 0.05, 0.1, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.5)
y = c(120, 95.67052, 77.25528, 63.12629, 52.15021, 43.52512, 36.67519, 31.18120, 26.73410, 23.10338, 20.11523)

# Los parámetros x e y son los datos tabulados iniciales.
# El parámetro opcional xindice calcula la derivada en un punto exacto. 
#Es necesario tener en cuenta que el indice corresponde al valor de x 
#en ese indice de la variable:
# es decir que si tengo x = (3, 4, 5) y quiero calcular la derivada en x = 4
#el xindice debe ser 2, ya que R empieza a contar desde el 1. 
#Recomiendo visualizar la tabla completa 
# sin el xindice para tener certeza de la derivada buscada. 
#Con este parametro se retorna el valor, por lo cual puede ser guardado en una variable.
# El parámetro, por defecto como TRUE, H indica que las formulas serán progresivas,
#con H = F las formulas serán regresivas.

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

#si tengo x = (3, 4, 5) y quiero calcular la derivada en x = 4
#el xindice debe ser 2, ya que R empieza a contar desde el 1.
derivada2ptos(x, y, 3, H = T)

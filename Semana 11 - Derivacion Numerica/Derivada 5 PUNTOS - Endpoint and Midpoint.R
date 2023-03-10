rm(list = ls())
graphics.off()
x = c(0, 0.05, 0.1, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.5)
y = c(120, 95.67052, 77.25528, 63.12629, 52.15021, 43.52512, 36.67519, 31.18120, 26.73410, 23.10338, 20.11523)

# Los par?metros x e y son los datos tabulados iniciales.
# El par?metro opcional xindice calcula la derivada en un punto exacto.
#Es necesario tener en cuenta que el indice corresponde al valor de x en ese indice 
#de la variable:
# es decir que si tengo x = (3, 4, 5) y quiero calcular la derivada en x = 4 el
#xindice debe ser 2, ya que R empieza a contar desde el 1. Recomiendo visualizar
#la tabla completa 
# sin el xindice para tener certeza de la derivada buscada.
#Con este parametro se retorna el valor, por lo cual puede ser guardado en una variable.
# El par?metro, por defecto como TRUE, H indica que las formulas ser?n 
#progresivas, con H = F las formulas ser?n regresivas.
# El par?metro, por defecto como FALSE, endpoint indica si la formula es con punto medio
#o punto extremo (endpoint = T)


derivada5ptos<-function(x, y, xindice, H = T, endpoint = F){
  
  n = length(y)
  yprima = c(rep(NA,n))
  
  if(H == T){#If para f?rmulas progresivas
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
  else{#If para f?rmulas regresivas
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

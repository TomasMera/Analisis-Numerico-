rm(list = ls())
graphics.off()
x = c(0, 0.05, 0.1, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.5)
y = c(120, 95.67052, 77.25528, 63.12629, 52.15021, 43.52512, 36.67519, 31.18120, 26.73410, 23.10338, 20.11523)

# Los parámetros x e y son los datos tabulados iniciales.
# El parámetro opcional xindice calcula la derivada en un punto exacto. Es necesario tener en cuenta que el indice corresponde al valor de x en ese indice de la variable:
# es decir que si tengo x = (3, 4, 5) y quiero calcular la derivada en x = 4 el xindice debe ser 2, ya que R empieza a contar desde el 1. Recomiendo visualizar la tabla completa 
# sin el xindice para tener certeza de la derivada buscada. Con este parametro se retorna el valor, por lo cual puede ser guardado en una variable.


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




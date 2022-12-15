#Ejercicio 5

x = c(2.31,2.91,3.51,4.11,4.71,5.31,5.91)
y = c(3.8915,2.8249,1.4308,0.3549,0.0994,0.8591,2.4595)


derivada_segunda = function(x, y, xindice, h){
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

derivada_segunda(x,y,4, 0.6)
derivada_segunda(x,y,4,1.2)

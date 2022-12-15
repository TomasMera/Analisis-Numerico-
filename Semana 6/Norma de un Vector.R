## Funcion Norma de un vector ##
Norma <- function(y, metodo){
  if (metodo==2)
  {
    return(sqrt(sum(y^2)))
  
  }
  if (metodo==Inf){
    return(max(abs(y)))
  }
  return("El metodo debe ser 2 o inf")
}


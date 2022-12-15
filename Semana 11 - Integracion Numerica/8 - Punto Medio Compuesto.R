rm(list = ls())

# Función a integrar
fn = function(x){
  val = x*sqrt(x^2 + 9)
  return(val)
}

puntomedio.compuesto = function(a, b, n){
  if((n %% 2) == 0){
    h = (b-a)/(n+2)
    
    xi2 = 0
    for (i in -1:(n+1)) {
      x = a + (i+1)*h
      if((i %% 2) == 0){
        xi2 = xi2 + fn(x)
      }
    }
    val = 2*h*xi2
    return(val)
  }
  else return("n debe ser par")
}

(puntomedio.compuesto(0,4,2000))
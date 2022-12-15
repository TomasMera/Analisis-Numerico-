rm(list = ls())

# Función a integrar
fn = function(x){
  val = x*sqrt(x^2 + 9)
  return(val)
}

trapecio.compuesto = function(a, b, n){
  h = (b-a)/n
  
  xio = fn(a) + fn(b)
  xi = 0
  for (i in 1:(n-1)) {
    x = a + i*h
    xi = xi + fn(x)
  }
  val = h*(xio + 2*xi)/2
  return(val)
}
(trapecio.compuesto(0,4,401))

rm(list = ls())

# Función a integrar
fn = function(x){
  val = x^(z-1)*exp(-x)
  return(val)
}

z=9.23

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

#punto a : 6173.21
(puntomedio.compuesto(0,100,200))


#punto b
#r(a) siempre que a sea positivo  es igual a (a-1)!
mi.factorial <- function(n){
  factorial <- 1
  for (i in 1:n){
    factorial <- factorial * i
  }
  return(factorial)
}


b=0.43
a=9.23
ra= mi.factorial(a-1)

mu = b * a #esperanza matematica
mu # 3.9689

var = a * b^2
var #1.706627

sqrt(1.706627)#1.306379


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


fn = function(x){
  val = (b*exp(-b*x))* ((b*x)^(a???1))/(ra)
  return(val)
}



(puntomedio.compuesto(0,100,10)) #1.415337


###########################################
#punto c

rm(list = ls())

# Función a integrar
fn = function(x){
  val = x*((b*exp(-b*x))* ((b*x)^(a???1))/(ra))
  return(val)
}

mi.factorial <- function(n){
  factorial <- 1
  for (i in 1:n){
    factorial <- factorial * i
  }
  return(factorial)
}


b=0.43
a=9.23
ra= mi.factorial(a-1)

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
(trapecio.compuesto(0,4,401)) #0.0003052964
###########################################

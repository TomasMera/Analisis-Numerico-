###Ejercicio 3
##Simpson Compuesto ----
simpson.compuesto = function(a, b, n){
  if((n %% 2) == 0){
    #Paso 1
    h = (b-a)/n
    
    #Paso 2
    xio = fn(a) + fn(b)
    xi1 = 0 #Suma impar
    xi2 = 0 #Suma par
    
    #Paso 3
    for (i in 1:(n-1)) {
      #Paso 4
      x = a + i*h
      #Paso 5 
      if((i %% 2) == 0){
        xi2 = xi2 + fn(x)
      }else{
        xi1 = xi1 + fn(x)
      }
    }
    #Paso 6
    val = h*(xio + 2*xi2 + 4*xi1)/3
    return(val)
  }
  else return("n debe ser par")
}

#A
fn = function(x){
  val = x * log(x)
  return(val)
}

simpson.compuesto(1,2,4)

#B
fn = function(x){
  val = x^3 *exp(x)
  return(val)
}

simpson.compuesto(-2,2,4)


#C
fn = function(x){
  val = 2/(x^2  + 4)
  return(val)
}

simpson.compuesto(0,2,6)


#D
fn = function(x){
  val = x^2 * cos(x)
  return(val)
}

simpson.compuesto(0,pi,6)


#E
fn = function(x){
  val = exp(2*x) * sin(3*x)
  return(val)
}

simpson.compuesto(0,2,8)


#F
fn = function(x){
  val = 2/(x^2  + 4)
  return(val)
}

simpson.compuesto(1,3,8)


#G
fn = function(x){
  val = 1/sqrt(x^2  - 4)
  return(val)
}

simpson.compuesto(3,5,8)


#H
fn = function(x){
  val = tan(x)
  return(val)
}

simpson.compuesto(0,3*pi/8,8)




##Trapecio Compuesto----
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

#A
fn = function(x){
  val = x * log(x)
  return(val)
}

trapecio.compuesto(1,2,4)

#B
fn = function(x){
  val = x^3 *exp(x)
  return(val)
}

trapecio.compuesto(-2,2,4)


#C
fn = function(x){
  val = 2/(x^2  + 4)
  return(val)
}

trapecio.compuesto(0,2,6)


#D
fn = function(x){
  val = x^2 * cos(x)
  return(val)
}

trapecio.compuesto(0,pi,6)


#E
fn = function(x){
  val = exp(2*x) * sin(3*x)
  return(val)
}

trapecio.compuesto(0,2,8)


#F
fn = function(x){
  val = 2/(x^2  + 4)
  return(val)
}

trapecio.compuesto(1,3,8)


#G
fn = function(x){
  val = 1/sqrt(x^2  - 4)
  return(val)
}

trapecio.compuesto(3,5,8)


#H
fn = function(x){
  val = tan(x)
  return(val)
}

trapecio.compuesto(0,3*pi/8,8)


##Punto Medio Compuesto----
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


#A
fn = function(x){
  val = x * log(x)
  return(val)
}

puntomedio.compuesto(1,2,4)

#B
fn = function(x){
  val = x^3 *exp(x)
  return(val)
}

puntomedio.compuesto(-2,2,4)


#C
fn = function(x){
  val = 2/(x^2  + 4)
  return(val)
}

puntomedio.compuesto(0,2,6)


#D
fn = function(x){
  val = x^2 * cos(x)
  return(val)
}

puntomedio.compuesto(0,pi,6)


#E
fn = function(x){
  val = exp(2*x) * sin(3*x)
  return(val)
}

puntomedio.compuesto(0,2,8)


#F
fn = function(x){
  val = 2/(x^2  + 4)
  return(val)
}

puntomedio.compuesto(1,3,8)


#G
fn = function(x){
  val = 1/sqrt(x^2  - 4)
  return(val)
}

puntomedio.compuesto(3,5,8)


#H
fn = function(x){
  val = tan(x)
  return(val)
}

puntomedio.compuesto(0,3*pi/8,8)

##Ejercicio 1

int_newton.cotes = function(a, b, n){
  h = (b - a)/n
  
  if(n == 1){
    xo = a
    x1 = b
    
    result = (h/2)*(fn(xo) + fn(x1))
    return(result)
  }
  else if(n == 2){
    xo = a
    x2 = b
    
    x1 = (xo + x2)/2
    #x1a = a + (b -a)/2
    
    result = (h/3)*(fn(xo) + 4*fn(x1) + fn(x2))
    return(result)
  }
  else if(n == 3){
    xo = a
    x3 = b
    
    x1 = xo + h
    x2 = xo + 2*h
    
    result = ((3/8)*h)*(fn(xo) + 3*fn(x1) + 3*fn(x2) + fn(x3))
    return(result)
  }
  else if(n == 4){
    xo = a
    x4 = b
    
    x1 = xo + h
    x2 = xo + 2*h
    x3 = xo + 3*h
    
    result = ((2/45)*h)*(7*fn(xo) + 32*fn(x1) + 12*fn(x2) + 32*fn(x3) + 7*fn(x4))
    return(result)
  }
  else print("Grado del polinomio no programado")
}

##Ejercicio 1.1 ----
  #A
fn = function(x){
  val = 2/(x-4)
  return(val)
}

int_newton.cotes(0, 0.5, 1)

alim<-0   #cargar límite inferior intervalo
blim<-0.5  #cargar límite superior intervalo
h <- blim - alim

d1sc<-D(expression(2/(x-4)),"x") #cargar función que desea derivar
d2sc<-D(d1sc,"x")
d2sc

derivadasc2<-function(x){
  f<- 2 * (2 * (x - 4))/((x - 4)^2)^2
  return(f)
}

optimosc<-optimize(derivadasc2,c(alim,blim),maximum = T)
optsc<-abs(optimosc$objective)
optsc

Error = (h^3)/12 * optsc

Integral = int_newton.cotes(0, 0.5, 1) + Error
Integral

  #B
fn = function(x){
  val = 2/((x^2)-4)
  return(val)
}

int_newton.cotes(0, 0.35, 1)

##Ejercicio 1.2 ----
  #A
fn = function(x){
  val = x^2 * exp(-x)
  return(val)
}

int_newton.cotes(0, 1, 2)

  #B
fn = function(x){
  val = exp(3*x) * sin(2*x)
  return(val)
}

int_newton.cotes(0, pi/4, 2)

##Ejercicio 1.3-----
  #A
fn = function(x){
  val = x^4
  return(val)
}

int_newton.cotes(0.5, 1, 3)

  #B
fn = function(x){
  val = (2*x)/(x^2 - 4)
  return(val)
}

int_newton.cotes(1, 1.6, 3)


##Ejercicio 1.4-----
  #A
fn = function(x){
  val = x^2 * log(x)
  return(val)
}

int_newton.cotes(1, 1.5, 4)

  #B
fn = function(x){
  val = x * sin(x)
  return(val)
}

int_newton.cotes(0, pi/4, 4)

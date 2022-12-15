###Ejercicio 2

int_newton.cotesopen = function(a, b, n){
  h = (b - a)/(n + 2)
  xo = a + h
  
  if(n == 0){
    result = 2*h*fn(xo)
    return(result)
  }
  else if(n == 1){
    x1 = b - h
    print(x1)
    #x1a = a + h
    #print(x1a)
    result = ((3/2)*h)*(fn(xo) + fn(x1))
    return(result)
  }
  else if(n == 2){
    x1 = xo + h
    x2 = b - h
    
    result = ((4/3)*h)*(2*fn(xo) - fn(x1) + 2*fn(x2))
    return(result)
  }
  else if(n == 3){
    x1 = xo + h
    x2 = xo + 2*h
    x3 = b - h
    
    result = ((5/24)*h)*(11*fn(xo) + fn(x1) + fn(x2) + 11*fn(x3))
    return(result)
  }
  else print("Grado del polinomio no programado")
}

# A
fn = function(x){
  val = sqrt(x + 1)
  return(val)
}

int_newton.cotesopen(0, 0.1, 0) 
int_newton.cotesopen(0, 0.1, 1)
int_newton.cotesopen(0, 0.1, 2)
int_newton.cotesopen(0, 0.1, 3)

#B

fn = function(x){
  val = (sin(x))^2
  return(val)
}

int_newton.cotesopen(0, pi/2, 0) 
int_newton.cotesopen(0, pi/2, 1)
int_newton.cotesopen(0, pi/2, 2)
int_newton.cotesopen(0, pi/2, 3)

#C
fn = function(x){
  val = exp(x)
  return(val)
}

int_newton.cotesopen(1.1, 1.5, 0) 
int_newton.cotesopen(1.1, 1.5, 1) 
int_newton.cotesopen(1.1, 1.5, 2) 
int_newton.cotesopen(1.1, 1.5, 3) 


#D
fn = function(x){
  val = 1/x
  return(val)
}

int_newton.cotesopen(1, 10, 0) 
int_newton.cotesopen(1, 10, 1)
int_newton.cotesopen(1, 10, 2)
int_newton.cotesopen(1, 10, 3)
  

#E
fn = function(x){
  val = 1/x
  return(val)
}

rdo_1.1 <- int_newton.cotesopen(1, 5.5, 0)
rdo_2.1 <- int_newton.cotesopen(1, 5.5, 1)
rdo_3.1 <- int_newton.cotesopen(1, 5.5, 2)
rdo_4.1 <- int_newton.cotesopen(1, 5.5, 3)


rdo_1.2 <- int_newton.cotesopen(5.5,10, 0)
rdo_2.2 <-int_newton.cotesopen(5.5,10, 1)
rdo_3.2 <-int_newton.cotesopen(5.5,10, 2)
rdo_4.2 <-int_newton.cotesopen(5.5,10, 3)

RDO1 <- rdo_1.1 + rdo_1.2
RDO2 <- rdo_2.1 + rdo_2.2
RDO3 <- rdo_3.1 + rdo_3.2
RDO4 <- rdo_4.1 + rdo_4.2


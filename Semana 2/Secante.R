fn <- function(r) #Funcion 
{
  f = (3200*(1-(1+(r/12))^-88)/(r/12))-180000
  return(f)
}


metodo_secante <- function(p0, p1, tol, n)
{
  #Paso 1
  i = 2
  q0 = fn(p0)
  q1 = fn(p1)
  #Paso 2
  while(i <= n)
  {
    #Paso 3
    p = p1 -(q1*(p1 - p0))/(q1 - q0)
    #Paso 4
    if(abs(p - p1) < tol)
    {
      return(p)
    }
    #Paso 5
    i = i + 1
    #Paso 6
    p0 = p1
    q0 = q1
    p1 = p
    q1 = fn(p)
  }
  #Paso 7
  return(paste('El metodo fallo luego de ', n, 'iteraciones'))
}

# Chequear que f(po) * f(p1) <0

s = metodo_secante(-24, -23.1, 0.00001,100000000000) 
print(s)
##Parcial en PDF 

#Ejercicio 1: Secante ----

fn<- function(x){
  h = cos(x) * x^1.54
  return(h)
}

x <- seq(16,36, by=1)
y<- fn(x)
df <- data.frame(x,y)

grafico = ggplot(data = df)+
  aes(x=x,y=y) + #Agrego capa estética (culumnas de "df")
  geom_line(linetype=1,colour="darkblue") + #Agrego la geometria
  geom_hline(yintercept = 0,linetype=1) + #Agrego linea que cruza en y=0
  geom_vline(xintercept = 0,linetype=1) +
  scale_x_continuous(name = "x",limits = c(16,36), breaks = seq(16,36, by = 1))
grafico


#Ejercicio 1_c



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

Raiz_1 <- metodo_secante(17,18,10^-4,100)
Raiz_2<- metodo_secante(20,21,10^-4,100)
Raiz_3 <-  metodo_secante(23,24,10^-4,100)
Raiz_4 <-  metodo_secante(26,27,10^-4,100)
Raiz_5 <-  metodo_secante(29.5,30,10^-4,100)
Raiz_6 <-  metodo_secante(32.5,33.5,10^-4,100)
print(paste("Raiz 1: ", Raiz_1,"
            Raiz 2: ", Raiz_2,"
            Raiz 3: ",Raiz_3,"
            Raiz 4: ",Raiz_4,"
            Raiz 5: ", Raiz_5,"
            Raiz 6: ",Raiz_6))

##Verificar
options(scipen= 9999)

fn(Raiz_1)
fn(Raiz_2)
fn(Raiz_3)
fn(Raiz_4)
fn(Raiz_5)
fn(Raiz_6)




##Ejercicio 1_d

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
    print(paste("N: ", i , "| p: " ,p))
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

metodo_secante(21,24,10^-4,14)


###Ejercicio 2 ----

fact_cholesky = function(A){
  n = ncol(A)
  i = 1
  L = matrix(data = 0, nrow = n, ncol = n, byrow = TRUE) 
  #Paso 1
  L[1,1] = (A[1,1])^(0.5)
  #Paso 2
  j = i + 1
  for (j in j:n) {
    L[j,1] = A[j,1]/L[1,1]
  }
  #Paso 3
  for (i in (i+1):(n-1)) {
    #Paso 4
    sum1 = 0
    k = 1
    for (k in k:(i - 1)) {
      sum1 = sum1 + L[i,k]^2
    }
    L[i,i] = sqrt(A[i,i] - sum1)
    #Paso 5
    for (j in (i+1):n) {
      sum2 = 0
      k = 1
      for (k in k:(i - 1)) {
        sum2 = sum2 + (L[j,k]*L[i,k])
      }
      L[j,i] = (A[j,i] - sum2)/L[i,i]
    }
  }
  #Paso 6
  sum3 = 0
  k = 1
  for (k in k:(n - 1)) {
    sum3 = sum3 + (L[n,k]^2)
  }
  L[n,n] = sqrt((A[n,n] - sum3))
  #Paso 7
  return(L)
}

# Datos 
A = matrix(data = c(4,-1,1,
                    -1,4.25,2.75,
                    1,2.75,3.55), nrow = 3, byrow = T)
A

#Compruebo
L = fact_cholesky(A)
L%*%t(L)
A == L%*%t(L)

#Ejercicio 4 ----

# 1) Cargar algoritmo de Norma 
norma <- function(y, metodo){
  if (metodo==2){
    return(sqrt(sum(y^2)))
  }
  if (metodo==Inf){
    return(max(abs(y)))
  }
  return("El metodo debe ser 2 o Inf")
}
# 2) Carga Algoritmo de SENOL 
Sist_Ec_NoLineal_Newton <- function(n,x,TOL,N){
  #Paso 1
  k <- 1
  #Paso 2
  while(k<=N){
    #Paso 3
    fx <- Fx(x)
    J <- Jacobiano(x[1],x[2],x[3])
    #Paso 4
    y = solve(J)%*%-fx
    #Paso 5
    x <- x + t(y)
    #Paso 6
    if (norma(y,2) < TOL){
      return(x)
    }
    #Paso 7
    k <- k+1
  }
  #Paso 8
  return(paste('Numero max de iteraciones excedido'))
}

# 3) Defino fn y dfn 

#         i)Funcion 1 y sus derivadas 
f1 <- function(x1,x2,x3){
  return(3*x1 - cos(x2 * x3) - 0.6)
}

f<-expression(3*x1 - cos(x2 * x3) - 0.6)
fprima<-D(f,"x3")
fprima

#Derivada primera de f1
df11 <- function(x1,x2,x3){
  return(3)
}
#Derivada segunda de f1
df12 <- function(x1,x2,x3){
  return(sin(x2 * x3) * x3)
}
#Derivada tercera de f1
df13 <- function(x1,x2,x3){
  return(sin(x2 * x3) * x2)
}

#         ii)Funcion 2 y sus derivadas
f2 <- function(x1,x2,x3){
  return (10*x1^2 - 88*(x2 +1/10)^2 +sin(x3) +0.97)
}

f<-expression(10*x1^2 - 88*(x2 +1/10)^2 +sin(x3) +0.97)
fprima<-D(f,"x3")
fprima

#Derivada primera de f2
df21 <- function(x1,x2,x3){
  return(10 * (2 * x1))
}
#Derivada segunda de f2
df22 <- function(x1,x2,x3){
  return(-(88 * (2 * (x2 + 1/10))))
}
#Derivada tercera de f2
df23 <- function(x1,x2,x3){
  return(cos(x3))
}

#         iii)Funcion 3 y sus derivadas 
f3 <- function(x1,x2,x3){
  return(exp(-x1*x2) +21*x3 -2 +(11/3)*pi)
}

f<-expression(exp(-x1*x2) +21*x3 -2 +(11/3)*pi)
fprima<-D(f,"x3")
fprima

#Derivada primera de f3
df31 <- function(x1,x2,x3){
  return(-(exp(-x1 * x2) * x2))
}
#Derivada segunda de f3
df32 <- function(x1,x2,x3){
  return(-(exp(-x1 * x2) * x1))
}
#Derivada tercera de f3
df33 <- function(x1,x2,x3){
  return(21)
}
# 4) Matriz Jacobiana y Fx 
###Defino como será la Matriz jacobiana

Jacobiano <- function(x1,x2,x3){
  col1 <- 
    c(df11(x1,x2,x3),df12(x1,x2,x3),df13(x1,x2,x3))
  
  col2 <- 
    c(df21(x1,x2,x3),df22(x1,x2,x3),df23(x1,x2,x3))
  
  col3 <-
    c(df31(x1,x2,x3),df32(x1,x2,x3),df33(x1,x2,x3))
  
  J <- rbind(col1,col2, col3) #con esta ultima armamos la matrix ampliada
  return(J)
}
#Definimos ahora Fx
Fx <- function(x){
  Fx <- rbind(f1(x[1],x[2],x[3]), f2(x[1],x[2],x[3]), f3(x[1],x[2],x[3]))
  return(Fx)
} #sera una matriz ampliada con las funciones definadas antes

# 5) Defino los puntos 
x <- c(1, 1, -1)
n=3
Resultado <- Sist_Ec_NoLineal_Newton(n, x, 10^-5, 100)
Resultado

# 6) Chequeo el resultado 
#Asigno los rdos del algoritmo a las variables x1,x2 y x3
x1 <- Sist_Ec_NoLineal_Newton(n,x, 10^-6, 100)[1] #posicion, osea mult por posicion 1
x2 <- Sist_Ec_NoLineal_Newton(n,x, 10^-6, 100)[2]
x3 <- Sist_Ec_NoLineal_Newton(n,x, 10^-6, 100)[3]

# 7) Imprimo resultados 
f1(x1, x2, x3)
f2(x1, x2, x3)
f3(x1, x2, x3)



#Ejercicio 5.1 SQL -----
#SELECT e.FirstName as NombreEmpleado, e.LastName as ApellidoEmpleado, sum(od.Quantity) as CantidadVentas, round(sum(p.Price * od.Quantity), 2) as Total
#FROM Categories c
#INNER JOIN Products p  ON p.CategoryID = c.CategoryID
#INNER JOIN Customers c ON c.CustomerID = o.CustomerID
#INNER JOIN Employees e ON e.EmployeeID = o.EmployeeID
#INNER JOIN OrderDetails od ON p.ProductID = od.ProductID
#INNER JOIN Orders o ON o.OrderID = od.OrderID
#Group by e.LastName
#Having Total >18800 and sum(od.Quantity) > 10





#Ejercicio 5.2 SQL ----
#SELECT s.SupplierName as NombreProveedor, s.City as CiudadProveedor, s.Country as PaisProveedor, s.ContactName as ContactoProveedor,  od.Quantity as CantidadVentas, (p.Price * od.Quantity) as Total
#FROM Categories c
#INNER JOIN Products p ON c.CategoryID = p.CategoryID
#INNER JOIN OrderDetails od ON p.ProductID = od.ProductID
#INNER JOIN Suppliers s ON p.SupplierID = s.SupplierID
#INNER JOIN Orders o ON od.OrderID = o.OrderID
#INNER JOIN Employees e ON o.EmployeeID = e.EmployeeID
#INNER JOIN Customers cu ON o.CustomerID = cu.CustomerID
#where PaisProveedor = 'Italy'
#ORDER BY Total desc

#Ejercicio 3
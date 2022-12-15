
f <- function(x){
  fn <-  2*sin(x) - exp(10/x)
  return(fn)
}

x <- seq(10,30,by = 0.5)
fx <- f(x)
df <- data.frame(x,fx)

grafico = ggplot(data = df)+
  aes(x = x, y= fx) +
  geom_line(linetype= 1, colour="darkblue")+
  geom_hline(yintercept= 0, linetype =1)+
  geom_vline(xintercept = 0, linetype =1)+
  scale_x_continuous(name = "x", limits= c(15,30),breaks = seq(15,30,by = 2))+
  scale_y_continuous(name = "fx", limits = c(-1,1),breaks = seq(-1,1,by=1))
grafico


funcion <- expression(2*sin(x) - exp(10/x))
fprima <- D(funcion,"x")
fprima

df <- function(x){
  pn<- 2 * cos(x) + exp(10/x) * (10/x^2)
  return(pn)
}

NewtonRapshon <- function(p0,Tol,N){ ##No hay que darle como argumnetos la funcion y su derivada, sino que hay que invocarlas directamente.
  #Paso 1
  i = 1
  #Paso 2
  while (i <= N){
    #Paso 3
    p = p0 - f(p0)/df(p0)  #Cambiaria el cociente, la funcion va en el nuemrador y la derivada en el denominador
    #Paso 4
    if (abs(p0-p) < Tol){ 
      return(p0)
    }#Falta abrir la "{}" para especificar el codigo que se va a ejecutar si cumplela condicion 
    #Paso 5
    i = i + 1
    #Paso 6
    p0 = p
    print(p0)
  }
  #Paso 7
  return(paste('El método falla luego de ', N, ' iteraciones')) #N iteraciones debe ir en mayusculas ya que R discrimina los caracteres en este sentido
}

x0 = 24 - ifelse(d4!=0,d4,ifelse(d5!=0,d5,d6))
Iteraciones = ifelse(d4>=2,d4,ifelse(d5>=2,d5,d6))
NewtonRapshon(x0,10^-3,Iteraciones)

NewtonRapshon(20,10^-3,100) #raiz1

NewtonRapshon(21,10^-3,100) #raiz2

NewtonRapshon(26,10^-3,100) #raiz 3


ifelse(d4>=2,d4,ifelse(d5>=2,d5,d6))


Funcion <- function(x){
  f <- x^2 * cos(x) 
  return(f)
}

x <- seq(0,3*pi,by = 0.5) #Generamos un vector "x" para crear los puntos en F(x)
fx <- Funcion(x) #Creamos los valores de f(x)
df <- data.frame(x, fx)

grafico = ggplot(data = df)+
  aes(x = x, y= fx) +
  geom_line(linetype= 1, colour="darkblue")+
  geom_hline(yintercept= 0, linetype =1)+
  geom_vline(xintercept = 0, linetype =1)+
  scale_x_continuous(name = "x", limits= c(0,2.5),breaks = seq(0,2.5,by = 0.5))+
  scale_y_continuous(name = "fx", limits = c(-3,2),breaks = seq(-3,2,by=1))
grafico

Falsa_Posicion <- function(p0, p1 , tol , n){
  i=2
  q0=Funcion(p0)
  q1=Funcion(p1)
  while(i<=n){
    p=p1 - q1*(p1 - p0)/(q1 - q0)
    if(abs(p-p1) < tol){
      return(p)
    }
    i= i+1
    q = Funcion(p)
    if(q*q1 <0){
      p0=p1
      q0=q1
    }
    p1=p
    q1=q
    print(q1)
  }
  return(paste("El metodo fallo luego de ",n," iteraciones"))
}


Funcion(0)

Falsa_Posicion(1.5,1.7,10^-3,100)

Falsa_Posicion(4,5,10^-3,100)

Falsa_Posicion(7.5,9, 10^-3,100)



grafico = ggplot(data = df)+
  aes(x = x, y= fx) +
  geom_line(linetype= 1, colour="darkblue")+
  geom_hline(yintercept= 0, linetype =1)+
  geom_vline(xintercept = 0, linetype =1)+
  geom_point(aes(x =Falsa_Posicion(1.5,1.7,10^-3,100),
                 y = 0),pch = 16, col = "red")+
  geom_point(aes(x =Falsa_Posicion(4,5,10^-3,100),
                 y = 0),pch = 16, col = "red")+
  geom_point(aes(x =Falsa_Posicion(7.5,9,10^-3,100),
                 y = 0),pch = 16, col = "red")+
  geom_point(aes(x =0,
                 y = 0),pch = 16, col = "red")
grafico


write_matex2 <- function(x) {
  begin = "\\begin{bmatrix}"
  end = "\\end{bmatrix}"
  X =
    apply(x, 1, function(x) {
      paste(
        paste(x, collapse = "&"),
        "\\\\"
      )
    })
  paste(c(begin, X, end), collapse = "")
}


set.seed(900071)
datos = matrix(rnorm(3000,10,2),nrow = 1000, ncol = 3)
A = round(cor(datos),4)
A

set.seed(900071)
datos = matrix(rnorm(3000,10,2),nrow = 1000, ncol = 3)
A = round(cor(datos),4)
A

fact_cholesky(A)

L = fact_cholesky(A)
L%*%t(L)
A


B



###SQL
#SELECT c.CustomerName as NombreCliente, c.City as CiudadCliente, c.Country as c.PaisCliente, od.Quantity as CantidadCompras, round(p.Price * (od.Quantity),2) as Total
#FROM Customers c
#INNER JOIN Orders o ON c.CustomersID = o.CustomersID
#INNER JOIN OrderDetails od ON o.OrderID = od.OrderID
#INNER JOIN Products p on p.ProductID = od.ProductID
#Group By c.CustomerName
#having Total > 10000
#Where c.Country = 'USA' or c.Country = 'Germany' 



##SELECT e.FirstName as NombreVendedor, e.LastName as ApellidoVendedor, c.CustomerName as NombreCLiente, c.City as CiudadCliente, c.Country as PaisCliente, o.OrderID, p.ProductName as NombreProducto, p.Unit as Presentacion, od.Quantity as Cantidad, p.Price as Precio, round(p.Price * (od.Quantity),2) as Total
#FROM Categories ct
#INNER JOIN Products p  ON p.CategoryID = ct.CategoryID
#INNER JOIN Customers c ON c.CustomerID = o.CustomerID
#INNER JOIN Employees e ON e.EmployeeID = o.EmployeeID
#INNER JOIN OrderDetails od ON p.ProductID = od.ProductID
#INNER JOIN Orders o ON o.OrderID = od.OrderID
#Where e.LastName = 'King' and (c.Country ='Spain' or c.Country ='Mexico' or c.Country ='Canada')


##
#El aprendizaje automatico o Machine Learning son una serie de tecnicas que, mediante la implementacion de
#algoritmos permiten que una computadora aprenda por si sola. Estos algortimos se retrtoalimentan constantementa
#con los datos que posee y se le van cargando.
#Permite obtener patrones que no son visibles a cierta vista a partir de tendencias que la maquina encuentra
#en los datos; puede predecir el valor de ciertas variables en el futuro con informacion pasada; informar 
#ante la deteccion de anomalias; asi como tambien llegar a desarrollar inteligencias artificiales.
#Las ventajas son enormes y numerosas.
#Los dos tipos mas utilizados en la actualidad son
#Latent Dirichlet Allocation (LDA): Trabaja con datos de Twitter y permite descubrir temas ocultos en esta gran
#cantidad de datos, Trabaja con datos no estructurados (NoSQL). Dadas las palabras y los documentos que son
#observables, el objetivo es inferir la estructura de temas latente, ajustando de forma iterativa la
#importancia relativa de los temas en los documentos y las palabras en los temas.
#El otro metodo utilizado es:
#Regresion Logistica: trabaja con sets de datos de creditos
#Se asgina cada observacion a una categoria o clase, se utiliza una clasificacion binaria (0 clase negativa y
#1 la clase positiva). Se modela la probabilidad de que la observacion pertenezca a una clase o a otra a partir
#de la funcion logistica y se asigna la clase mas probable




##Defina el aprendizaje automático y mencione qué ventajas se pueden obtener de su utilización. Explique brevemente los dos tipos de métodos más utilizados en la actualidad.
#Tecnicas estadisticas y de programacion que tienen el objetivo derealizar el analisis automaico de grandes volumenes de datos
#y detectar patrones sin intervencion humana. Utilizan info pasada y se van retroalimentando continuamente para que la "Maquina aprenda"
##Permite obtener patrones que no son visibles a cierta vista a partir de tendencias que la maquina encuentra, deteccion de valores atipicos
#o anomalias, sistemas de recomendacion, autos que se manejan solos, segmentacion de clietnes, clasificacion de paginas web, autenticacion de personas,
#predecir el valor de ciertas variables en el futuro, etc
#Las ventajas son enormes y numerosas.
#Los dos tipos mas utilizados en la actualidad son:
  #Aprendizaje supervisado: le asigna a cada uno de los datos una salida o etiqueta. La base de conocimiento del programa esta dada por etiquetados anteriores,
#revisa que etiqueta recibio cada valor en el pasada y compara estos valores con los nuevos ingresados para asignarles una etiqueta tambien.
  #Aprendizaje NO supeervisado: Datos sin etiquetas asociadas. El sistema es capaz de reconocer patrones de las entradas anteriores para poder etiquetar las nuevas entradas
#El conjunto de datos historicos esta formado solo por entradas al sistema, no tienen etiquetas
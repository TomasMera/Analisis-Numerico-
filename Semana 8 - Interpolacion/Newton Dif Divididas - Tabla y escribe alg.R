rm(list=ls())

#creo 2 vectores
x_i<-c(1/360,1/48,1/12,2/12,3/12,6/12,12/12)
f_i<-c(1.82075,1.881,1.978,2.00438,2.027,1.95063,1.85313)

#creo función interpolación newton con los dos vectores creados
InterpolacionNewton_3.2 <- function(x_i, f_i) {
  
  n <- length(x_i)
  q <- matrix(data = NA, n, n)
  q[,1] <- f_i
  
  #Paso 1
  
  for (i in 2:n) {
    for (j in i:n) {
      q[j,i] <- (q[j,i-1] - q[j-1,i-1]) / (x_i[j] - x_i[j-i+1])
    }
  }
  
  #Paso 2
  
  
  return(q)
}

#TABLA DIFERENCIAS DIVIDIDAS

InterpolacionNewton_3.2(x_i,f_i)

#Polinomio:
#cambiar el numero de F[]por la cantidad del triangulo
p <- InterpolacionNewton_3.2(x_i,f_i)
p<- c(p[1,1],p[2,2],p[3,3],p[4,4],p[5,5],p[6,6],p[7,7])

r <- as.character(round(p[1], 7))
ri <- ''
#cambiar el intervalo de 2:"". Va la cantidad de columnas del triangulo
for (i in 2:7) {
  ri <- paste(ri, '*(x - ', x_i[i-1], ')', sep = '', collapse = '')
  
  r <- paste(r, ' + ', round(p[i], 7), ri, sep = '', collapse = '')
}

r <- paste('p(x)=',r)

r
#copiar el polinomio y crear una función para verificar los valores
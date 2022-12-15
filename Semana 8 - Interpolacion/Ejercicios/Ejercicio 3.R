# Ejercicio 3

#Polinomio A ----
x_i <-c(0,0.25,0.5,0.75)
f_i <-c(1,1.64872,2.71828,4.48169)

InterpolacionNewton_3.2(x_i,f_i)

p <- InterpolacionNewton_3.2(x_i,f_i)
p<- c(p[1,1],p[2,2],p[3,3],p[4,4])

r <- as.character(round(p[1], 4))
ri <- ''
#cambiar el intervalo de 2:"". Va la cantidad de columnas del triangulo
for (i in 2:4) {
  ri <- paste(ri, '*(x - ', x_i[i-1], ')', sep = '', collapse = '')
  
  r <- paste(r, ' + ', round(p[i], 7), ri, sep = '', collapse = '')
}

r <- paste('p(x)=',r)

r
#Polinomio B ----
x_i = c(-0.5,-0.25,0.25,0.5)
f_i = c(1.93750,1.33203,0.800781,0.687500)

InterpolacionNewton_3.2(x_i,f_i)

p <- InterpolacionNewton_3.2(x_i,f_i)
p<- c(p[1,1],p[2,2],p[3,3],p[4,4])

r <- as.character(round(p[1], 4))
ri <- ''
#cambiar el intervalo de 2:"". Va la cantidad de columnas del triangulo
for (i in 2:4) {
  ri <- paste(ri, '*(x - ', x_i[i-1], ')', sep = '', collapse = '')
  
  r <- paste(r, ' + ', round(p[i], 7), ri, sep = '', collapse = '')
}

r <- paste('p(x)=',r)

r
#Polinomio C ----
x_i = c(0.1,0.2,0.3,0.4)
f_i = c(-0.29004986,-0.56079734,-0.81401972,-1.0526302)

InterpolacionNewton_3.2(x_i,f_i)


p <- InterpolacionNewton_3.2(x_i,f_i)
p<- c(p[1,1],p[2,2],p[3,3],p[4,4])

r <- as.character(round(p[1], 4))
ri <- ''
#cambiar el intervalo de 2:"". Va la cantidad de columnas del triangulo
for (i in 2:4) {
  ri <- paste(ri, '*(x - ', x_i[i-1], ')', sep = '', collapse = '')
  
  r <- paste(r, ' + ', round(p[i], 7), ri, sep = '', collapse = '')
}

r <- paste('p(x)=',r)

r


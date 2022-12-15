##Ejercicio 2
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



## A ----
x_i <-c(0,0.25,0.5,0.75)
f_i <-c(1,1.64872,2.71828,4.48169)

InterpolacionNewton_3.2(x_i,f_i)

## B ----
x_i = c(-0.5,-0.25,0.25,0.5)
f_i = c(1.93750,1.33203,0.800781,0.687500)

InterpolacionNewton_3.2(x_i,f_i)

## C ----
x_i = c(0.1,0.2,0.3,0.4)
f_i = c(-0.29004986,-0.56079734,-0.81401972,-1.0526302)

InterpolacionNewton_3.2(x_i,f_i)

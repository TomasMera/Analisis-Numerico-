##Ejercicio 1

# 1) Cargamos el Algoritmos ----
Lagrange<-function(x,x_dado,f_dado){ ###
  Pol<-0
  n<-length(x_dado)
  Aux<-c(rep(1,n))
  for (k in 1:n){
    for(i in 1:n){
      if(k != i){
        Aux[k]<-Aux[k]*((x-x_dado[i])/(x_dado[k]-x_dado[i]))
      }
    }
    
    Pol<- Pol+f_dado[k]*Aux[k]
  }
  return(Pol)
}

# A ----

x= 0.43
# Polinomio de grado 1

x_dado <- c(0.25,0.5)
f_dado <- c(1.64872,2.71828)

#Polinomio de grado 2
x_dado = c(0.25,0.5,0.75)
f_dado = c(1.64872,2.71828,4.48169)

#Otro polinomio de grado 2
x_dado = c(0,0.25,0.5)
f_dado = c(1,1.64872,2.71828)

##polinomio de grado 3
x_dado = c(0,0.25,0.5,0.75)
f_dado = c(1,1.64872,2.71828,4.48169)


# Resultado 
Lagrange(x,x_dado,f_dado) 


# B ----

x= 0
# Polinomio de grado 1

x_dado <- c(-0.25,0.25)
f_dado <- c(1.33203,0.800781)

#Polinomio de grado 2
x_dado = c(-0.5,-0.25,0.25)
f_dado = c(1.93750,1.33203,0.800781)

#Otro polinomio de grado 2
x_dado = c(-0.25,0.25,0.5)
f_dado = c(1.33203,0.800781,0.687500)

##polinomio de grado 3
x_dado = c(-0.5,-0.25,0.25,0.5)
f_dado = c(1.93750,1.33203,0.800781,0.687500)


# Resultado 
Lagrange(x,x_dado,f_dado) 


# C ----
x= 0.18
# Polinomio de grado 1

x_dado <- c(0.1,0.2)
f_dado <- c(-0.29004986,-0.56079734)

#Polinomio de grado 2
x_dado = c(0.1,0.2,0.3)
f_dado = c(-0.29004986,-0.56079734,-0.81401972)

##polinomio de grado 3
x_dado = c(0.1,0.2,0.3,0.4)
f_dado = c(-0.29004986,-0.56079734,-0.81401972,-1.0526302)


# Resultado 
Lagrange(x,x_dado,f_dado) 

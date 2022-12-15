##Segun el Teorema 2.1 las cotas del numero de iteraciones van a estar dadas por
##  |Pn - p| <= (b-a) / 2^n   n>=1

##quiero una exactitud de 10^-3, a la solucion x^3+x-4
#que se encuentra en el intervalo [1,4]
#|Pn - p| = 10^-3


polinomio <- function(x){
  f <-x^3 + x - 4
  return(f)
}
#Biseccion

Met_Biseccion = function(a,b,Tol,N){ 
  #Step 1 
  i = 1 
  FA = polinomio(a)
  #Step 2
  while (1 <= N){ 
    #Step 3
    p = a + (b-a)/2 
    FP = polinomio (p) 
    #Step 4
    if (FP==0 | (b-a)/2 < Tol){
      p<- c(p,i) #Tira la cantidad de iteraciones
      return (p)
    }
    #Step 5
    i = i + 1
    #Step 6
    if (FP * FA> 0){
      a = p 
      FA = FP 
    } else { 
      b = p 
    }
  }
  #Step 7
  return (paste("El metodo fall??? luego de ", N, "iteraciones"))
}


#Raiz con este grado de exactitud
Raiz_Biseccion(1,4,10^-3,100)  ##1.378662
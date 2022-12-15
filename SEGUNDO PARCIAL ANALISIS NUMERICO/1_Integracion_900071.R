#### INTEGRACION ####
#TOMAS MERA 900071
#1.1 ----
## ALGORITMOS

Int_Trapecio = function(a, b){
  h = (b - a)/1
  xo = a
  x1 = b
  
  result = (h/2)*(fn(xo) + fn(x1))
  return(result)
}

Int_Simpson <- function(a,b){
  h = (b - a) /2
  xo = a
  x2 = b
  
  x1 = (xo + x2)/2
  #x1a = a + (b -a)/2
  
  result = (h/3)*(fn(xo) + 4*fn(x1) + fn(x2))
  return(result)
}

Int_Simpson38 <- function(a,b){
  h = (b - a)/3
  xo = a
  x3 = b
  
  x1 = xo + h
  x2 = xo + 2*h
  
  result = ((3/8)*h)*(fn(xo) + 3*fn(x1) + 3*fn(x2) + fn(x3))
  return(result)
}


alpha = 2.35
theeta = 1.33

Gamma_Alpha = gamma(alpha)

fn <- function(x){
  val <- ((theeta / x)^alpha  * exp(-theeta /x)) / (x * Gamma_Alpha)
  return(val)
}

#Con metodo del trapecio

Prob_Trapecio = Int_Trapecio(46.89,84.88)
Prob_Trapecio
print("Utiliza los nodos y0, y1")

#Con metodo de Simpson
Prob_Simpson = Int_Simpson(46.89,84.88)
Prob_Simpson
print("Utiliza los nodos y0, y1, y2")

#con metodo Simpson38
Prob_Simpson38 = Int_Simpson38(46.89,84.88)
Prob_Simpson38
print("Utiliza los nodos y0, y1, y2, y3")



## 1.2 ----
simpson.compuesto = function(a, b, n){
  if((n %% 2) == 0){
    #Paso 1
    h = (b-a)/n
    
    #Paso 2
    xio = fn(a) + fn(b)
    xi1 = 0 #Suma impar
    xi2 = 0 #Suma par
    
    #Paso 3
    for (i in 1:(n-1)) {
      #Paso 4
      x = a + i*h
      #Paso 5 
      if((i %% 2) == 0){
        xi2 = xi2 + fn(x)
      }else{
        xi1 = xi1 + fn(x)
      }
    }
    #Paso 6
    val = h*(xio + 2*xi2 + 4*xi1)/3
    return(val)
  }
  else return("n debe ser par")
}

n = 21 # Como el algortimo necesita un n par, tomo 22
n=22
Prob_Simpson_Compuesto <- simpson.compuesto(46.89,84.88,n)
Prob_Simpson_Compuesto
print("Utiliza los nodos desde y0 hasta yn, es decir, 23 nodos")


###Cota del Error
alim<-46.89   #cargar límite inferior intervalo
blim<-84.88  #cargar límite superior intervalo

d1sc<-D(expression(((theeta / x)^alpha  * exp(-theeta /x)) / (x * Gamma_Alpha)),"x") #cargar función que desea derivar
d2sc<-D(d1sc,"x")
d3sc<-D(d2sc,"x")
d4sc<-D(d3sc,"x")

d4sc

derivadasc4<-function(x){
  f<-((theeta/x)^alpha * (((exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - 
                              exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) * (theeta/x^2) - 
                             exp(-theeta/x) * (theeta/x^2) * (theeta * (2 * x)/(x^2)^2) - 
                             (exp(-theeta/x) * (theeta/x^2) * (theeta * (2 * x)/(x^2)^2) + 
                                exp(-theeta/x) * (theeta * 2/(x^2)^2 - theeta * (2 * 
                                                                                   x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2))) * (theeta/x^2) - 
                            (exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * 
                               (theeta * (2 * x)/(x^2)^2)) * (theeta * (2 * x)/(x^2)^2) - 
                            ((exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * 
                                (theeta * (2 * x)/(x^2)^2)) * (theeta * (2 * x)/(x^2)^2) + 
                               exp(-theeta/x) * (theeta/x^2) * (theeta * 2/(x^2)^2 - 
                                                                  theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2)) - 
                            ((exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * 
                                (theeta * (2 * x)/(x^2)^2)) * (theeta * (2 * x)/(x^2)^2) + 
                               exp(-theeta/x) * (theeta/x^2) * (theeta * 2/(x^2)^2 - 
                                                                  theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2) + 
                               (exp(-theeta/x) * (theeta/x^2) * (theeta * 2/(x^2)^2 - 
                                                                   theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2) - 
                                  exp(-theeta/x) * (theeta * 2 * (2 * (2 * x * (x^2)))/((x^2)^2)^2 + 
                                                      ((theeta * 2 * (2 * (2 * x * (x^2))) + theeta * 
                                                          (2 * x) * (2 * (2 * (x^2) + 2 * x * (2 * x))))/((x^2)^2)^2 - 
                                                         theeta * (2 * x) * (2 * (2 * x * (x^2))) * 
                                                         (2 * (2 * (2 * x * (x^2)) * ((x^2)^2)))/(((x^2)^2)^2)^2))))) - 
        (theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * ((exp(-theeta/x) * 
                                                              (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * (theeta * 
                                                                                                                (2 * x)/(x^2)^2)) * (theeta/x^2) - exp(-theeta/x) * (theeta/x^2) * 
                                                             (theeta * (2 * x)/(x^2)^2) - (exp(-theeta/x) * (theeta/x^2) * 
                                                                                             (theeta * (2 * x)/(x^2)^2) + exp(-theeta/x) * (theeta * 
                                                                                                                                              2/(x^2)^2 - theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2))) - 
        ((theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * ((exp(-theeta/x) * 
                                                               (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * (theeta * 
                                                                                                                 (2 * x)/(x^2)^2)) * (theeta/x^2) - exp(-theeta/x) * (theeta/x^2) * 
                                                              (theeta * (2 * x)/(x^2)^2) - (exp(-theeta/x) * (theeta/x^2) * 
                                                                                              (theeta * (2 * x)/(x^2)^2) + exp(-theeta/x) * (theeta * 
                                                                                                                                               2/(x^2)^2 - theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2))) - 
           ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + 
              (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
              (alpha * (theeta/x^2))) * (exp(-theeta/x) * (theeta/x^2) * 
                                           (theeta/x^2) - exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2))) - 
        ((theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * ((exp(-theeta/x) * 
                                                               (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * (theeta * 
                                                                                                                 (2 * x)/(x^2)^2)) * (theeta/x^2) - exp(-theeta/x) * (theeta/x^2) * 
                                                              (theeta * (2 * x)/(x^2)^2) - (exp(-theeta/x) * (theeta/x^2) * 
                                                                                              (theeta * (2 * x)/(x^2)^2) + exp(-theeta/x) * (theeta * 
                                                                                                                                               2/(x^2)^2 - theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2))) - 
           ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + 
              (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
              (alpha * (theeta/x^2))) * (exp(-theeta/x) * (theeta/x^2) * 
                                           (theeta/x^2) - exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) - 
           (((theeta/x)^(alpha - 1) * (alpha * (theeta * 2/(x^2)^2 - 
                                                  theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2)) - 
               (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
               (alpha * (theeta * (2 * x)/(x^2)^2)) - ((theeta/x)^((alpha - 
                                                                      1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * 
                                                                                                                  (theeta * (2 * x)/(x^2)^2)) + ((theeta/x)^((alpha - 
                                                                                                                                                                1) - 1) * ((alpha - 1) * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                                                   (theeta/x)^(((alpha - 1) - 1) - 1) * (((alpha - 1) - 
                                                                                                                                                                                            1) * (theeta/x^2)) * ((alpha - 1) * (theeta/x^2))) * 
                                                         (alpha * (theeta/x^2)))) * (exp(-theeta/x) * (theeta/x^2)) + 
              ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * 
                                                              x)/(x^2)^2)) + (theeta/x)^((alpha - 1) - 1) * 
                 ((alpha - 1) * (theeta/x^2)) * (alpha * (theeta/x^2))) * 
              (exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - 
                 exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)))) - 
        ((theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * ((exp(-theeta/x) * 
                                                               (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * (theeta * 
                                                                                                                 (2 * x)/(x^2)^2)) * (theeta/x^2) - exp(-theeta/x) * (theeta/x^2) * 
                                                              (theeta * (2 * x)/(x^2)^2) - (exp(-theeta/x) * (theeta/x^2) * 
                                                                                              (theeta * (2 * x)/(x^2)^2) + exp(-theeta/x) * (theeta * 
                                                                                                                                               2/(x^2)^2 - theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2))) - 
           ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + 
              (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
              (alpha * (theeta/x^2))) * (exp(-theeta/x) * (theeta/x^2) * 
                                           (theeta/x^2) - exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) - 
           (((theeta/x)^(alpha - 1) * (alpha * (theeta * 2/(x^2)^2 - 
                                                  theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2)) - 
               (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
               (alpha * (theeta * (2 * x)/(x^2)^2)) - ((theeta/x)^((alpha - 
                                                                      1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * 
                                                                                                                  (theeta * (2 * x)/(x^2)^2)) + ((theeta/x)^((alpha - 
                                                                                                                                                                1) - 1) * ((alpha - 1) * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                                                   (theeta/x)^(((alpha - 1) - 1) - 1) * (((alpha - 1) - 
                                                                                                                                                                                            1) * (theeta/x^2)) * ((alpha - 1) * (theeta/x^2))) * 
                                                         (alpha * (theeta/x^2)))) * (exp(-theeta/x) * (theeta/x^2)) + 
              ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * 
                                                              x)/(x^2)^2)) + (theeta/x)^((alpha - 1) - 1) * 
                 ((alpha - 1) * (theeta/x^2)) * (alpha * (theeta/x^2))) * 
              (exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - 
                 exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2))) - 
           (((theeta/x)^(alpha - 1) * (alpha * (theeta * 2/(x^2)^2 - 
                                                  theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2)) - 
               (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
               (alpha * (theeta * (2 * x)/(x^2)^2)) - ((theeta/x)^((alpha - 
                                                                      1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * 
                                                                                                                  (theeta * (2 * x)/(x^2)^2)) + ((theeta/x)^((alpha - 
                                                                                                                                                                1) - 1) * ((alpha - 1) * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                                                   (theeta/x)^(((alpha - 1) - 1) - 1) * (((alpha - 1) - 
                                                                                                                                                                                            1) * (theeta/x^2)) * ((alpha - 1) * (theeta/x^2))) * 
                                                         (alpha * (theeta/x^2)))) * (exp(-theeta/x) * (theeta/x^2)) - 
              ((theeta/x)^(alpha - 1) * (alpha * (theeta * 2 * 
                                                    (2 * (2 * x * (x^2)))/((x^2)^2)^2 + ((theeta * 
                                                                                            2 * (2 * (2 * x * (x^2))) + theeta * (2 * x) * 
                                                                                            (2 * (2 * (x^2) + 2 * x * (2 * x))))/((x^2)^2)^2 - 
                                                                                           theeta * (2 * x) * (2 * (2 * x * (x^2))) * (2 * 
                                                                                                                                         (2 * (2 * x * (x^2)) * ((x^2)^2)))/(((x^2)^2)^2)^2))) + 
                 (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * 
                                                   (theeta/x^2)) * (alpha * (theeta * 2/(x^2)^2 - 
                                                                               theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2)) + 
                 ((theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * 
                                                    (theeta/x^2)) * (alpha * (theeta * 2/(x^2)^2 - 
                                                                                theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2)) - 
                    ((theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * 
                                                       (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^(((alpha - 
                                                                                                     1) - 1) - 1) * (((alpha - 1) - 1) * (theeta/x^2)) * 
                       ((alpha - 1) * (theeta/x^2))) * (alpha * 
                                                          (theeta * (2 * x)/(x^2)^2))) + ((theeta/x)^((alpha - 
                                                                                                         1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * 
                                                                                                                                                     (theeta * 2/(x^2)^2 - theeta * (2 * x) * (2 * 
                                                                                                                                                                                                 (2 * x * (x^2)))/((x^2)^2)^2)) - ((theeta/x)^((alpha - 
                                                                                                                                                                                                                                                  1) - 1) * ((alpha - 1) * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                                                                                                                                     (theeta/x)^(((alpha - 1) - 1) - 1) * (((alpha - 
                                                                                                                                                                                                                                                                               1) - 1) * (theeta/x^2)) * ((alpha - 1) * (theeta/x^2))) * 
                                                                                            (alpha * (theeta * (2 * x)/(x^2)^2)) + (((theeta/x)^((alpha - 
                                                                                                                                                    1) - 1) * ((alpha - 1) * (theeta * 2/(x^2)^2 - 
                                                                                                                                                                                theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2)) - 
                                                                                                                                       (theeta/x)^(((alpha - 1) - 1) - 1) * (((alpha - 
                                                                                                                                                                                 1) - 1) * (theeta/x^2)) * ((alpha - 1) * (theeta * 
                                                                                                                                                                                                                             (2 * x)/(x^2)^2)) - ((theeta/x)^(((alpha - 
                                                                                                                                                                                                                                                                  1) - 1) - 1) * (((alpha - 1) - 1) * (theeta/x^2)) * 
                                                                                                                                                                                                                                                    ((alpha - 1) * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                                                                                                                                                    ((theeta/x)^(((alpha - 1) - 1) - 1) * (((alpha - 
                                                                                                                                                                                                                                                                                               1) - 1) * (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^((((alpha - 
                                                                                                                                                                                                                                                                                                                                                        1) - 1) - 1) - 1) * ((((alpha - 1) - 1) - 1) * 
                                                                                                                                                                                                                                                                                                                                                                               (theeta/x^2)) * (((alpha - 1) - 1) * (theeta/x^2))) * 
                                                                                                                                                                                                                                                    ((alpha - 1) * (theeta/x^2)))) * (alpha * (theeta/x^2)) - 
                                                                                                                                      ((theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * 
                                                                                                                                                                         (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^(((alpha - 
                                                                                                                                                                                                                       1) - 1) - 1) * (((alpha - 1) - 1) * (theeta/x^2)) * 
                                                                                                                                         ((alpha - 1) * (theeta/x^2))) * (alpha * (theeta * 
                                                                                                                                                                                     (2 * x)/(x^2)^2))))) * exp(-theeta/x) + (((theeta/x)^(alpha - 
                                                                                                                                                                                                                                             1) * (alpha * (theeta * 2/(x^2)^2 - theeta * (2 * 
                                                                                                                                                                                                                                                                                             x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2)) - (theeta/x)^((alpha - 
                                                                                                                                                                                                                                                                                                                                                       1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * 
                                                                                                                                                                                                                                                                                                                                                                                                   (theeta * (2 * x)/(x^2)^2)) - ((theeta/x)^((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                 1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             (theeta * (2 * x)/(x^2)^2)) + ((theeta/x)^((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           1) - 1) * ((alpha - 1) * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (theeta/x)^(((alpha - 1) - 1) - 1) * (((alpha - 1) - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       1) * (theeta/x^2)) * ((alpha - 1) * (theeta/x^2))) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                    (alpha * (theeta/x^2)))) * (exp(-theeta/x) * (theeta/x^2)) + 
                                                                                                                                                                                                                                ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * 
                                                                                                                                                                                                                                                                                x)/(x^2)^2)) + (theeta/x)^((alpha - 1) - 1) * 
                                                                                                                                                                                                                                   ((alpha - 1) * (theeta/x^2)) * (alpha * (theeta/x^2))) * 
                                                                                                                                                                                                                                (exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - 
                                                                                                                                                                                                                                   exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2))))))/(x * 
                                                                                                                                                                                                                                                                                       Gamma_Alpha) - ((theeta/x)^alpha * ((exp(-theeta/x) * (theeta/x^2) * 
                                                                                                                                                                                                                                                                                                                              (theeta/x^2) - exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) * 
                                                                                                                                                                                                                                                                                                                             (theeta/x^2) - exp(-theeta/x) * (theeta/x^2) * (theeta * 
                                                                                                                                                                                                                                                                                                                                                                               (2 * x)/(x^2)^2) - (exp(-theeta/x) * (theeta/x^2) * (theeta * 
                                                                                                                                                                                                                                                                                                                                                                                                                                      (2 * x)/(x^2)^2) + exp(-theeta/x) * (theeta * 2/(x^2)^2 - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2))) - 
                                                                                                                                                                                                                                                                                                         (theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * 
                                                                                                                                                                                                                                                                                                                                                              (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * (theeta * 
                                                                                                                                                                                                                                                                                                                                                                                                                (2 * x)/(x^2)^2)) - ((theeta/x)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (theeta/x^2)) * (exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) - ((theeta/x)^(alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * (theeta/x^2))) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                       (exp(-theeta/x) * (theeta/x^2))) - ((theeta/x)^(alpha - 1) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             (alpha * (theeta/x^2)) * (exp(-theeta/x) * (theeta/x^2) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         (theeta/x^2) - exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (alpha * (theeta/x^2))) * (exp(-theeta/x) * (theeta/x^2)) - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             (((theeta/x)^(alpha - 1) * (alpha * (theeta * 2/(x^2)^2 - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2)) - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (alpha * (theeta * (2 * x)/(x^2)^2)) - ((theeta/x)^((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * (theeta * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             (2 * x)/(x^2)^2)) + ((theeta/x)^((alpha - 1) - 1) * ((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1) * (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^(((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        1) - 1) - 1) * (((alpha - 1) - 1) * (theeta/x^2)) * ((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                1) * (theeta/x^2))) * (alpha * (theeta/x^2)))) * exp(-theeta/x) + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   (alpha * (theeta/x^2))) * (exp(-theeta/x) * (theeta/x^2))))) * 
    Gamma_Alpha/(x * Gamma_Alpha)^2 - (((theeta/x)^alpha * ((exp(-theeta/x) * 
                                                               (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * (theeta * 
                                                                                                                 (2 * x)/(x^2)^2)) * (theeta/x^2) - exp(-theeta/x) * (theeta/x^2) * 
                                                              (theeta * (2 * x)/(x^2)^2) - (exp(-theeta/x) * (theeta/x^2) * 
                                                                                              (theeta * (2 * x)/(x^2)^2) + exp(-theeta/x) * (theeta * 2/(x^2)^2 - 
                                                                                                                                               theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2))) - 
                                          (theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * 
                                                                                               (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * (theeta * 
                                                                                                                                                 (2 * x)/(x^2)^2)) - ((theeta/x)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                  (theeta/x^2)) * (exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - 
                                                                                                                                                                                                                     exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) - ((theeta/x)^(alpha - 
                                                                                                                                                                                                                                                                                   1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^((alpha - 
                                                                                                                                                                                                                                                                                                                                              1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * (theeta/x^2))) * 
                                                                                                                                                                        (exp(-theeta/x) * (theeta/x^2))) - ((theeta/x)^(alpha - 1) * 
                                                                                                                                                                                                              (alpha * (theeta/x^2)) * (exp(-theeta/x) * (theeta/x^2) * 
                                                                                                                                                                                                                                          (theeta/x^2) - exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) - 
                                                                                                                                                                                                              ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                                                                                                                 (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
                                                                                                                                                                                                                 (alpha * (theeta/x^2))) * (exp(-theeta/x) * (theeta/x^2)) - 
                                                                                                                                                                                                              (((theeta/x)^(alpha - 1) * (alpha * (theeta * 2/(x^2)^2 - 
                                                                                                                                                                                                                                                     theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2)) - 
                                                                                                                                                                                                                  (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
                                                                                                                                                                                                                  (alpha * (theeta * (2 * x)/(x^2)^2)) - ((theeta/x)^((alpha - 
                                                                                                                                                                                                                                                                         1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * (theeta * 
                                                                                                                                                                                                                                                                                                                              (2 * x)/(x^2)^2)) + ((theeta/x)^((alpha - 1) - 1) * ((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                      1) * (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^(((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                         1) - 1) - 1) * (((alpha - 1) - 1) * (theeta/x^2)) * ((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1) * (theeta/x^2))) * (alpha * (theeta/x^2)))) * exp(-theeta/x) + 
                                                                                                                                                                                                                 ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                                                                                                                    (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
                                                                                                                                                                                                                    (alpha * (theeta/x^2))) * (exp(-theeta/x) * (theeta/x^2))))) * 
                                         Gamma_Alpha/(x * Gamma_Alpha)^2 - ((theeta/x)^alpha * (exp(-theeta/x) * 
                                                                                                  (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * (theeta * 
                                                                                                                                                    (2 * x)/(x^2)^2)) - (theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * 
                                                                              (exp(-theeta/x) * (theeta/x^2)) - ((theeta/x)^(alpha - 1) * 
                                                                                                                   (alpha * (theeta/x^2)) * (exp(-theeta/x) * (theeta/x^2)) - 
                                                                                                                   ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                      (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
                                                                                                                      (alpha * (theeta/x^2))) * exp(-theeta/x))) * Gamma_Alpha * 
                                         (2 * (Gamma_Alpha * (x * Gamma_Alpha)))/((x * Gamma_Alpha)^2)^2) - 
    (((theeta/x)^alpha * ((exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - 
                             exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) * (theeta/x^2) - 
                            exp(-theeta/x) * (theeta/x^2) * (theeta * (2 * x)/(x^2)^2) - 
                            (exp(-theeta/x) * (theeta/x^2) * (theeta * (2 * x)/(x^2)^2) + 
                               exp(-theeta/x) * (theeta * 2/(x^2)^2 - theeta * (2 * 
                                                                                  x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2))) - (theeta/x)^(alpha - 
                                                                                                                                            1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * (theeta/x^2) * 
                                                                                                                                                                             (theeta/x^2) - exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) - 
        ((theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * 
                                                              (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * (theeta * 
                                                                                                                (2 * x)/(x^2)^2)) - ((theeta/x)^(alpha - 1) * (alpha * 
                                                                                                                                                                 (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^((alpha - 
                                                                                                                                                                                                              1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * 
                                                                                                                                                                                                                                                          (theeta/x^2))) * (exp(-theeta/x) * (theeta/x^2))) - 
        ((theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * 
                                                              (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * (theeta * 
                                                                                                                (2 * x)/(x^2)^2)) - ((theeta/x)^(alpha - 1) * (alpha * 
                                                                                                                                                                 (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^((alpha - 
                                                                                                                                                                                                              1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * 
                                                                                                                                                                                                                                                          (theeta/x^2))) * (exp(-theeta/x) * (theeta/x^2)) - 
           (((theeta/x)^(alpha - 1) * (alpha * (theeta * 2/(x^2)^2 - 
                                                  theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2)) - 
               (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * 
                                                 (theeta/x^2)) * (alpha * (theeta * (2 * x)/(x^2)^2)) - 
               ((theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * 
                                                  (theeta/x^2)) * (alpha * (theeta * (2 * x)/(x^2)^2)) + 
                  ((theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * 
                                                     (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^(((alpha - 
                                                                                                   1) - 1) - 1) * (((alpha - 1) - 1) * (theeta/x^2)) * 
                     ((alpha - 1) * (theeta/x^2))) * (alpha * 
                                                        (theeta/x^2)))) * exp(-theeta/x) + ((theeta/x)^(alpha - 
                                                                                                          1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^((alpha - 
                                                                                                                                                                     1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * 
                                                                                                                                                                                                                 (theeta/x^2))) * (exp(-theeta/x) * (theeta/x^2))))) * 
       Gamma_Alpha/(x * Gamma_Alpha)^2 - ((theeta/x)^alpha * 
                                            (exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * 
                                               (theeta * (2 * x)/(x^2)^2)) - (theeta/x)^(alpha - 
                                                                                           1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * (theeta/x^2)) - 
                                            ((theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * 
                                                                                                  (theeta/x^2)) - ((theeta/x)^(alpha - 1) * (alpha * 
                                                                                                                                               (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^((alpha - 
                                                                                                                                                                                            1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * 
                                                                                                                                                                                                                                        (theeta/x^2))) * exp(-theeta/x))) * Gamma_Alpha * 
       (2 * (Gamma_Alpha * (x * Gamma_Alpha)))/((x * Gamma_Alpha)^2)^2 - 
       ((((theeta/x)^alpha * (exp(-theeta/x) * (theeta/x^2) * 
                                (theeta/x^2) - exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) - 
            (theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * 
            (exp(-theeta/x) * (theeta/x^2)) - ((theeta/x)^(alpha - 
                                                             1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * (theeta/x^2)) - 
                                                 ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * 
                                                                                                 x)/(x^2)^2)) + (theeta/x)^((alpha - 1) - 1) * 
                                                    ((alpha - 1) * (theeta/x^2)) * (alpha * (theeta/x^2))) * 
                                                 exp(-theeta/x))) * Gamma_Alpha * (2 * (Gamma_Alpha * 
                                                                                          (x * Gamma_Alpha))) + ((theeta/x)^alpha * (exp(-theeta/x) * 
                                                                                                                                       (theeta/x^2)) - (theeta/x)^(alpha - 1) * (alpha * 
                                                                                                                                                                                   (theeta/x^2)) * exp(-theeta/x)) * Gamma_Alpha * (2 * 
                                                                                                                                                                                                                                      (Gamma_Alpha * Gamma_Alpha)))/((x * Gamma_Alpha)^2)^2 - 
          ((theeta/x)^alpha * (exp(-theeta/x) * (theeta/x^2)) - 
             (theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * 
             exp(-theeta/x)) * Gamma_Alpha * (2 * (Gamma_Alpha * 
                                                     (x * Gamma_Alpha))) * (2 * (2 * (Gamma_Alpha * 
                                                                                        (x * Gamma_Alpha)) * ((x * Gamma_Alpha)^2)))/(((x * 
                                                                                                                                          Gamma_Alpha)^2)^2)^2)) - (((theeta/x)^alpha * 
                                                                                                                                                                       ((exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * 
                                                                                                                                                                           (theeta * (2 * x)/(x^2)^2)) * (theeta/x^2) - exp(-theeta/x) * 
                                                                                                                                                                          (theeta/x^2) * (theeta * (2 * x)/(x^2)^2) - (exp(-theeta/x) * 
                                                                                                                                                                                                                         (theeta/x^2) * (theeta * (2 * x)/(x^2)^2) + exp(-theeta/x) * 
                                                                                                                                                                                                                         (theeta * 2/(x^2)^2 - theeta * (2 * x) * (2 * (2 * x * 
                                                                                                                                                                                                                                                                          (x^2)))/((x^2)^2)^2))) - (theeta/x)^(alpha - 1) * 
                                                                                                                                                                       (alpha * (theeta/x^2)) * (exp(-theeta/x) * (theeta/x^2) * 
                                                                                                                                                                                                   (theeta/x^2) - exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) - 
                                                                                                                                                                       ((theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * 
                                                                                                                                                                                                                             (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * (theeta * 
                                                                                                                                                                                                                                                                               (2 * x)/(x^2)^2)) - ((theeta/x)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                                                                                (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^((alpha - 1) - 
                                                                                                                                                                                                                                                                                                                                                                            1) * ((alpha - 1) * (theeta/x^2)) * (alpha * (theeta/x^2))) * 
                                                                                                                                                                          (exp(-theeta/x) * (theeta/x^2))) - ((theeta/x)^(alpha - 
                                                                                                                                                                                                                            1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * (theeta/x^2) * 
                                                                                                                                                                                                                                                             (theeta/x^2) - exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) - 
                                                                                                                                                                                                                ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                                                                                                                   (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
                                                                                                                                                                                                                   (alpha * (theeta/x^2))) * (exp(-theeta/x) * (theeta/x^2)) - 
                                                                                                                                                                                                                (((theeta/x)^(alpha - 1) * (alpha * (theeta * 2/(x^2)^2 - 
                                                                                                                                                                                                                                                       theeta * (2 * x) * (2 * (2 * x * (x^2)))/((x^2)^2)^2)) - 
                                                                                                                                                                                                                    (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
                                                                                                                                                                                                                    (alpha * (theeta * (2 * x)/(x^2)^2)) - ((theeta/x)^((alpha - 
                                                                                                                                                                                                                                                                           1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * (theeta * 
                                                                                                                                                                                                                                                                                                                                (2 * x)/(x^2)^2)) + ((theeta/x)^((alpha - 1) - 1) * ((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                        1) * (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^(((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                           1) - 1) - 1) * (((alpha - 1) - 1) * (theeta/x^2)) * ((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   1) * (theeta/x^2))) * (alpha * (theeta/x^2)))) * exp(-theeta/x) + 
                                                                                                                                                                                                                   ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                                                                                                                      (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
                                                                                                                                                                                                                      (alpha * (theeta/x^2))) * (exp(-theeta/x) * (theeta/x^2))))) * 
                                                                                                                                                                      Gamma_Alpha/(x * Gamma_Alpha)^2 - ((theeta/x)^alpha * (exp(-theeta/x) * 
                                                                                                                                                                                                                               (theeta/x^2) * (theeta/x^2) - exp(-theeta/x) * (theeta * 
                                                                                                                                                                                                                                                                                 (2 * x)/(x^2)^2)) - (theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * 
                                                                                                                                                                                                           (exp(-theeta/x) * (theeta/x^2)) - ((theeta/x)^(alpha - 1) * 
                                                                                                                                                                                                                                                (alpha * (theeta/x^2)) * (exp(-theeta/x) * (theeta/x^2)) - 
                                                                                                                                                                                                                                                ((theeta/x)^(alpha - 1) * (alpha * (theeta * (2 * x)/(x^2)^2)) + 
                                                                                                                                                                                                                                                   (theeta/x)^((alpha - 1) - 1) * ((alpha - 1) * (theeta/x^2)) * 
                                                                                                                                                                                                                                                   (alpha * (theeta/x^2))) * exp(-theeta/x))) * Gamma_Alpha * 
                                                                                                                                                                      (2 * (Gamma_Alpha * (x * Gamma_Alpha)))/((x * Gamma_Alpha)^2)^2 - 
                                                                                                                                                                      ((((theeta/x)^alpha * (exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - 
                                                                                                                                                                                               exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) - (theeta/x)^(alpha - 
                                                                                                                                                                                                                                                            1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * (theeta/x^2)) - 
                                                                                                                                                                           ((theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * 
                                                                                                                                                                                                                                 (theeta/x^2)) - ((theeta/x)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                              (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^((alpha - 
                                                                                                                                                                                                                                                                                                                           1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * 
                                                                                                                                                                                                                                                                                                                                                                       (theeta/x^2))) * exp(-theeta/x))) * Gamma_Alpha * 
                                                                                                                                                                          (2 * (Gamma_Alpha * (x * Gamma_Alpha))) + ((theeta/x)^alpha * 
                                                                                                                                                                                                                       (exp(-theeta/x) * (theeta/x^2)) - (theeta/x)^(alpha - 
                                                                                                                                                                                                                                                                       1) * (alpha * (theeta/x^2)) * exp(-theeta/x)) * Gamma_Alpha * 
                                                                                                                                                                          (2 * (Gamma_Alpha * Gamma_Alpha)))/((x * Gamma_Alpha)^2)^2 - 
                                                                                                                                                                         ((theeta/x)^alpha * (exp(-theeta/x) * (theeta/x^2)) - 
                                                                                                                                                                            (theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * 
                                                                                                                                                                            exp(-theeta/x)) * Gamma_Alpha * (2 * (Gamma_Alpha * 
                                                                                                                                                                                                                    (x * Gamma_Alpha))) * (2 * (2 * (Gamma_Alpha * (x * 
                                                                                                                                                                                                                                                                      Gamma_Alpha)) * ((x * Gamma_Alpha)^2)))/(((x * Gamma_Alpha)^2)^2)^2) - 
                                                                                                                                                                      ((((theeta/x)^alpha * (exp(-theeta/x) * (theeta/x^2) * (theeta/x^2) - 
                                                                                                                                                                                               exp(-theeta/x) * (theeta * (2 * x)/(x^2)^2)) - (theeta/x)^(alpha - 
                                                                                                                                                                                                                                                            1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * (theeta/x^2)) - 
                                                                                                                                                                           ((theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * (exp(-theeta/x) * 
                                                                                                                                                                                                                                 (theeta/x^2)) - ((theeta/x)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                              (theeta * (2 * x)/(x^2)^2)) + (theeta/x)^((alpha - 
                                                                                                                                                                                                                                                                                                                           1) - 1) * ((alpha - 1) * (theeta/x^2)) * (alpha * 
                                                                                                                                                                                                                                                                                                                                                                       (theeta/x^2))) * exp(-theeta/x))) * Gamma_Alpha * 
                                                                                                                                                                          (2 * (Gamma_Alpha * (x * Gamma_Alpha))) + ((theeta/x)^alpha * 
                                                                                                                                                                                                                       (exp(-theeta/x) * (theeta/x^2)) - (theeta/x)^(alpha - 
                                                                                                                                                                                                                                                                       1) * (alpha * (theeta/x^2)) * exp(-theeta/x)) * Gamma_Alpha * 
                                                                                                                                                                          (2 * (Gamma_Alpha * Gamma_Alpha)) + ((theeta/x)^alpha * 
                                                                                                                                                                                                                 (exp(-theeta/x) * (theeta/x^2)) - (theeta/x)^(alpha - 
                                                                                                                                                                                                                                                                 1) * (alpha * (theeta/x^2)) * exp(-theeta/x)) * Gamma_Alpha * 
                                                                                                                                                                          (2 * (Gamma_Alpha * Gamma_Alpha)))/((x * Gamma_Alpha)^2)^2 - 
                                                                                                                                                                         (((theeta/x)^alpha * (exp(-theeta/x) * (theeta/x^2)) - 
                                                                                                                                                                             (theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * 
                                                                                                                                                                             exp(-theeta/x)) * Gamma_Alpha * (2 * (Gamma_Alpha * 
                                                                                                                                                                                                                     (x * Gamma_Alpha))) + ((theeta/x)^alpha * exp(-theeta/x)) * 
                                                                                                                                                                            Gamma_Alpha * (2 * (Gamma_Alpha * Gamma_Alpha))) * 
                                                                                                                                                                         (2 * (2 * (Gamma_Alpha * (x * Gamma_Alpha)) * ((x * 
                                                                                                                                                                                                                           Gamma_Alpha)^2)))/(((x * Gamma_Alpha)^2)^2)^2 - 
                                                                                                                                                                         (((((theeta/x)^alpha * (exp(-theeta/x) * (theeta/x^2)) - 
                                                                                                                                                                               (theeta/x)^(alpha - 1) * (alpha * (theeta/x^2)) * 
                                                                                                                                                                               exp(-theeta/x)) * Gamma_Alpha * (2 * (Gamma_Alpha * 
                                                                                                                                                                                                                       (x * Gamma_Alpha))) + ((theeta/x)^alpha * exp(-theeta/x)) * 
                                                                                                                                                                              Gamma_Alpha * (2 * (Gamma_Alpha * Gamma_Alpha))) * 
                                                                                                                                                                             (2 * (2 * (Gamma_Alpha * (x * Gamma_Alpha)) * ((x * 
                                                                                                                                                                                                                               Gamma_Alpha)^2))) + ((theeta/x)^alpha * exp(-theeta/x)) * 
                                                                                                                                                                             Gamma_Alpha * (2 * (Gamma_Alpha * (x * Gamma_Alpha))) * 
                                                                                                                                                                             (2 * (2 * (Gamma_Alpha * Gamma_Alpha) * ((x * Gamma_Alpha)^2) + 
                                                                                                                                                                                     2 * (Gamma_Alpha * (x * Gamma_Alpha)) * (2 * 
                                                                                                                                                                                                                                (Gamma_Alpha * (x * Gamma_Alpha))))))/(((x * 
                                                                                                                                                                                                                                                                           Gamma_Alpha)^2)^2)^2 - ((theeta/x)^alpha * exp(-theeta/x)) * 
                                                                                                                                                                            Gamma_Alpha * (2 * (Gamma_Alpha * (x * Gamma_Alpha))) * 
                                                                                                                                                                            (2 * (2 * (Gamma_Alpha * (x * Gamma_Alpha)) * ((x * 
                                                                                                                                                                                                                              Gamma_Alpha)^2))) * (2 * (2 * (2 * (Gamma_Alpha * 
                                                                                                                                                                                                                                                                    (x * Gamma_Alpha)) * ((x * Gamma_Alpha)^2)) * (((x * 
                                                                                                                                                                                                                                                                                                                       Gamma_Alpha)^2)^2)))/((((x * Gamma_Alpha)^2)^2)^2)^2)))
  return(f)
}

optimosc<-optimize(derivadasc4,c(alim,blim),maximum = T)
optsc<-abs(optimosc$objective)
optsc

error<-((blim-alim)/180)*(((blim-alim)/n)^4)*optsc
error



#1.3 ----
#La esperanza se calcula como la integral en el dominio de Y * F(Y)

fn <- function (x){
  val <- x * (((theeta / x)^alpha  * exp(-theeta /x)) / (x * Gamma_Alpha))
  return(val)
}

n= 414

Esperanza_y = simpson.compuesto(0,100000,n) #Como el dominio es de 0 a infinito, tomo 100000 para aproximar el infinito
Esperanza_y




#1.4----
#Calculamos E(Y^2)
fn <- function (x){
  val <- x^2 * ((theeta / x)^alpha  * exp(-theeta /x)) / (x * Gamma_Alpha)
  return(val)
}

trapecio.compuesto = function(a, b, n){
  h = (b-a)/n
  
  xio = fn(a) + fn(b)
  xi = 0
  for (i in 1:(n-1)) {
    x = a + i*h
    xi = xi + fn(x)
  }
  val = h*(xio + 2*xi)/2
  return(val)
}

Esperanza_y2 = trapecio.compuesto(0,100000,n)
Esperanza_y2

Varianza_Y = Esperanza_y2 - Esperanza_y^2
Varianza_Y


#Cota del error de Esperanza_Y2
alim<-0  #cargar límite inferior intervalo
blim<-100000  #cargar límite superior intervalo

X <- seq(from = alim, to = blim , by = 0.001)
fx <- eval(D(D(expression(x^2 * ((theeta / x)^alpha  * exp(-theeta /x)) / (x * Gamma_Alpha)), "x"),"x"))

n <- 1000
h <- (blim - alim) / n

ERROR <- ((blim - alim)/12) * (h^2) * max(abs(fx))
ERROR


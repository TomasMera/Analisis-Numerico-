#### EJERCICIO 2 ####

Int_Trapecio = function(a, b,fn){
  h = (b - a)/1
  xo = a
  x1 = b
    
  result = (h/2)*(fn(xo) + fn(x1))
  return(result)
}

Int_Simpson <- function(a,b,fn){
  h = (b - a) /2
  xo = a
  x2 = b
  
  x1 = (xo + x2)/2
  #x1a = a + (b -a)/2
  
  result = (h/3)*(fn(xo) + 4*fn(x1) + fn(x2))
  return(result)
}

Int_Simpson38 <- function(a,b,fn){
  h = (b - a)/3
  xo = a
  x3 = b
  
  x1 = xo + h
  x2 = xo + 2*h
  
  result = ((3/8)*h)*(fn(xo) + 3*fn(x1) + 3*fn(x2) + fn(x3))
  return(result)
}

alpha = 2.52
theeta = 1.32

#2.1 ----
a = 0.25
b = 5

#Con trapecio
gamma <- function(x){
  val <- x^(alpha -1) * exp(-x)
  return(val)
}

Gamma_Alpha_Trapecio = Int_Trapecio(0, 10000,gamma) #Tomo de 0 a 10000, aproximando el infinito
Gamma_Alpha_Trapecio

Gamma_Alpha_Simpson = Int_Simpson(0,10000,gamma)
Gamma_Alpha_Simpson

Gamma_Alpha_Simpson38 = Int_Simpson38(0, 10000,gamma)
Gamma_Alpha_Simpson38


#Cargamos la funcion y
fn_Trapecio <- function(x){
  val = ((x/theeta)^alpha *exp(-x/theeta)) / ( x * Gamma_Alpha_Trapecio)
  return(val)
}


Prob_Y_Trapecio <- Int_Trapecio(a,b,fn_Trapecio)
Prob_Y_Trapecio
print("Utiliza los nodos y0,y1")


fn_Simpson <- function(x){
  val = ((x/theeta)^alpha *exp(-x/theeta)) / ( x * Gamma_Alpha_Simpson)
  return(val)
}  


Prob_Y_Simpson <- Int_Simpson(a,b,fn_Simpson)
Prob_Y_Simpson
print("Utiliza los nodos y0,y1,y2")


fn_Simpson38 <- function(x){
  val = ((x/theeta)^alpha *exp(-x/theeta)) / ( x * Gamma_Alpha_Simpson38)
  return(val)
}  


Prob_Y_Simpson38 <- Int_Simpson38(a,b,fn_Simpson38)
Prob_Y_Simpson38
print("Utiliza los nodos y0,y1,y2,y3")

#2.2 ----
simpson.compuesto = function(a, b, n,fn){
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

n=20

Gamma_Alpha_Simpson_Compuesto = simpson.compuesto(0,10000,20,gamma)
Gamma_Alpha_Simpson_Compuesto

#Cargamos la Funcion con Gamma de Alpha con Simpson Compuesto
fn_Simpson_Compuesto <- function(x){
  val = ((x/theeta)^alpha *exp(-x/theeta)) / ( x * Gamma_Alpha_Simpson_Compuesto)
  return(val)
}  

Prob_Y_Simpson_Compuesto = simpson.compuesto(a,b,n,fn_Simpson_Compuesto)
Prob_Y_Simpson_Compuesto


#COTA DEL ERROR

d1sc<-D(expression(((x/theeta)^alpha *exp(-x/theeta)) / ( x * Gamma_Alpha_Simpson_Compuesto)),"x") #cargar función que desea derivar
d2sc<-D(d1sc,"x")
d3sc<-D(d2sc,"x")
d4sc<-D(d3sc,"x")

d4sc

derivadasc4<-function(x){
  f<-((x/theeta)^((((alpha - 1) - 1) - 1) - 1) * ((((alpha - 1) - 
                                                      1) - 1) * (1/theeta)) * (((alpha - 1) - 1) * (1/theeta)) * 
        ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta)) * exp(-x/theeta) - 
        (x/theeta)^(((alpha - 1) - 1) - 1) * (((alpha - 1) - 1) * 
                                                (1/theeta)) * ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta)) * 
        (exp(-x/theeta) * (1/theeta)) - ((x/theeta)^(((alpha - 
                                                         1) - 1) - 1) * (((alpha - 1) - 1) * (1/theeta)) * ((alpha - 
                                                                                                               1) * (1/theeta)) * (alpha * (1/theeta)) * (exp(-x/theeta) * 
                                                                                                                                                            (1/theeta)) - (x/theeta)^((alpha - 1) - 1) * ((alpha - 1) * 
                                                                                                                                                                                                            (1/theeta)) * (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta) * 
                                                                                                                                                                                                                                                    (1/theeta))) - ((x/theeta)^(((alpha - 1) - 1) - 1) * (((alpha - 
                                                                                                                                                                                                                                                                                                              1) - 1) * (1/theeta)) * ((alpha - 1) * (1/theeta)) * (alpha * 
                                                                                                                                                                                                                                                                                                                                                                      (1/theeta)) * (exp(-x/theeta) * (1/theeta)) - (x/theeta)^((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                   1) - 1) * ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta)) * 
                                                                                                                                                                                                                                                                      (exp(-x/theeta) * (1/theeta) * (1/theeta)) - ((x/theeta)^((alpha - 
                                                                                                                                                                                                                                                                                                                                   1) - 1) * ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta)) * 
                                                                                                                                                                                                                                                                                                                      (exp(-x/theeta) * (1/theeta) * (1/theeta)) - (x/theeta)^(alpha - 
                                                                                                                                                                                                                                                                                                                                                                                 1) * (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta) * 
                                                                                                                                                                                                                                                                                                                                                                                                                (1/theeta) * (1/theeta)))) - ((x/theeta)^(((alpha - 1) - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                             1) - 1) * (((alpha - 1) - 1) * (1/theeta)) * ((alpha - 1) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             (1/theeta)) * (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta)) - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                (x/theeta)^((alpha - 1) - 1) * ((alpha - 1) * (1/theeta)) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (1/theeta)) - ((x/theeta)^((alpha - 1) - 1) * ((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            1) * (1/theeta)) * (alpha * (1/theeta)) * (exp(-x/theeta) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         (1/theeta) * (1/theeta)) - (x/theeta)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (1/theeta)) * (exp(-x/theeta) * (1/theeta) * (1/theeta) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (1/theeta))) - ((x/theeta)^((alpha - 1) - 1) * ((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    1) * (1/theeta)) * (alpha * (1/theeta)) * (exp(-x/theeta) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (1/theeta) * (1/theeta)) - (x/theeta)^(alpha - 1) * (alpha * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (1/theeta)) * (exp(-x/theeta) * (1/theeta) * (1/theeta) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         (1/theeta)) - ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (exp(-x/theeta) * (1/theeta) * (1/theeta) * (1/theeta)) - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (x/theeta)^alpha * (exp(-x/theeta) * (1/theeta) * (1/theeta) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (1/theeta) * (1/theeta))))))/(x * Gamma_Alpha_Simpson_Compuesto) - 
    ((x/theeta)^(((alpha - 1) - 1) - 1) * (((alpha - 1) - 1) * 
                                             (1/theeta)) * ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta)) * 
       exp(-x/theeta) - (x/theeta)^((alpha - 1) - 1) * ((alpha - 
                                                           1) * (1/theeta)) * (alpha * (1/theeta)) * (exp(-x/theeta) * 
                                                                                                        (1/theeta)) - ((x/theeta)^((alpha - 1) - 1) * ((alpha - 
                                                                                                                                                          1) * (1/theeta)) * (alpha * (1/theeta)) * (exp(-x/theeta) * 
                                                                                                                                                                                                       (1/theeta)) - (x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * 
                                                                                                                         (exp(-x/theeta) * (1/theeta) * (1/theeta))) - ((x/theeta)^((alpha - 
                                                                                                                                                                                       1) - 1) * ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta)) * 
                                                                                                                                                                          (exp(-x/theeta) * (1/theeta)) - (x/theeta)^(alpha - 1) * 
                                                                                                                                                                          (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta) * 
                                                                                                                                                                                                    (1/theeta)) - ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * 
                                                                                                                                                                                                                     (exp(-x/theeta) * (1/theeta) * (1/theeta)) - (x/theeta)^alpha * 
                                                                                                                                                                                                                     (exp(-x/theeta) * (1/theeta) * (1/theeta) * (1/theeta))))) * 
    Gamma_Alpha_Simpson_Compuesto/(x * Gamma_Alpha_Simpson_Compuesto)^2 - 
    (((x/theeta)^(((alpha - 1) - 1) - 1) * (((alpha - 1) - 1) * 
                                              (1/theeta)) * ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta)) * 
        exp(-x/theeta) - (x/theeta)^((alpha - 1) - 1) * ((alpha - 
                                                            1) * (1/theeta)) * (alpha * (1/theeta)) * (exp(-x/theeta) * 
                                                                                                         (1/theeta)) - ((x/theeta)^((alpha - 1) - 1) * ((alpha - 
                                                                                                                                                           1) * (1/theeta)) * (alpha * (1/theeta)) * (exp(-x/theeta) * 
                                                                                                                                                                                                        (1/theeta)) - (x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * 
                                                                                                                          (exp(-x/theeta) * (1/theeta) * (1/theeta))) - ((x/theeta)^((alpha - 
                                                                                                                                                                                        1) - 1) * ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta)) * 
                                                                                                                                                                           (exp(-x/theeta) * (1/theeta)) - (x/theeta)^(alpha - 1) * 
                                                                                                                                                                           (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta) * 
                                                                                                                                                                                                     (1/theeta)) - ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * 
                                                                                                                                                                                                                      (exp(-x/theeta) * (1/theeta) * (1/theeta)) - (x/theeta)^alpha * 
                                                                                                                                                                                                                      (exp(-x/theeta) * (1/theeta) * (1/theeta) * (1/theeta))))) * 
       Gamma_Alpha_Simpson_Compuesto/(x * Gamma_Alpha_Simpson_Compuesto)^2 - 
       ((x/theeta)^((alpha - 1) - 1) * ((alpha - 1) * (1/theeta)) * 
          (alpha * (1/theeta)) * exp(-x/theeta) - (x/theeta)^(alpha - 
                                                                1) * (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta)) - 
          ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * 
             (exp(-x/theeta) * (1/theeta)) - (x/theeta)^alpha * 
             (exp(-x/theeta) * (1/theeta) * (1/theeta)))) * 
       Gamma_Alpha_Simpson_Compuesto * (2 * (Gamma_Alpha_Simpson_Compuesto * 
                                               (x * Gamma_Alpha_Simpson_Compuesto)))/((x * Gamma_Alpha_Simpson_Compuesto)^2)^2) - 
    (((x/theeta)^(((alpha - 1) - 1) - 1) * (((alpha - 1) - 1) * 
                                              (1/theeta)) * ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta)) * 
        exp(-x/theeta) - (x/theeta)^((alpha - 1) - 1) * ((alpha - 
                                                            1) * (1/theeta)) * (alpha * (1/theeta)) * (exp(-x/theeta) * 
                                                                                                         (1/theeta)) - ((x/theeta)^((alpha - 1) - 1) * ((alpha - 
                                                                                                                                                           1) * (1/theeta)) * (alpha * (1/theeta)) * (exp(-x/theeta) * 
                                                                                                                                                                                                        (1/theeta)) - (x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * 
                                                                                                                          (exp(-x/theeta) * (1/theeta) * (1/theeta))) - ((x/theeta)^((alpha - 
                                                                                                                                                                                        1) - 1) * ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta)) * 
                                                                                                                                                                           (exp(-x/theeta) * (1/theeta)) - (x/theeta)^(alpha - 1) * 
                                                                                                                                                                           (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta) * 
                                                                                                                                                                                                     (1/theeta)) - ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * 
                                                                                                                                                                                                                      (exp(-x/theeta) * (1/theeta) * (1/theeta)) - (x/theeta)^alpha * 
                                                                                                                                                                                                                      (exp(-x/theeta) * (1/theeta) * (1/theeta) * (1/theeta))))) * 
       Gamma_Alpha_Simpson_Compuesto/(x * Gamma_Alpha_Simpson_Compuesto)^2 - 
       ((x/theeta)^((alpha - 1) - 1) * ((alpha - 1) * (1/theeta)) * 
          (alpha * (1/theeta)) * exp(-x/theeta) - (x/theeta)^(alpha - 
                                                                1) * (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta)) - 
          ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * 
             (exp(-x/theeta) * (1/theeta)) - (x/theeta)^alpha * 
             (exp(-x/theeta) * (1/theeta) * (1/theeta)))) * 
       Gamma_Alpha_Simpson_Compuesto * (2 * (Gamma_Alpha_Simpson_Compuesto * 
                                               (x * Gamma_Alpha_Simpson_Compuesto)))/((x * Gamma_Alpha_Simpson_Compuesto)^2)^2 - 
       ((((x/theeta)^((alpha - 1) - 1) * ((alpha - 1) * (1/theeta)) * 
            (alpha * (1/theeta)) * exp(-x/theeta) - (x/theeta)^(alpha - 
                                                                  1) * (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta)) - 
            ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * 
               (exp(-x/theeta) * (1/theeta)) - (x/theeta)^alpha * 
               (exp(-x/theeta) * (1/theeta) * (1/theeta)))) * 
           Gamma_Alpha_Simpson_Compuesto * (2 * (Gamma_Alpha_Simpson_Compuesto * 
                                                   (x * Gamma_Alpha_Simpson_Compuesto))) + ((x/theeta)^(alpha - 
                                                                                                          1) * (alpha * (1/theeta)) * exp(-x/theeta) - (x/theeta)^alpha * 
                                                                                              (exp(-x/theeta) * (1/theeta))) * Gamma_Alpha_Simpson_Compuesto * 
           (2 * (Gamma_Alpha_Simpson_Compuesto * Gamma_Alpha_Simpson_Compuesto)))/((x * 
                                                                                      Gamma_Alpha_Simpson_Compuesto)^2)^2 - ((x/theeta)^(alpha - 
                                                                                                                                           1) * (alpha * (1/theeta)) * exp(-x/theeta) - (x/theeta)^alpha * 
                                                                                                                               (exp(-x/theeta) * (1/theeta))) * Gamma_Alpha_Simpson_Compuesto * 
          (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto))) * 
          (2 * (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto)) * 
                  ((x * Gamma_Alpha_Simpson_Compuesto)^2)))/(((x * 
                                                                 Gamma_Alpha_Simpson_Compuesto)^2)^2)^2)) - (((x/theeta)^(((alpha - 
                                                                                                                              1) - 1) - 1) * (((alpha - 1) - 1) * (1/theeta)) * ((alpha - 
                                                                                                                                                                                    1) * (1/theeta)) * (alpha * (1/theeta)) * exp(-x/theeta) - 
                                                                                                                (x/theeta)^((alpha - 1) - 1) * ((alpha - 1) * (1/theeta)) * 
                                                                                                                (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta)) - 
                                                                                                                ((x/theeta)^((alpha - 1) - 1) * ((alpha - 1) * (1/theeta)) * 
                                                                                                                   (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta)) - 
                                                                                                                   (x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * (exp(-x/theeta) * 
                                                                                                                                                                      (1/theeta) * (1/theeta))) - ((x/theeta)^((alpha - 
                                                                                                                                                                                                                  1) - 1) * ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta)) * 
                                                                                                                                                                                                     (exp(-x/theeta) * (1/theeta)) - (x/theeta)^(alpha - 1) * 
                                                                                                                                                                                                     (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta) * (1/theeta)) - 
                                                                                                                                                                                                     ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * (exp(-x/theeta) * 
                                                                                                                                                                                                                                                         (1/theeta) * (1/theeta)) - (x/theeta)^alpha * (exp(-x/theeta) * 
                                                                                                                                                                                                                                                                                                          (1/theeta) * (1/theeta) * (1/theeta))))) * Gamma_Alpha_Simpson_Compuesto/(x * 
                                                                                                                                                                                                                                                                                                                                                                                      Gamma_Alpha_Simpson_Compuesto)^2 - ((x/theeta)^((alpha - 
                                                                                                                                                                                                                                                                                                                                                                                                                                         1) - 1) * ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta)) * 
                                                                                                                                                                                                                                                                                                                                                                                                                            exp(-x/theeta) - (x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * 
                                                                                                                                                                                                                                                                                                                                                                                                                            (exp(-x/theeta) * (1/theeta)) - ((x/theeta)^(alpha - 1) * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                               (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta)) - (x/theeta)^alpha * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                               (exp(-x/theeta) * (1/theeta) * (1/theeta)))) * Gamma_Alpha_Simpson_Compuesto * 
                                                                                                               (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto)))/((x * 
                                                                                                                                                                                               Gamma_Alpha_Simpson_Compuesto)^2)^2 - ((((x/theeta)^((alpha - 
                                                                                                                                                                                                                                                       1) - 1) * ((alpha - 1) * (1/theeta)) * (alpha * (1/theeta)) * 
                                                                                                                                                                                                                                          exp(-x/theeta) - (x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * 
                                                                                                                                                                                                                                          (exp(-x/theeta) * (1/theeta)) - ((x/theeta)^(alpha - 1) * 
                                                                                                                                                                                                                                                                             (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta)) - (x/theeta)^alpha * 
                                                                                                                                                                                                                                                                             (exp(-x/theeta) * (1/theeta) * (1/theeta)))) * Gamma_Alpha_Simpson_Compuesto * 
                                                                                                                                                                                                                                         (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto))) + 
                                                                                                                                                                                                                                         ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * exp(-x/theeta) - 
                                                                                                                                                                                                                                            (x/theeta)^alpha * (exp(-x/theeta) * (1/theeta))) * Gamma_Alpha_Simpson_Compuesto * 
                                                                                                                                                                                                                                         (2 * (Gamma_Alpha_Simpson_Compuesto * Gamma_Alpha_Simpson_Compuesto)))/((x * 
                                                                                                                                                                                                                                                                                                                    Gamma_Alpha_Simpson_Compuesto)^2)^2 - ((x/theeta)^(alpha - 
                                                                                                                                                                                                                                                                                                                                                                         1) * (alpha * (1/theeta)) * exp(-x/theeta) - (x/theeta)^alpha * 
                                                                                                                                                                                                                                                                                                                                                             (exp(-x/theeta) * (1/theeta))) * Gamma_Alpha_Simpson_Compuesto * 
                                                                                                                                                                                                                                        (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto))) * 
                                                                                                                                                                                                                                        (2 * (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto)) * 
                                                                                                                                                                                                                                                ((x * Gamma_Alpha_Simpson_Compuesto)^2)))/(((x * Gamma_Alpha_Simpson_Compuesto)^2)^2)^2) - 
                                                                                                               ((((x/theeta)^((alpha - 1) - 1) * ((alpha - 1) * (1/theeta)) * 
                                                                                                                    (alpha * (1/theeta)) * exp(-x/theeta) - (x/theeta)^(alpha - 
                                                                                                                                                                          1) * (alpha * (1/theeta)) * (exp(-x/theeta) * (1/theeta)) - 
                                                                                                                    ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * (exp(-x/theeta) * 
                                                                                                                                                                        (1/theeta)) - (x/theeta)^alpha * (exp(-x/theeta) * 
                                                                                                                                                                                                            (1/theeta) * (1/theeta)))) * Gamma_Alpha_Simpson_Compuesto * 
                                                                                                                   (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto))) + 
                                                                                                                   ((x/theeta)^(alpha - 1) * (alpha * (1/theeta)) * exp(-x/theeta) - 
                                                                                                                      (x/theeta)^alpha * (exp(-x/theeta) * (1/theeta))) * 
                                                                                                                   Gamma_Alpha_Simpson_Compuesto * (2 * (Gamma_Alpha_Simpson_Compuesto * 
                                                                                                                                                           Gamma_Alpha_Simpson_Compuesto)) + ((x/theeta)^(alpha - 
                                                                                                                                                                                                            1) * (alpha * (1/theeta)) * exp(-x/theeta) - (x/theeta)^alpha * 
                                                                                                                                                                                                (exp(-x/theeta) * (1/theeta))) * Gamma_Alpha_Simpson_Compuesto * 
                                                                                                                   (2 * (Gamma_Alpha_Simpson_Compuesto * Gamma_Alpha_Simpson_Compuesto)))/((x * 
                                                                                                                                                                                              Gamma_Alpha_Simpson_Compuesto)^2)^2 - (((x/theeta)^(alpha - 
                                                                                                                                                                                                                                                    1) * (alpha * (1/theeta)) * exp(-x/theeta) - (x/theeta)^alpha * 
                                                                                                                                                                                                                                        (exp(-x/theeta) * (1/theeta))) * Gamma_Alpha_Simpson_Compuesto * 
                                                                                                                                                                                                                                       (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto))) + 
                                                                                                                                                                                                                                       ((x/theeta)^alpha * exp(-x/theeta)) * Gamma_Alpha_Simpson_Compuesto * 
                                                                                                                                                                                                                                       (2 * (Gamma_Alpha_Simpson_Compuesto * Gamma_Alpha_Simpson_Compuesto))) * 
                                                                                                                  (2 * (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto)) * 
                                                                                                                          ((x * Gamma_Alpha_Simpson_Compuesto)^2)))/(((x * 
                                                                                                                                                                         Gamma_Alpha_Simpson_Compuesto)^2)^2)^2 - (((((x/theeta)^(alpha - 
                                                                                                                                                                                                                                    1) * (alpha * (1/theeta)) * exp(-x/theeta) - (x/theeta)^alpha * 
                                                                                                                                                                                                                        (exp(-x/theeta) * (1/theeta))) * Gamma_Alpha_Simpson_Compuesto * 
                                                                                                                                                                                                                       (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto))) + 
                                                                                                                                                                                                                       ((x/theeta)^alpha * exp(-x/theeta)) * Gamma_Alpha_Simpson_Compuesto * 
                                                                                                                                                                                                                       (2 * (Gamma_Alpha_Simpson_Compuesto * Gamma_Alpha_Simpson_Compuesto))) * 
                                                                                                                                                                                                                      (2 * (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto)) * 
                                                                                                                                                                                                                              ((x * Gamma_Alpha_Simpson_Compuesto)^2))) + ((x/theeta)^alpha * 
                                                                                                                                                                                                                                                                             exp(-x/theeta)) * Gamma_Alpha_Simpson_Compuesto * (2 * 
                                                                                                                                                                                                                                                                                                                                  (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto))) * 
                                                                                                                                                                                                                      (2 * (2 * (Gamma_Alpha_Simpson_Compuesto * Gamma_Alpha_Simpson_Compuesto) * 
                                                                                                                                                                                                                              ((x * Gamma_Alpha_Simpson_Compuesto)^2) + 2 * (Gamma_Alpha_Simpson_Compuesto * 
                                                                                                                                                                                                                                                                               (x * Gamma_Alpha_Simpson_Compuesto)) * (2 * (Gamma_Alpha_Simpson_Compuesto * 
                                                                                                                                                                                                                                                                                                                              (x * Gamma_Alpha_Simpson_Compuesto))))))/(((x * Gamma_Alpha_Simpson_Compuesto)^2)^2)^2 - 
                                                                                                                                                                                                                     ((x/theeta)^alpha * exp(-x/theeta)) * Gamma_Alpha_Simpson_Compuesto * 
                                                                                                                                                                                                                     (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto))) * 
                                                                                                                                                                                                                     (2 * (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto)) * 
                                                                                                                                                                                                                             ((x * Gamma_Alpha_Simpson_Compuesto)^2))) * (2 * 
                                                                                                                                                                                                                                                                            (2 * (2 * (Gamma_Alpha_Simpson_Compuesto * (x * Gamma_Alpha_Simpson_Compuesto)) * 
                                                                                                                                                                                                                                                                                    ((x * Gamma_Alpha_Simpson_Compuesto)^2)) * (((x * 
                                                                                                                                                                                                                                                                                                                                    Gamma_Alpha_Simpson_Compuesto)^2)^2)))/((((x * 
                                                                                                                                                                                                                                                                                                                                                                                 Gamma_Alpha_Simpson_Compuesto)^2)^2)^2)^2)))
  return(f)
}

optimosc<-optimize(derivadasc4,c(a,b),maximum = T)
optsc<-abs(optimosc$objective)
optsc

n<-20
error<-((b-a)/180)*(((b-a)/n)^4)*optsc
error


#2.3 ----
trapecio.compuesto = function(a, b, n,fn){
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

n=1500

Gamma_Alpha_Trapecio_Compuesto = trapecio.compuesto(0,10000,n,gamma)
Gamma_Alpha_Trapecio_Compuesto

#Cargamos la Funcion de la esperanza de Y con Gamma de Alpha con Trapecio Compuesto
fn_Trapecio_Compuesto <- function(x){
  val = x* (((x/theeta)^alpha *exp(-x/theeta)) / ( x * Gamma_Alpha_Trapecio_Compuesto))
  return(val)
}  

E_Y <- trapecio.compuesto(0,10000,n,fn_Trapecio_Compuesto)
E_Y



#### EJERCICIO 3 ####

r = seq(0,0.15,0.01)

P = c(115,109.7069,104.7135,100,95.5482,91.3410,87.3629,83.5992,80.0364,76.6621,73.4645,70.4328,67.5570,64.8277,62.2391,59.7741)


derivada5ptos_centrada <-function(x,y,xindice){
  n = length(y)
  yprima = c(rep(NA,n))
  h = diff(x)[1]
  yprima[xindice] = (y[xindice-2] - 8*y[xindice-1] + 8*y[xindice+1] - y[xindice+2])/(12*h)
  return(yprima[xindice])
}


r0 <- 0.1154564

P_prima_ro <- derivada5ptos_centrada(x,P,r0)
P_prima_ro

#DERIVADA SEGUNDA

derivada_segunda = function(x, y, xindice){
  h = diff(x)[1]
  n = length(y)
  yprima = c(rep(NA,n))
  
  if(missing(xindice)){
    for (i in 2:(n - 1)) {
      yprima[i] = (y[i-1] - 2*y[i] + y[i+1])/(h^2)
    }
    
    tabla = cbind.data.frame(x, y, yprima)
    return(tabla)
  }
  else{
    if(xindice == 1 || xindice == n){
      return("Este valor no se puede calcular")
    }
    else{
      yprima[xindice] = (y[xindice-1] - 2*y[xindice] + y[xindice+1])/(h^2)
      return(yprima[xindice])
    }
  }
}

P_Prima2_r0 = derivada_segunda(x,P,r0)
P_Prima2_r0


#### EJERCICIO 5 ####

#5.1----
#La interpolacion mediante trazadores cubicos es la APROXIMACION POLINOMICA FRAGMENTARIA mas comun
#a una funcion. Consiste en dividir el intervalo en una serie de subintervalos y en cada uno construir
#un polinomio de grado 3 entre cada par consecutivos de nodos.
#Los TRAZADORES NATURALES se construyen de tal manera que haya continuidad, manteniendo igual pendiente y 
#Curvatura con los polinomios de intervalos adyacentes. Su grafica se aproxima a la forma que adoptara una variable
# LARGA y FLEXIBLE si la hicieramos pasar por los puntos {(x0,f(x0)) , (x1,f(x1)), ... , (xn,f(xn))}

#Por otro lado con los TRAZADORES SUJETOS se logran aproximaciones mas exactas ya que abarcan mas info 
#acerca de la funcion. Pero se requiere tener los calores de la derivada en los extremos o bien tener una aproximacion
#precisa de ellas.

#5.2 LAGRANGE ----
L <- c(9.4812,9.5244,10.0952,11.1160,12.8725,13.4485,14.1008,14.4795,15.2031,19.6600)

PA <- seq(0.1,1,0.1)

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

P_L14 <- Lagrange(14,L,PA)
P_L14
#En este caso parrece que aproxima bastante bien la probabilidad ya que se encuentra entre 0.6 y 0.7

P_L18 <- Lagrange(18,L,PA)
P_L18
## El polinomio de Lagrange aproxima mal los valores extremos, por eso no es de extrañar el resultado.
# Esto se debe a que aproxima con un polinomio de grado n-1, en este caso un polinomio de grado 9.



#### EJERCICIO 6 SIMULACION ####
P0<-75
mu<-0.15  #retorno esperado
sigma<-0.20  #volatilidad
T<-0.5  #seis meses (año seria 1)
dt<-1/252  #Saltos diarios
n<-T/dt  #número de time-steps


m<- 1000 #cantidad de caminos de precios (cantidad de simulaciones)
Pt<- matrix(NA,nrow=m,ncol=n+1) #matriz de camino de precios con m filas y n+1 precio (porque arranca de 0)
Pt[,1]<- P0 #para todas las filas, que en la primer columna tenga P0

for (i in 1:m) { 
  for (t in 2:(n+1)) {
    #el 2 porque arrance de la fila 2 porque 1 era P0 (precio inicial)
    Pt[i,t]<-Pt[i,t-1]*exp((mu-0.5*sigma^2)*dt+sigma*sqrt(dt)*rnorm(1))
  } 
}

View(Pt)

#Histograma de los precios Finales
hist(Pt[,ncol(Pt)],xlab = "Pt", main = "Histograma de los precios finales")

#2.2
#Probabilidad de que el precio final Pt este entre 65 y 75

df_pt <- data.frame(Pt)
ncol(Pt) #127

(length(df_pt[df_pt$X127 > 65, "X127" ]) / length(Pt[,ncol(Pt)])) - (length(df_pt[df_pt$X127 < 75, "X127" ]) / length(Pt[,ncol(Pt)]))

#Probabiliad de que el precio final Pt sea menor al Precio esperado en t (E(Pt))

E_Pt <- mean(Pt[,ncol(Pt)])
E_Pt

length(df_pt[df_pt$X127 < E_Pt, "X127" ]) / length(Pt[,ncol(Pt)])

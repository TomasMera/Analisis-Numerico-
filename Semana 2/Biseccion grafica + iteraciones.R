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
  return (paste("El metodo falló luego de ", N, "iteraciones"))
}


polinomio = function (x){
  f = x^3+x-4
  return (f)
}

x <- 0:3
y <- polinomio(x)
graf_func1 <- data.frame(x,y)
ggplot(graf_func1, aes(x,y))+
  geom_line(colour="red") + 
  geom_point( size=2, shape=21, fill="white", colour="red") + 
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  theme_minimal()



#iteracion 1
 Met_Biseccion(1,4,10^-3,100)#1.378796756


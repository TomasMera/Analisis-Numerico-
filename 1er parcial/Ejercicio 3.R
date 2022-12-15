#Ejercicio 3

#3.1 ----
##  Algoritmo de Elim Gaussiana 
Elim_Gauss = function(A,b){ # siendo A la matriz de incognitas y b el vector de resultados
  n = nrow(A)
  Aa = cbind(A,b) # Matriz ampliada
  
  #Paso 1
  for(i in 1:(n-1)){
    
    #Paso 2 Buscar el minimo p tal que A[p,i] != 0, y lo llama pp `
    
    pp = 0
    for (p in i:n){
      if (Aa[p,i] != 0){ #si Aa[p,i] no es igual a cero
        pp = p          #Asigno a pp el valor de la fila analizada
        break       #Salgo del bucle for p in i:n
      }
    }
    if (pp == 0)   {     #Si el bucle anterior nunca encontró resultado
      return (paste("No hay solución única"))
      break
    }
    
    #Paso 3      si pp != i entonces E(p) = E(i)
    
    if (pp != i) { 
      aux = Aa[pp,]
      Aa[pp,] = Aa[i,]
      Aa[i,] = aux
    }
    #Paso 4
    
    for (j in (i+1):n){ 
      
      #Paso 5
      
      m = Aa[j,i]/Aa[i,i]
      
      #Paso 6
      
      Aa[j,] = Aa[j,] - (m*Aa[i,])    
    }
    
    #Paso 7 si a_nn=0 exit
    
    if (Aa[n,n] == 0) { 
      return(paste("no existe solucion unica"))
      break 
    }
  }
  
  #Paso 8 #Inicia la sustitucion hacia atras
  
  x = rep(NA, times = n) 
  x[n] = Aa[n,(n+1)]/Aa[n,n]
  
  #Paso 9
  
  for (i in (n-1):1) { 
    
    sumaux = 0 #Creo la variable auxiliar para hacer la sumatoria
    for (j in (i+1):n) {      #Calculo la sumatoria
      sumaux = sumaux + Aa[i,j]*x[j]
    }
    x[i] = (Aa[i,(n+1)] - sumaux)/Aa[i,i]
  }
  #Paso 10
  return (x)
}

# Defino Matrices 
MatrixA = matrix(c(2,0,0,0,
                   1,1.5,0,0,
                   0,-3,0.5,0,
                   2,-2,1,1), nrow = 4, ncol = 4, byrow = T)
VectorB = matrix(c(1,4.5,-6.6,0.8), nrow = 4, ncol = 1)

# Resultado
X<-Elim_Gauss(MatrixA,VectorB)
 
#Para corroborar
MatrixA%*%X

MatrixA%*%X == VectorB

##3.2----
FactorizacionLU_6.4 <- function(A){
  i <- 1
  n <- nrow(A)
  L<-matrix(data=0,nrow=nrow(A),ncol=ncol(A),byrow=TRUE)
  U<-matrix(data=0,nrow=nrow(A),ncol=ncol(A),byrow=TRUE)
  for (i in i:n){
    L[i,i]=1
  }
  
  #Paso 1
  if (A[1,1]==0){
    return('Factorizacion imposible')
    break} else {
      U[1,1]<-A[1,1]/L[1,1]
    }
  
  #Paso 2
  j <- 2
  for (j in j:n){
    U[1,j]=A[1,j]/L[1,1] #Primer renglon de U
    L[j,1]=A[j,1]/U[1,1] #Primera columna de L
  }
  
  #Paso 3
  i<-2
  m<-(n-1)
  while (i<=m){
    #Paso 4
    aux <- 0
    k <- 1
    kk<-(i-1)
    for(k in k:kk){
      aux <- aux+L[i,k]*U[k,i]
      U[i,i]=A[i,i]-aux
    }
    if (U[i,i]==0){
      return('Factorizacion Imposible')
      break
    }
    
    #Paso 5
    jj<-(1+i)
    for (j in jj:n){
      aux2 <- 0
      k <- 1
      for(k in k:(i-1)){
        aux2 <- aux2+L[i,k]*U[k,j]
      }
      aux3 <- 0
      k <- 1
      for(k in k:(i-1)){
        aux3 <- aux3+L[j,k]*U[k,i]
      }
      U[i,j]=(1/L[i,i])*(A[i,j]-aux2)
      L[j,i]=(1/U[i,i])*(A[j,i]-aux3)
    }
    i<-i+1
  }
  #Paso 6
  aux4 <- 0
  k <- 1
  nn<- (n-1)
  for(k in k:nn){
    aux4 <- aux4+L[n,k]*U[k,n]
  }
  U[n,n]=A[n,n]-aux4
  
  #Paso 7
  return(list("L"=L, "U"=U))
}



FactorizacionLU_6.4(MatrixA)

#comprobacion
L<-FactorizacionLU_6.4(MatrixA)$L
U<-FactorizacionLU_6.4(MatrixA)$U
L%*%U

L%*%U == MatrixA
#Se verifica que son iguales.
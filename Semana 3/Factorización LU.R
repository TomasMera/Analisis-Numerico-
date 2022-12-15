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


#Ejemplo
A <- matrix(data=c(1,1,0,3,
                   2,1,-1,1,
                   3,-1,-1,2,
                   -1,2,3,-1),
            nrow=4, ncol=4, byrow=TRUE)
b <- matrix(data=c(1,1,-3,4),nrow=4, ncol=1)

FactorizacionLU_6.4(A)

#comprobacion
L<-FactorizacionLU_6.4(A)$L
U<-FactorizacionLU_6.4(A)$U
L%*%U
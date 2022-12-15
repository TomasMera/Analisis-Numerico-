## Eliminacion Gaussiana con sustitucion hacia atras ----
## 1) Algoritmo de Elim Gaussiana ----
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

#2) Defino Matrices ----
MatrixA = matrix(c(1,-1,2,-1,
                   2,-2,3,-3,
                   1,1,1,0,
                   1,-1,4,3), nrow = 4, ncol = 4, byrow = T)
VectorB = matrix(c(-8,-20,-2,4), nrow = 4, ncol = 1)

#3) Resultado ----
X<-Elim_Gauss(MatrixA,VectorB)
#4) Corroboración Resultado ----
#Para corroborar
MatrixA%*%X

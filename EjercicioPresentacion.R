RaicesCuadraticas <- function (a,b,c){
  x1 = (-b +(sqrt(b^2 - (4*a*c)))/(2*a)
        
  x2 = (-b -(sqrt(b^2 - (4*a*c)))/(2*a)
  
  print(paste("la raiz x1 es: ", x1))
  print(paste("la raiz x2 es: ", x2))
}

factorial <- function(a){
  factorial = 1
  for (i in 1:a){
    factorial = factorial*i
  }
  print (paste(sprintf("El factorial de %i es:",a),factorial))
}
factorial(4)

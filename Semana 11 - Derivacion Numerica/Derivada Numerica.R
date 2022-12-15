#Derivada Numerica
rm (list = ls())
dev.off () #Borra graficos

#Creo set de puntos----
Tabla = data.frame( x = seq(from =0, to = 3*pi, by = pi/4),
                    fx = sin(seq(from=0, to= 3*pi, by= pi/4)))

#Funcion sen(x) ----
plot(Tabla, type="p")
title("f(x) = seno(x)")
abline(h=0)
curve(sin,min(Tabla$x), max(Tabla$x), lwd = 2, col ="grey", add = T) #graficamos la funcion

#Funcion de la Derivada f'(x) = cos(x)----
curve(cos,min(Tabla$x), max(Tabla$x), lwd = 2, col= "magenta") #Grafico de la derivada
title ("f'(x) = coseno(x)")
abline(h=0)
f1= cos(Tabla$x) #valores de la derivada de seno evaluada en el vector ingresado.
points(Tabla$x, f1, col="black")

#Aproximamos la Derivada ----
h <- 0.5
x = Tabla$x
f1.aprox_0.5 = (sin(x+h) -sin(x))/h
points(x,f1.aprox_0.5, col= "red")

h = 0.1 #aproximo la derivada con h=0.1 y grafico
f1.aprox_0.1 = (sin(x+h) -sin(x))/h
points(x, f1.aprox_0.1, col = "blue")

h = 0.01 #aproximo la derivada con h=0.01 y grafico
f1.aprox_0.01 = (sin(x+h) -sin(x))/h
points(x, f1.aprox_0.01, col = "green")


#Aproximo la Derivada en Base a los Datos
h = diff(Tabla$x) #diff calcula la diferencia de los valores de la tabla
delta_f =diff(Tabla$fx) #aplico diff a la col f de la tabla
f1.DATOS = c(delta_f/h, NA)
points(x, f1.DATOS, col= "black", pch=11, cex=1.2)

legend("topright", legend = c("Exacta", "h=0.5", "h=0.1", "h=0.01", "Datos"),
       pch = c(1,1,1,1,11), col = c("black","red", "blue","green","black"))

Resultados = data.frame(x = Tabla$x, fx= Tabla$f,
                        f1,f1.aprox_0.5, f1.aprox_0.1, f1.aprox_0.01, f1.DATOS)
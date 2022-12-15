f = function(x){
  cos(x)-sqrt(x)
}
x <- seq(0,10,by = 0.1) #Generamos un vector "x" para crear los puntos en F(x)
fx <- f(x) #Creamos los valores de f(x)
df <- data.frame(x, fx) # Creo data frame
gg_fx = ggplot(data = df) + #Cargo los datos
  aes(x=x,y=fx) + #Agrego capa estética (culumnas de "df")
  geom_line(linetype=1,colour="darkblue") + #Agrego la geometria
  geom_hline(yintercept = 0,linetype=1) + #Agrego linea que cruza en y=0
  geom_vline(xintercept = 0,linetype=1) + #Agrego linea que cruza en x=0
  scale_x_continuous(name = "x", breaks = seq(0,10, by = 1)) + # Cambio ticks en eje X
  scale_y_continuous(name = "y = f(x)", breaks = seq(-4.5,1, by = 0.5)) + # Cambio ticks en eje Y
  geom_vline(xintercept = c(0.5,1),linetype=2, col = "red") + # Intervalo de las raíces
  ggtitle("Función con raíz entre 0.5 y 1") #
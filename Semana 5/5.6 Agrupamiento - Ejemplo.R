#Ejemplo 4: Machile learning sobre datos de acciones
#Clustering/Agrupamiento de acciones en riesgosas/no riesgosas 
library(ggplot2)
library(xlsx)
library(caret)
library(quantmod)

#quantmod: Quantitative Financial Modelling & Trading Framework 

#Obtengo datos de una accion
getSymbols('AMZN', from = '2020-01-01', to = '2021-01-01',warnings = FALSE,
           auto.assign = TRUE)

#Grafico
chartSeries(AMZN, type="line")
##Bollinger Bands are a way to compare a security's volatility and price levels over a period of time.
#addBBands()

#Calculo de retornos de distinta periodicidad
AMZNDailyReturn= dailyReturn(AMZN)
AMZNMonthlyReturn= monthlyReturn(AMZN)
AMZNDailyReturn
AMZNMonthlyReturn

#Calculo de la volatilidad
m=length(AMZN$AMZN.Close)
AMZNVolatility <-volatility(AMZN,n=m)
AMZNVolatility[[m]]

#Carga de los simbolos de las acciones 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('acciones.R')

#Recorro los simbolos, obtengo datos de yahoo finance y calculo retorno y volatilidad
#Guardo datos en dataset
i<-0
for(valor in simbolos) {
  i<-i+1
  print(i)
  simbolo <- getSymbols(valor, from = "2021-01-01", to = "2021-07-01", 
                        auto.assign = FALSE, src = 'yahoo')
  simbolo$Retorno <- dailyReturn(simbolo)
  
  #cantidad de dias del precio de cierre para calcular volatilidad
  m=length(simbolo[4])
  Volatilidad <-volatility(simbolo,n=m)
  Volatilidad <-Volatilidad[[m]]
  
  if (i==1) { 
    datasetAcciones <- data.frame(lapply(simbolo[, 5:7], mean, na.rm = TRUE)) 
    datasetAcciones <-cbind(datasetAcciones, Volatilidad)
    colnames(datasetAcciones)<- c("Volumen","Precio","Retorno", "Volatilidad")
  } else {
    colnames(simbolo)[5]  <- "Volumen"
    colnames(simbolo)[6]  <- "Precio"
    datasetAcciones <-rbind(datasetAcciones,c(lapply(simbolo[, 5:7], mean, na.rm = TRUE), Volatilidad = Volatilidad))
  }
  row.names(datasetAcciones)[i]<-valor
}
closeAllConnections()


#guardo los datos
write.xlsx(datasetAcciones, "C:/dataset_acciones.xlsx", row.names=TRUE)
datasetAcciones <- read.xlsx("C:/dataset_acciones.xlsx",  sheetIndex = 1, row.names=TRUE)

#Con el csv falla porque toma el nombre de las filas como otra columna
#para probarlo tendria que modificar cosas

#MODELO K-MEANS
#Clusteting a partir de datos de acciones: retorno diario medio y volatilidad 
#2 clusters
kmeansStocks<-kmeans(subset(datasetAcciones, select = -c(Precio, Volumen)), centers=2)
#print(kmeansStocks)
kmeansStocks$centers


#Guardo el dato del cluster para cada accion
datasetAcciones$cluster <- as.factor(kmeansStocks$cluster)
table(datasetAcciones$cluster)
#Grafico retorno y volatilidad, color por cluster
qplot(Retorno, Volatilidad,  colour=cluster, data=datasetAcciones)

#Si tengo un dato nuevo y quiero ver en que cluster cae, tengo que entrenar el modelo
#Modelo Regresion logistica
modFit<-train(cluster~Retorno+Volatilidad, data=datasetAcciones, method="glm", family="binomial")
modFit

#Predicciones
#amazon -7.677236e-04 0.2538100
newdata1<-data.frame(Retorno=0.003590732, Volatilidad=1.1281870)
#apple -1.240856e-03 0.3998589
newdata2<-data.frame(Retorno=0.001363948 , Volatilidad=0.3312324)
#una con mas mas volatilidad
newdata3<-data.frame(Retorno=0.10, Volatilidad=0.95)
predict(modFit, newdata1)
predict(modFit, newdata2)
predict(modFit, newdata3)

#Modelo Arbol de decision
modFit<-train(cluster~Retorno+Volatilidad, data=datasetAcciones, method="rpart")
modFit

#Predicciones
#amazon -7.677236e-04 0.2538100
newdata1<-data.frame(Retorno=-7.677236e-04, Volatilidad=0.2538100)
#apple -1.240856e-03 0.3998589
newdata2<-data.frame(Retorno=-1.240856e-03, Volatilidad=0.3998589)
#una con mas mas volatilidad
newdata3<-data.frame(Retorno=0.04, Volatilidad=0.95)
predict(modFit, newdata1)
predict(modFit, newdata2)
predict(modFit, newdata3)




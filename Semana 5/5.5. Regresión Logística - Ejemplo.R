#Ejemplo 3: Machile learning sobre datos de otorgamiento de creditos
# Clasificación de creditos: creditos buenos/malos

library(caret)
library(Hmisc)
library(ggplot2)


#caret (Classification And REgression Training) es un conjunto de funciones que intentan agilizar el proceso de creación de modelos predictivos.
#El paquete contiene herramientas para: división de datos, preprocesamiento, entrenamiento del modelo, predicciones

#Hmisc: para cortar una variable numérica en intervalos

#data: german_credit
#https://archive.ics.uci.edu/ml/datasets/Statlog+%28German+Credit+Data%29

#datos
german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

#pongo los nombres de las columnas
colnames(german_credit) = c("chk_acct", "duration", "credit_his", "purpose", 
                            "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                            "job", "n_people", "telephone", "foreign", "response")

#reemplazo la respuesta 1 y 2 por palabras que identifiquen las clases (1 = Bueno, 2 = Malo)
german_credit$response  <- replace(as.character(german_credit$response), german_credit$response == "2", "Malo")
german_credit$response  <- replace(as.character(german_credit$response), german_credit$response == "1", "Bueno")


#Analisis preliminar de datos
#Cantidad de creditos por clase
table(german_credit$response)

#Grafico Credit amount y Duration in month, color por clase
qplot(amount, duration, color=response, data=german_credit)
qplot(age, amount, color=response, data=german_credit)

#Proporciones de variables por cada clase
#foreign worker: A201 : yes, A202 : no
tabla <-table(german_credit$foreign, german_credit$response)
prop.table(tabla, 1)

#prop.table(table(german_credit$foreign, german_credit$response),1)

#Housing: A151 : rent, A152 : own, A153 : for free
tabla <-table(german_credit$housing, german_credit$response)
prop.table(tabla, 1)

#prop.table(table(german_credit$housing, german_credit$response),1)

#Job: A171 : unemployed/ unskilled - non-resident
#A172 : unskilled - resident, A173 : skilled employee / official
#A174 : management/ self-employed/  highly qualified employee/ officer
tabla <-table(german_credit$job, german_credit$response)
prop.table(tabla, 1)

# o lo que es lo mismo:
prop.table(table(german_credit$job, german_credit$response),1)

#Intervalos para las edades
cutAge<-cut2(german_credit$age, g=5)
tabla <-table(cutAge, german_credit$response)
prop.table(tabla, 1)

#Intervalos para la duracion de los creditos
cutDuration<-cut2(german_credit$duration, g=5)
tabla <-table(cutDuration, german_credit$response)
prop.table(tabla, 1)

#Separo datos en entrenamiento y testeo
#CreateDataPartition para separar los datos en training (80%) y test (20%) de forma aleatoria, basado en la respuesta
inTrain <- createDataPartition(german_credit$response, p=0.8, list=FALSE)
training <- german_credit[ inTrain, ]
testing <- german_credit[ -inTrain, ]

#Modelo con todos los predictores
#Train: que voy a predecir (response) a partir de que variables (en este caso todas), 
#los datos (el set de entrenamiento) y el metodo (method="glm", family="binomial" -> Regresion logistica).
#Dejo los valores default para el resto de los parámetros

mod_fit <- train(response ~ .,  data=training, method="glm", family="binomial")
mod_fit

#importancia de predictores
varImp(mod_fit)

#Predicciones con datos de testeo
pred <- predict(mod_fit, newdata=testing)
table(pred)

#Predicciones con datos de testeo con probabilidades
pred_prob = predict(mod_fit, newdata=testing, type="prob")
#pred_prob

#Comparacion de predicciones con las clases reales
tabla <-table(pred,testing$response)
tabla
prop.table(tabla, 1)

#matriz de confusion
confusionMatrix(table(pred, testing$response)) 

#Modelo con menos variables
mod_fit <-  train(response ~ chk_acct + duration + property+
                            credit_his + amount + age + job+
                            saving_acct  + purpose + sex +
                            other_install + installment_rate, 
                          data=training, method="glm", family="binomial")
mod_fit

#Predicciones con datos de testeo
pred = predict(mod_fit, newdata=testing)
table(pred)

#Comparo predicciones con las clases reales
tabla <-table(pred,testing$response)
tabla
prop.table(tabla, 1)


#matriz de confusion
confusionMatrix(table(pred, testing$response)) 
###################### LECTOR DE ARCHIVO ####################
ww <- ("C:/Users/V Tibs/Desktop/datos/Datos del Prestamo/Prestamo_Indiv.csv")
Prestamos<- read.csv(ww, header = TRUE)
#View(Prestamos)
#Prest <- as.factor(Prestamos)
############ ENTRENAMIENTO DE RED  Y TESTEO DE #############
##################TEOREMA DE BAYES #########################
library(e1071)
# Selección de una submuestra el 70% de los datos
set.seed(200)
ABA.indices <- sample(1:nrow(Prestamos), nrow(Prestamos)* 0.7)
ABA.entrenamiento <- Prestamos[ABA.indices,]
ABA.test <- Prestamos[-ABA.indices,]
model <- naiveBayes(Status_Prestamo ~ ., data = ABA.entrenamiento)

####################### PREDICCION ###############################
results <- predict(object = model, newdata = ABA.test, type = "raw")

results2 <- predict(object = model, newdata = ABA.test, type = "class")
mc <- table(results2,ABA.test$Status_Prestamo)
mc
Porcentaje <- round(results*100)
# Correctamente clasificados

x <- 100 * sum(diag(mc)) / sum(mc)
x
#Agregar Columna de Prediccion a la tabla ABA.test
ABA.test$Pronostico <- Porcentaje
write.csv(ABA.test, "PrestamosP.csv")
#ABA.test$PClas <- results2
#write.csv(ABA.test, "Clasif_Canc2.csv")

########### PRUEBA DE ARCHIVO RECIBIDO csv #####################
#Prueba <- ("C:\\Users\\V Tibs\\Desktop\\Prueba.csv")
#PruebaR <- read.csv(Prueba, header = TRUE)
#PruebaP <- predict(object= model, newdata = PruebaR, type =  "raw")
#prue <- PruebaP * 100
############## JSON ###########################################

#library(rjson)
#data = fromJSON(file ="Pruebajson.json")
#Pruebajson <- as.data.frame(data)
#print(Pruebajson)
#Prueba2<- predict(object = model, newdata = Pruebajson, type = "raw")
#PorcentajeP <- Prueba2 * 100
################VECTOR#######################################





